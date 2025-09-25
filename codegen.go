package main

import (
	"fmt"
	"strings"
)

type Emitter struct {
	sb           strings.Builder
	labelCounter int

	// Lexical environment: stack of scopes. Each symbol maps to its rbp-relative offset in bytes.
	scopes []map[string]int

	// Bytes of locals allocated in this frame via sub rsp, 8.
	locals int
}

func (e *Emitter) String() string { return e.sb.String() }
func (e *Emitter) line(s string)  { e.sb.WriteString(s); e.sb.WriteByte('\n') }

func (e *Emitter) newLabel(prefix string) string {
	e.labelCounter++
	return fmt.Sprintf(".L%s_%d", prefix, e.labelCounter)
}

// env helpers
func (e *Emitter) envPush() { e.scopes = append(e.scopes, map[string]int{}) }
func (e *Emitter) envPop()  { e.scopes = e.scopes[:len(e.scopes)-1] }

func (e *Emitter) envBind(name string, offset int) {
	if len(e.scopes) == 0 {
		e.envPush()
	}
	e.scopes[len(e.scopes)-1][name] = offset
}

func (e *Emitter) envLookup(name string) (int, bool) {
	for i := len(e.scopes) - 1; i >= 0; i-- {
		if off, ok := e.scopes[i][name]; ok {
			return off, true
		}
	}
	return 0, false
}

// codegen entry
func (e *Emitter) Gen(n Node) error {
	switch t := n.(type) {
	case *Num:
		e.line(fmt.Sprintf("  mov rax, %d", t.Val))
		return nil

	case *Symbol:
		off, ok := e.envLookup(t.Name)
		if !ok {
			return fmt.Errorf("unbound variable %q", t.Name)
		}
		e.line(fmt.Sprintf("  mov rax, [rbp-%d]", off))
		return nil

	case *List:
		if len(t.Items) == 0 {
			e.line("  mov rax, 0")
			return nil
		}
		head, ok := t.Items[0].(*Symbol)
		if !ok {
			return fmt.Errorf("operator must be a symbol at list head")
		}
		op := strings.ToLower(head.Name)
		args := t.Items[1:]

		switch op {
		// arithmetic
		case "+", "-", "*", "/":
			return e.genArith(op, args)

		// comparisons -> 0/1
		case "=", "<", ">", "<=", ">=", "!=", "/=":
			return e.genComparison(op, args)

		// logical
		case "not":
			return e.genNot(args)
		case "and":
			return e.genAnd(args)
		case "or":
			return e.genOr(args)

		// control flow
		case "if":
			return e.genIf(args)

		// sequencing
		case "begin", "progn":
			return e.genBegin(args)

		// bindings (sequential)
		case "let*":
			return e.genLetStar(args)

		default:
			return fmt.Errorf("unsupported operator %q", head.Name)
		}

	default:
		return fmt.Errorf("unknown node type %T", n)
	}
}

// Arithmetic
func (e *Emitter) genArith(op string, args []Node) error {
	switch op {
	case "+":
		if len(args) == 0 {
			e.line("  mov rax, 0")
			return nil
		}
	case "*":
		if len(args) == 0 {
			e.line("  mov rax, 1")
			return nil
		}
	case "-":
		// Unary minus: (- x) => -x
		if len(args) == 1 {
			if err := e.Gen(args[0]); err != nil {
				return err
			}
			e.line("  neg rax")
			return nil
		}
	case "/":
		if len(args) < 2 {
			return fmt.Errorf("division expects at least 2 args")
		}
	}

	// Evaluate first argument into RAX
	if err := e.Gen(args[0]); err != nil {
		return err
	}

	// Combine remaining arguments
	for i := 1; i < len(args); i++ {
		e.line("  push rax")
		if err := e.Gen(args[i]); err != nil {
			return err
		}
		e.line("  mov rbx, rax") // current operand
		e.line("  pop rax")      // accumulator

		switch op {
		case "+":
			e.line("  add rax, rbx")
		case "-":
			e.line("  sub rax, rbx")
		case "*":
			e.line("  imul rax, rbx")
		case "/":
			e.line("  cqo")
			e.line("  idiv rbx")
		default:
			return fmt.Errorf("internal: unsupported op %q", op)
		}
	}
	return nil
}

// Comparisons: (=, <, >, <=, >=, !=, /=) -> return 0/1 in RAX
func (e *Emitter) genComparison(op string, args []Node) error {
	if len(args) != 2 {
		return fmt.Errorf("comparison %s expects exactly 2 args", op)
	}
	if err := e.Gen(args[0]); err != nil {
		return err
	}
	e.line("  push rax")
	if err := e.Gen(args[1]); err != nil {
		return err
	}
	e.line("  mov rbx, rax") // right operand
	e.line("  pop rax")      // left operand
	e.line("  cmp rax, rbx")
	switch op {
	case "=":
		e.line("  sete al")
	case "!=":
		e.line("  setne al")
	case "/=":
		e.line("  setne al")
	case "<":
		e.line("  setl al")
	case ">":
		e.line("  setg al")
	case "<=":
		e.line("  setle al")
	case ">=":
		e.line("  setge al")
	default:
		return fmt.Errorf("internal: bad cmp op %q", op)
	}
	e.line("  movzx rax, al") // zero-extend to rax
	return nil
}

// Logical: not, and, or
func (e *Emitter) genNot(args []Node) error {
	if len(args) != 1 {
		return fmt.Errorf("not expects 1 arg")
	}
	if err := e.Gen(args[0]); err != nil {
		return err
	}
	e.line("  cmp rax, 0")
	e.line("  sete al")
	e.line("  movzx rax, al")
	return nil
}

func (e *Emitter) genAnd(args []Node) error {
	if len(args) == 0 {
		e.line("  mov rax, 1") // true
		return nil
	}
	fail := e.newLabel("and_fail")
	end := e.newLabel("and_end")
	for _, arg := range args {
		if err := e.Gen(arg); err != nil {
			return err
		}
		e.line("  cmp rax, 0")
		e.line("  je " + fail)
	}
	e.line("  mov rax, 1")
	e.line("  jmp " + end)
	e.line(fail + ":")
	e.line("  mov rax, 0")
	e.line(end + ":")
	return nil
}

func (e *Emitter) genOr(args []Node) error {
	if len(args) == 0 {
		e.line("  mov rax, 0") // false
		return nil
	}
	success := e.newLabel("or_success")
	end := e.newLabel("or_end")
	for _, arg := range args {
		if err := e.Gen(arg); err != nil {
			return err
		}
		e.line("  cmp rax, 0")
		e.line("  jne " + success)
	}
	e.line("  mov rax, 0")
	e.line("  jmp " + end)
	e.line(success + ":")
	e.line("  mov rax, 1")
	e.line(end + ":")
	return nil
}

// Control flow: if
func (e *Emitter) genIf(args []Node) error {
	if len(args) != 3 {
		return fmt.Errorf("if expects (if cond then else)")
	}
	elseLabel := e.newLabel("else")
	endLabel := e.newLabel("endif")
	if err := e.Gen(args[0]); err != nil {
		return err
	}
	e.line("  cmp rax, 0")
	e.line("  je " + elseLabel)

	if err := e.Gen(args[1]); err != nil {
		return err
	}
	e.line("  jmp " + endLabel)

	e.line(elseLabel + ":")
	if err := e.Gen(args[2]); err != nil {
		return err
	}
	e.line(endLabel + ":")
	return nil
}

// Sequencing: begin, progn
func (e *Emitter) genBegin(args []Node) error {
	if len(args) == 0 {
		e.line(" mov rax, 0")
		return nil
	}
	for i := range args {
		if err := e.Gen(args[i]); err != nil {
			return err
		}
	}
	// The value of the last expression is the value of (begin ...).
	return e.Gen(args[len(args)-1])
}

// Bindings: let* (sequential)
func (e *Emitter) genLetStar(args []Node) error {
	if len(args) < 2 {
		return fmt.Errorf("let* expects ((var expr) ...) and a body")
	}
	// args[0] is the bindings list
	bindList, ok := args[0].(*List)
	if !ok {
		return fmt.Errorf("let*: first arg must be a list of bindings")
	}

	e.envPush()
	bound := 0

	// Process each (name expr) in order, later inits can see earlier bindings
	for _, b := range bindList.Items {
		pair, ok := b.(*List)
		if !ok || len(pair.Items) != 2 {
			return fmt.Errorf("let*: each binding must be (name expr)")
		}
		nameSym, ok := pair.Items[0].(*Symbol)
		if !ok {
			return fmt.Errorf("let*: variable name must be a symbol")
		}

		// Evaluate initializer in the current env (sees previous let* bindings)
		if err := e.Gen(pair.Items[1]); err != nil {
			return err
		}

		// Allocate 8 bytes for this local and store value
		e.line(" sub rsp, 8")
		e.locals += 8
		offset := e.locals
		e.line(fmt.Sprintf(" mov [rbp-%d], rax", offset))
		e.envBind(nameSym.Name, offset)
		bound++
	}

	// Evaluate body; result of last expr stays in RAX
	if err := e.genBegin(args[1:]); err != nil {
		return err
	}

	// Pop the locals allocated in this scope and pop scope
	if bound > 0 {
		e.line(fmt.Sprintf(" add rsp, %d", bound*8))
		e.locals -= bound * 8
	}
	e.envPop()
	return nil
}
