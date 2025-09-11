package main

import (
	"fmt"
	"strings"
)

type Emitter struct {
	sb           strings.Builder
	labelCounter int
}

func (e *Emitter) String() string { return e.sb.String() }
func (e *Emitter) line(s string)  { e.sb.WriteString(s); e.sb.WriteByte('\n') }

func (e *Emitter) newLabel(prefix string) string {
	e.labelCounter++
	return fmt.Sprintf(".L%s_%d", prefix, e.labelCounter)
}

func (e *Emitter) Gen(n Node) error {
	switch t := n.(type) {
	case *Num:
		e.line(fmt.Sprintf("  mov rax, %d", t.Val))
		return nil

	case *Symbol:
		return fmt.Errorf("bare symbol %q not allowed (expect a list)", t.Name)

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

		default:
			return fmt.Errorf("unsupported operator %q", head.Name)
		}

	default:
		return fmt.Errorf("unknown node type %T", n)
	}
}

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
