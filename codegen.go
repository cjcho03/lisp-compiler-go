package main

import (
	"fmt"
	"strings"
)

type Emitter struct {
	sb strings.Builder
}

func (e *Emitter) String() string { return e.sb.String() }
func (e *Emitter) line(s string)  { e.sb.WriteString(s); e.sb.WriteByte('\n') }

func (e *Emitter) Gen(n Node) error {
	switch t := n.(type) {
	case *Num:
		e.line(fmt.Sprintf("  mov rax, %d", t.Val))
		return nil

	case *Symbol:
		return fmt.Errorf("bare symbol %q not allowed", t.Name)

	case *List:
		// Empty list
		if len(t.Items) == 0 {
			e.line("  mov rax, 0")
			return nil
		}
		// Expect: (op arg1 arg2 ...)
		head, ok := t.Items[0].(*Symbol)
		if !ok {
			return fmt.Errorf("operator must be a symbol at list head")
		}
		args := t.Items[1:]
		switch head.Name {
		case "+", "-", "*", "/":
			return e.genArith(head.Name, args)
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
