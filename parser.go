package main

import (
	"fmt"
	"strconv"
)

type parser struct {
	toks []Token
	pos  int
}

func (p *parser) peek() Token {
	if p.pos >= len(p.toks) {
		return Token{Kind: "EOF"}
	}
	return p.toks[p.pos]
}

func (p *parser) next() Token {
	t := p.peek()
	p.pos++
	return t
}

func Parse(input string) (Node, error) {
	p := &parser{toks: lex(input)}
	n, err := p.parseSexpr()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind != "EOF" {
		return nil, fmt.Errorf("extra tokens after expression")
	}
	return n, nil
}

func (p *parser) parseSexpr() (Node, error) {
	t := p.next()
	switch t.Kind {
	case "LP":
		var items []Node
		for {
			pt := p.peek()
			if pt.Kind == "EOF" {
				return nil, fmt.Errorf("unmatched '('")
			}
			if pt.Kind == "RP" {
				p.next() // consume ')'
				break
			}
			n, err := p.parseSexpr()
			if err != nil {
				return nil, err
			}
			items = append(items, n)
		}
		return &List{Items: items}, nil

	case "NUM":
		v, _ := strconv.ParseInt(t.Text, 10, 64)
		return &Num{Val: v}, nil

	case "SYMBOL":
		return &Symbol{Name: t.Text}, nil

	case "RP":
		return nil, fmt.Errorf("unexpected ')'")
	case "EOF":
		return nil, fmt.Errorf("unexpected EOF")
	default:
		return nil, fmt.Errorf("unknown token kind %q", t.Kind)
	}
}

// parseProgram parses 1+ top-level S-expressions until EOF
func ParseProgram(input string) (Node, error) {
	p := &parser{toks: lex(input)}

	var forms []Node
	for p.peek().Kind != "EOF" {
		n, err := p.parseSexpr()
		if err != nil {
			return nil, err
		}
		forms = append(forms, n)
	}

	if len(forms) == 0 {
		return nil, fmt.Errorf("empty input")
	}
	if len(forms) == 1 {
		return forms[0], nil
	}

	// (begin form1 form2 ...)
	items := make([]Node, 0, len(forms)+1)
	items = append(items, &Symbol{Name: "begin"})
	items = append(items, forms...)
	return &List{Items: items}, nil
}
