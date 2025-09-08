package main

import (
	"strconv"
	"strings"
)

type Token struct {
	Kind string // "LP", "RP", "NUM", "SYMBOL", "EOF"
	Text string
}

func lex(input string) []Token {
	input = strings.TrimSpace(input)
	var toks []Token
	i := 0
	emit := func(k, t string) { toks = append(toks, Token{Kind: k, Text: t}) }

	for i < len(input) {
		c := input[i]
		switch {
		case c == '(':
			emit("LP", "(")
			i++
		case c == ')':
			emit("RP", ")")
			i++
		case c == ';':
			// comment until end of line
			for i < len(input) && input[i] != '\n' {
				i++
			}
		case c == ' ' || c == '\t' || c == '\n' || c == '\r':
			i++
		default:
			start := i
			for i < len(input) {
				ch := input[i]
				if ch == '(' || ch == ')' || ch == ';' || ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
					break
				}
				i++
			}
			raw := input[start:i]
			if _, err := strconv.ParseInt(raw, 10, 64); err == nil {
				emit("NUM", raw)
			} else {
				emit("SYMBOL", raw)
			}
		}
	}
	emit("EOF", "")
	return toks
}
