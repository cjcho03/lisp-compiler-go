package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	// Read all stdin into a single string (easy for piping)
	sc := bufio.NewScanner(os.Stdin)
	sc.Buffer(make([]byte, 0, 1<<20), 1<<20)
	var lines []string
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}
	if err := sc.Err(); err != nil {
		fmt.Fprintln(os.Stderr, "read error:", err)
		os.Exit(1)
	}
	src := strings.TrimSpace(strings.Join(lines, "\n"))
	if src == "" {
		fmt.Fprintln(os.Stderr, `usage: echo "(+ 1 (* 2 3))" | go run . > out.s`)
		os.Exit(2)
	}

	ast, err := Parse(src)
	if err != nil {
		fmt.Fprintln(os.Stderr, "parse error:", err)
		os.Exit(3)
	}

	var e Emitter
	e.Prologue()
	if err := e.Gen(ast); err != nil {
		fmt.Fprintln(os.Stderr, "codegen error:", err)
		os.Exit(4)
	}
	e.Epilogue()

	os.Stdout.WriteString(e.String())
}
