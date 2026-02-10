package main

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func compileAndRun(t *testing.T, expr string) string {
	t.Helper()

	// 1) Build assembly in-process using your parser + codegen
	ast, err := ParseProgram(expr)
	if err != nil {
		t.Fatalf("parse error for %q: %v", expr, err)
	}
	var e Emitter
	e.Prologue()
	e.envPush()
	if err := e.Gen(ast); err != nil {
		t.Fatalf("codegen error for %q: %v", expr, err)
	}
	e.Epilogue()

	dir := t.TempDir()
	sPath := filepath.Join(dir, "out.s")
	if err := os.WriteFile(sPath, []byte(e.String()), 0644); err != nil {
		t.Fatalf("write asm: %v", err)
	}

	// 2) Assemble & link with gcc (platform-specific flags / exe name)
	var exePath string
	var cc *exec.Cmd
	switch runtime.GOOS {
	case "linux":
		exePath = filepath.Join(dir, "a.out")
		cc = exec.Command("gcc", "-no-pie", sPath, "-o", exePath)
	case "windows":
		exePath = filepath.Join(dir, "a.exe")
		cc = exec.Command("gcc", sPath, "-o", exePath)
	default:
		t.Skipf("end-to-end test skipped on GOOS=%s (needs linux/windows toolchain)", runtime.GOOS)
	}
	if out, err := cc.CombinedOutput(); err != nil {
		t.Fatalf("gcc failed: %v\n%s", err, string(out))
	}

	// 3) Run the program and capture stdout
	run := exec.Command(exePath)
	out, err := run.CombinedOutput()
	if err != nil {
		t.Fatalf("program failed: %v\n%s", err, string(out))
	}
	return strings.TrimSpace(string(out))
}

func TestArithmeticEndToEnd(t *testing.T) {
	tests := []struct {
		expr string
		want string
	}{
		// Basic arithmetic
		{`(+ 1 2)`, "3"},
		{`(- 5 2)`, "3"},
		{`(* 4 3)`, "12"},
		{`(/ 20 4)`, "5"},

		// Unary minus
		{`(- 7)`, "-7"},
		{`(- -5)`, "5"},

		// Nesting
		{`(+ 1 (* 2 3))`, "7"},
		{`(- (+ 10 5) 3)`, "12"},
		{`(* (+ 2 3) (- 8 6))`, "10"},
		{`(/ (* 10 4) (+ 5 5))`, "4"},

		// Associativity
		{`(- 10 2 3)`, "5"},
		{`(/ 100 2 5)`, "10"},

		// Edge arities
		{`(+)`, "0"},
		{`(*)`, "1"},

		// Stress nesting
		{`(+ 1 (+ 2 (+ 3 (+ 4 5))))`, "15"},
		{`(* (* 2 3) (* 4 5))`, "120"},

		// Division with negatives
		{`(/ -20 5)`, "-4"},
		{`(/ 20 -5)`, "-4"},
		{`(/ -20 -5)`, "4"},
	}

	for _, tc := range tests {
		tc := tc
		t.Run(tc.expr, func(t *testing.T) {
			got := compileAndRun(t, tc.expr)
			if got != tc.want {
				t.Fatalf("expr=%s: got %s, want %s", tc.expr, got, tc.want)
			}
		})
	}
}

func TestCompareLogicIf(t *testing.T) {
	tests := []struct {
		expr string
		want string
	}{
		// Comparisons
		{"(= 3 3)", "1"},
		{"(= 3 4)", "0"},
		{"(< 1 2)", "1"},
		{"(> 1 2)", "0"},
		{"(<= 2 2)", "1"},
		{"(>= 3 4)", "0"},
		{"(!= 3 4)", "1"},
		{"(!= 5 5)", "0"},
		{"(/= 3 4)", "1"},
		{"(/= 5 5)", "0"},

		// Logical
		{"(not 0)", "1"},
		{"(not 5)", "0"},
		{"(and 1 2 3)", "1"},
		{"(and 1 0 3)", "0"},
		{"(or 0 0 7)", "1"},
		{"(or 0 0 0)", "0"},

		// If
		{"(if (= 1 1) 42 13)", "42"},
		{"(if 0 1 2)", "2"},
		{"(if 5 1 2)", "1"},

		// mixes
		{"(if (and (< 1 2) (> 5 3)) 9 8)", "9"},
		{"(if (or (= 0 1) (= 2 2)) 11 12)", "11"},
	}

	for _, tc := range tests {
		t.Run(tc.expr, func(t *testing.T) {
			got := compileAndRun(t, tc.expr)
			if got != tc.want {
				t.Fatalf("expr=%s: got %s, want %s", tc.expr, got, tc.want)
			}
		})
	}
}

func TestLetStarAndBegin(t *testing.T) {
	tests := []struct {
		expr string
		want string
	}{
		{`(let* ((x 10) (y 20)) (+ x y))`, "30"},
		{`(let* ((x 2) (y (+ x 3))) y)`, "5"},                   // y sees x (sequential)
		{`(let* ((x 5)) (begin (+ x 1) (* x 2) (- x 3)))`, "2"}, // begin/progn sequencing; last result returned
		{`(let* ((x 1)) (let* ((x 7) (y 4)) (+ x y)))`, "11"},   // shadowing in nested scope
		{`(progn 1 2 3)`, "3"},
	}

	for _, tc := range tests {
		t.Run(tc.expr, func(t *testing.T) {
			got := compileAndRun(t, tc.expr)
			if got != tc.want {
				t.Fatalf("expr=%s: got %s, want %s", tc.expr, got, tc.want)
			}
		})
	}
}

func TestMultipleTopLevelForms_SpaceSeparated(t *testing.T) {
	src := "1 2 3" // should behave like (begin 1 2 3) => 3
	out := compileAndRun(t, src)
	if strings.TrimSpace(out) != "3" {
		t.Fatalf("expected 3, got %q", out)
	}
}

func TestMultipleTopLevelForms_NewlineSeparated(t *testing.T) {
	src := "1\n2\n3\n" // should behave like (begin 1 2 3) => 3
	out := compileAndRun(t, src)
	if strings.TrimSpace(out) != "3" {
		t.Fatalf("expected 3, got %q", out)
	}
}

func TestLetParallel(t *testing.T) {
	tests := []struct {
		expr string
		want string
	}{
		// Basic parallel let
		{`(let ((x 10) (y 20)) (+ x y))`, "30"},

		// Key semantics: sibling initializers do NOT see each other
		// Outer x=2; in (let ((x 5) (y x)) y), y's init sees outer x (2), not the new x (5).
		{`(let* ((x 2)) (let ((x 5) (y x)) y))`, "2"},

		// Contrast: let* DOES allow y to see x
		{`(let* ((x 2)) (let* ((x 5) (y x)) y))`, "5"},

		// Another example where RHS uses outer bindings
		{`(let* ((a 7)) (let ((a 1) (b (+ a 3))) b))`, "10"}, // b sees outer a=7
	}

	for _, tc := range tests {
		t.Run(tc.expr, func(t *testing.T) {
			got := compileAndRun(t, tc.expr)
			if got != tc.want {
				t.Fatalf("expr=%s: got %s, want %s", tc.expr, got, tc.want)
			}
		})
	}
}
