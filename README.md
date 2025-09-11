# lisp-compiler-go

Minimal Lisp → **x86-64 assembly** compiler written in Go.
Currently supports integers, arithmetic, comparisons, short-circuit logic, and `if`.
Emits Intel-syntax GAS; prints the final result via `printf`.

## Features (current)

* **Numbers**: 64-bit signed integers
* **Arithmetic**: `+  -  *  /`

  * `(+ ) → 0`, `(* ) → 1`, `(- x) → -x`, `(/ x y …)` is left-assoc integer division
* **Comparisons** → boolean `0/1`: `=  !=  /=  <  <=  >  >=`
* **Logic** (short-circuit): `not  and  or`
* **Control flow**: `(if cond then else)` (non-zero is true)
* **Cross-platform codegen**: Linux (SysV) & Windows (Win64) via build tags

## Quick start

### Prerequisites

* **Go 1.21+**
* A C toolchain:

  * **Linux/WSL**: `sudo apt install -y build-essential`
  * **Windows (native)**: install **MSYS2/MinGW** and ensure `gcc` is on PATH
    *(Alternatively, run everything inside WSL.)*

### Run a one-off compile & execute

```bash
# From repo root
echo "(+ 1 (* 2 3))" | go run . > build/out.s
gcc -no-pie build/out.s -o build/a.out   # on Linux/WSL
./build/a.out                             # prints 7
```

Windows (native MinGW):

```powershell
echo "(+ 1 (* 2 3))" | go run . > build/out.s
gcc build/out.s -o build/a.exe
.\build\a.exe
```

### Or use the Makefile (Linux/WSL)

```bash
make run     # builds build/out.s → build/a.out and runs it
make test    # runs the Go test suite (see below)
```

## Usage examples

```lisp
(+ 1 2)                         ; 3
(+ 1 (* 2 3))                   ; 7
(- 7)                           ; -7
(/ (* 10 4) (+ 5 5))            ; 4

(= 3 3)                         ; 1
(!= 3 4)                        ; 1
(/= 5 5)                        ; 0        ; Common Lisp spelling

(not 0)                         ; 1
(and 1 2 3)                     ; 1
(and 1 0 3)                     ; 0
(or 0 0 7)                      ; 1

(if (< 1 2) 42 13)              ; 42
(if (and (< 1 2) (> 5 3)) 9 8)  ; 9
```

### Truthiness

* `0` → false
* any non-zero → true
  Comparisons and logic return `0` or `1`.

## Project layout

```
.
├─ main.go                # CLI: read S-expr on stdin → emit assembly on stdout
├─ lexer.go               # Tokenizer
├─ ast.go                 # AST node types (Num, Symbol, List)
├─ parser.go              # Parse S-expressions → AST
├─ codegen_common.go      # Emitter, Gen, arithmetic/logic/cmp/if
├─ codegen_linux.go       # //go:build linux → SysV prologue/epilogue (printf@PLT)
├─ codegen_windows.go     # //go:build windows → Win64 ABI (RCX/RDX + shadow space)
├─ compiler_test.go       # End-to-end tests (compile→gcc→run→assert)
├─ Makefile               # optional helpers
└─ README.md
```

> Go build tags ensure the right prologue/epilogue is used per OS.

## Tests

Black-box tests compile each expression, assemble with `gcc`, run the result, and assert stdout.

```bash
go test -v
```

If you ever see `expected identifier, found "."` during `go test`, you likely have a stray `out.s` in the repo root; remove it (Go tries to assemble any `*.s` it finds). Use the `build/` folder for artifacts.

## Design notes

* Output is a single program with its **final value in `RAX`**, then printed via `printf`.
* **Linux (SysV)**: args in `RDI/RSI`, `.section .rodata`, `printf@PLT`.
* **Windows (Win64)**: args in `RCX/RDX`, 32-byte shadow space, `.data` + `.asciz`.
* No runtime or GC—pure codegen for expressions.

## Limitations / TODO

* No variables (`let`), sequences (`begin`/`progn`), or user functions (`defun`) yet
* No pairs/cons/lists at runtime (only compile-time lists as AST)
* No divide-by-zero checks (will trap on `idiv`)
* Integers only; no strings as values

## Roadmap (next steps)

* **Bindings**: `(let ((x 10) (y 20)) (+ x y))` with stack slots
* **Sequences**: `(begin expr1 expr2 ... exprN)`
* **Functions**: `defun`, calls, and a basic calling convention
* **Lists**: `cons`, `car`, `cdr`, and a tiny heap
* **Errors**: runtime guards (e.g., divide by zero)
* **Multiple top-level forms**: compile & print each / or a `main` block
