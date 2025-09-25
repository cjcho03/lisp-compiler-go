# lisp-compiler-go

Minimal Lisp → **x86-64 assembly** compiler written in Go.  
Emits Intel-syntax GAS and prints the final result via `printf`.

## Features (current)

- **Numbers**: 64-bit signed integers
- **Arithmetic**: `+  -  *  /`  
  - `(+ ) → 0`, `(* ) → 1`, `(- x) → -x`, `(/ x y …)` is left-assoc integer division
- **Comparisons** → boolean `0/1`: `=  !=  /=  <  <=  >  >=`
- **Logic** (short-circuit): `not  and  or`
- **Control flow**: `(if cond then else)` (non-zero is true)
- **Variables (locals)**: **`let*`** with sequential bindings, lexical scoping, shadowing
- **Sequencing**: **`begin` / `progn`** evaluate multiple expressions; result is the last
- **Cross-platform codegen**: Linux (SysV) & Windows (Win64) via build tags

## Quick start

### Prerequisites
- **Go 1.21+**
- A C toolchain:
  - **Linux/WSL**: `sudo apt install -y build-essential`
  - **Windows (native)**: install **MSYS2/MinGW** and ensure `gcc` is on PATH  
    *(Alternatively, run everything inside WSL.)*

### Run a one-off compile & execute

```bash
# From repo root
echo "(+ 1 (* 2 3))" | go run . > build/out.s
gcc -no-pie build/out.s -o build/a.out   # on Linux/WSL
./build/a.out                             # prints 7
````

Windows (native MinGW):

```powershell
echo "(+ 1 (* 2 3))" | go run . > build/out.s
gcc build/out.s -o build/a.exe
.\build\a.exe
```

### Or use the Makefile (Linux/WSL)

```bash
make run     # builds build/out.s → build/a.out and runs it
make test    # runs the Go test suite
```

## Usage examples

```lisp
;; arithmetic
(+ 1 2)                           ; 3
(+ 1 (* 2 3))                     ; 7
(- 7)                             ; -7
(/ (* 10 4) (+ 5 5))              ; 4

;; comparisons + logic
(= 3 3)                           ; 1
(!= 3 4)                          ; 1
(/= 5 5)                          ; 0        ; Common Lisp spelling
(not 0)                           ; 1
(and 1 2 3)                       ; 1
(and 1 0 3)                       ; 0
(or 0 0 7)                        ; 1

;; control flow
(if (< 1 2) 42 13)                ; 42
(if (and (< 1 2) (> 5 3)) 9 8)    ; 9

;; let* (sequential) + lexical scope
(let* ((x 10) (y 20)) (+ x y))    ; 30
(let* ((x 2) (y (+ x 3))) y)      ; 5   ; y sees x
(let* ((x 1)) (let* ((x 7) (y 4)) (+ x y))) ; 11 (shadowing)

;; begin / progn sequencing: value is the last expression
(begin (+ 5 1) (* 5 2) (- 5 3))    ; 2
(progn 1 2 3)                      ; 3
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
├─ codegen_common.go      # Emitter, env/scope, arithmetic/logic/cmp/if, let*, begin/progn
├─ codegen_linux.go       # //go:build linux → SysV prologue/epilogue (printf@PLT)
├─ codegen_windows.go     # //go:build windows → Win64 ABI (RCX/RDX + shadow space)
├─ compiler_test.go       # End-to-end tests (compile→gcc→run→assert)
├─ Makefile               # optional helpers (build/run/test)
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

* The program evaluates a single top-level S-expression; the **final value in `RAX`** is printed via `printf`.
* **Env & locals**: `let*` binds symbols to stack slots at fixed `[rbp - offset]`.
  Each binding allocates 8 bytes with `sub rsp, 8`; values are stored and later popped when the scope ends.
* **Linux (SysV)**: args in `RDI/RSI`, `.section .rodata`, `printf@PLT`.
* **Windows (Win64)**: args in `RCX/RDX`, 32-byte shadow space, `.data` + `.asciz`.
* No runtime/GC—pure codegen for expressions.

## Limitations / TODO

* **Parallel `let`** (evaluate all RHS first, then bind)
* **Assignments** (e.g., `set!`), mutations
* **Functions**: `defun`, calls, returns (register/stack calling convention)
* **Multiple top-level forms** (sequence or a dedicated `main` block)
* **Lists at runtime**: `cons`, `car`, `cdr` (heap / bump allocator)
* **Errors**: runtime guards (e.g., divide-by-zero)

## Roadmap (next steps)

* `let` (parallel) and a temporary area for initializers
* `begin` at top-level / multiple forms
* `defun` + calls (with proper SysV/Win64 arg passing and caller/callee save)
* Minimal heap + pairs for list ops
