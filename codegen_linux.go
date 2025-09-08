//go:build linux
// +build linux

package main

func (e *Emitter) Prologue() {
	e.line(".intel_syntax noprefix")
	e.line(".section .rodata")
	e.line(`fmt: .string "%ld\n"`)
	e.line(".text")
	e.line(".globl main")
	e.line("main:")
	e.line("  push rbp")
	e.line("  mov rbp, rsp")
}

func (e *Emitter) Epilogue() {
	e.line("  mov rsi, rax")
	e.line("  lea rdi, [rip+fmt]")
	e.line("  xor eax, eax")
	e.line("  call printf@PLT")
	e.line("  mov eax, 0")
	e.line("  pop rbp")
	e.line("  ret")
}
