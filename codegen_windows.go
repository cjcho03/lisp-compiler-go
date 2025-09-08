//go:build windows
// +build windows

package main

func (e *Emitter) Prologue() {
	e.line(".intel_syntax noprefix")
	e.line(".data")
	e.line(`fmt: .asciz "%lld\n"`)
	e.line(".text")
	e.line(".globl main")
	e.line("main:")
	e.line("  push rbp")
	e.line("  mov rbp, rsp")
}

func (e *Emitter) Epilogue() {
	e.line("  mov rdx, rax")       // 2nd arg
	e.line("  lea rcx, [rip+fmt]") // 1st arg
	e.line("  sub rsp, 32")        // shadow space (Win64 ABI)
	e.line("  call printf")
	e.line("  add rsp, 32")
	e.line("  mov eax, 0")
	e.line("  pop rbp")
	e.line("  ret")
}
