section .text
extern printf

__print_int:
	mov rsi, [rsp + 8]
	push rsp
	push rsi
	push rcx
	push rdx
	push rbx
	push r8
	push r9
	push r10
	push r11
	mov rbx, rsp
	and rbx, 1111b
	sub rsp, rbx
	lea rdi, [int_to_string]
	xor rax, rax
	call printf wrt ..plt
	add rsp, rbx
	xor rax, rax
	pop r11
	pop r10
	pop r9
	pop r8
	pop rbx
	pop rdx
	pop rcx
	pop rsi
	pop rsp
	ret
section .data
int_to_string: db '%d', 10, 0
_true: dq 1
_false: dq 0 
_print_int: dq __print_int
