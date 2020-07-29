section .text
extern printf

__print_int:
	mov rsi, r12 
	push rsp
	push rdi
	push rsi
	push rbx
	push rcx
	push rdx
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
	pop rdx
	pop rcx
	pop rbx
	pop rsi
	pop rdi
	pop rsp
	ret

_custom_malloc:
	push rsp
	push rdi
	push rsi
	push rbx
	push rcx
	push rdx
	push r8
	push r9
	push r10
	push r11
	mov rbx, rsp
	and rbx, 1111b
	sub rsp, rbx
	mov rdi, rax 
	xor rax, rax
	call malloc wrt ..plt
	add rsp, rbx
	pop r11
	pop r10
	pop r9
	pop r8
	pop rdx
	pop rcx
	pop rbx
	pop rsi
	pop rdi
	pop rsp
	ret

section .data
int_to_string: db '%d', 10, 0
_True: dq 1
_False: dq 0 
__dummy1: dq __print_int
_print_int: dq __dummy1
