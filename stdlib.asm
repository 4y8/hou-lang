section .text
extern printf

__print_int:
	mov rsi, [rsp + 8]
	mov rax, rsp 
	and rax, 1111b
	cmp rax, 0
	je .no_align
	sub rsp, 8
	lea rdi, [int_to_string]
	xor rax, rax
	call printf wrt ..plt
	xor rax, rax
	add rsp, 8
	ret
	.no_align:
	lea rdi, [int_to_string]
	xor rax, rax
	call printf wrt ..plt
	xor rax, rax
	ret
section .data
int_to_string: db '%d', 10, 0
_true: dq 1
_false: dq 0 
_print_int: dq __print_int
