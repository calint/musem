bits 16
org 7c00h
.setup_screen:
	cli
	xor ax,ax
	mov bx,ax
	mov cx,ax
	mov dx,ax
	mov si,ax
	mov di,ax
	mov bp,ax
	mov ds,ax
	mov es,ax
	mov ss,ax
	mov sp,0x7c00
	cld
	mov ax,13h		; bios:vga 320x200x8b
	int 10h
	mov ax,0a000h
	mov es,ax
	mov word[es:0x0],0x0101
	mov word[es:0x4],0x0202
	mov word[es:0x8],0x0303
	mov word[es:0xc],0x0404
	mov word[es:0x10],0x0505
	jmp $
times 436-($-$$) db 0
times 10 db 0       ; optional disk signature
times 16 db 0       ; partion 0
times 16 db 0       ; partion 1
times 16 db 0       ; partion 2
times 16 db 0       ; partion 3
	dw 0xaa55
