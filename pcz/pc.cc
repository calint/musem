asm(".code16");
asm("_start:");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
//asm("xor %ax,%ax");// initiate cpu state
//asm("mov %ax,%ds");// data segment
//asm("mov %ax,%ss");// stack segment
//asm("mov $0x7c00,%sp");// stack pointer
//asm("cld");// clear direction flag
// setup_screen:
//asm("cli");
asm("mov $0x13,%ax");// bios:vga 320x200x8b
asm("int $0x10");
asm("mov $0xa000,%ax");// segment to vga buffer
asm("mov %ax,%es");
asm("movw $0x0101,%es:0x0");
asm("movw $0x0202,%es:0x4");
asm("movw $0x0303,%es:0x8");
asm("movw $0x0404,%es:0xc");
asm("movw $0x0505,%es:0x10");
asm("jmp .");
/*
.setup_load:
;	mov ah,0		; reset disk system (necessary?)
;	int 13h
	; setup read
	mov ax,0x021f	; bios:read(02) 1fh sectors x 512B
	mov cx,0x0002	; from cylinder 0 (ch) sector 2 (1st=1)
	mov dh,0		; head 0
;	mov dl,[k0.dr]	; default drive
	xor bx,bx
	mov es,bx
	mov bx,0x7e00
	int 13h			; load $7e00 to $8c00
	jnc .setup_drw	; if no error continue
	mov ax,0a000h	; error while reading
	mov es,ax		; put red on screen
	mov word[es:0],0x0404
	jmp $			; hang
.setup_drw:
	mov ax,0a000h	; segment register to vga
	mov es,ax		;
	mov word[es:8],0x0202 ; status

	; ds clear at line 24,25 
	mov cx,4096*4   ; copy 16k from 7c00 to a0000+320*100
	mov si,7c00h
	mov di,320*100
	rep movsb

	mov word[es:12],0x0303
;	jmp $
*/
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".space _start+436-.,0");//reserved
asm(".space 10,0");//partition table
asm(".space 16,0");// #1
asm(".space 16,0");// #2
asm(".space 16,0");// #3
asm(".space 16,0");// #4
asm(".word 0xaa55");//pc boot sector signature
