; snippet library osmo32
;   compile with    http://www.nasm.us/pub/nasm/releasebuilds/2.06/
;
; tested on:
;   qemu linux : ok
;      qemu win: ok
;       eeepc4g: ok
;         6720s: ok
;          vaio: ok
txt:
mem:
.idta	equ 0h
.idtl	equ 128*8-1 	; 128 400h-1
.pdta	equ 1000h
.pdtl	equ 1*4-1
.peta	equ 2000h
.petl	equ 16*16*4-1   ; 256 map first M 400h-1 
.vgaa   equ 0a0000h
.vgal   equ 320*200-1

org 7c00h
;	jump over BIOS Parameter Block (BPB) because 
;	that memory may be written to by BIOS when booting from USB

	jmp k0s
	times 3-($-$$) db 0; support 2 or 3 byte encoded jmp

	; fake BPB
	times 59 db 0
k0s:
.setup_init:
	xor ax,ax		; initiate cpu state
	mov ds,ax		; data segment
	mov ss,ax		; stack segment
	mov sp,k0s		; setup stack pointer
	mov [k0s.dr],dx	; save the boot drive (dl)
	cld				; clear direction flag
.setup_screen:
	mov ax,13h		; bios:vga 320x200x8b
	int 10h
	mov ax,0a000h
	mov es,ax
	mov word[es:0],0x0202
	mov word[es:4],0x0101
;	jmp $
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
.setup_enable_a20:
	in al,0x92      ; enable a20 line
	or al,2         ;  to access
	out 0x92,al     ;    odd megs
	mov word[es:4],0x0303
.setup_pic:
;	jmp $
.setup_enable_pm:
;	xor ax,ax
;	mov ds,ax
	lgdt [.gdtr]    ; load gdt
	mov eax,cr0		; enable
	or al,1			; protected
	mov cr0,eax		; mode
	jmp 8:setup_pm	; jmp to flush
;...............................
align 16
.gdt	dq 0x0000000000000000 ;  4g pl0
		dq 0x00cf9a000000ffff ; code rx
		dq 0x00cf92000000ffff ; data rw
.gdt.bmp equ ($-.gdt)
	dw 320*200,mem.vgaa&0ffffh
 	db mem.vgaa>>16	;
 	db 0x92			; data rw pl3
	db 0xcf			;
	db 0h			;
;.gdt.stk equ ($-.gdt)
;	dw stke-stk-1,stk;
; 	db 0h			;
; 	db 0x96			; data rw pl0 expdown
;	db 0x00			;
;	db 0h			;
;...............................
.gdtr   dw .gdtr-.gdt-1         ; limit 
        dd .gdt                 ; offset
		dw 0x0000
.idtr   dw 0x03ff
        dd mem.idta
;...............................
;  data
;...............................
align 4
.dr		dw 0			; drive (dx at boot, dl is boot drive)
.sc		dw 0 			; last scan code 
.tc		dd 0			; tick count (inc by timer)
.fc		dd 0			; frame count (inc by loop)


times 436-($-$$) db 0
times 10 db 0       ; optional disk signature
times 16 db 0       ; partion 0
times 16 db 0       ; partion 1
times 16 db 0       ; partion 2
times 16 db 0       ; partion 3
	dw 0xaa55
	
;...............................
sector2: 				; 07e00h
align 16	; why not 32?
bits 32
setup_pm:
	cli	; disable interrupts (why again? on asus zenbook interrupts are enabled)
	mov dword[es:16],0x04040404
;	mov dword[0xa0010],0x04040404
;	jmp $
setup_isr:
	mov bp,mem.idta
	mov cx,128 					; 0h to 400h with idt
	.1	mov word[bp  ],kierr	; offset 0_16
		mov word[bp+2],8        ; segsel
		mov word[bp+4],0x8f00	; idt pl0 prsnt
		mov word[bp+6],0 		; offset 16_32
		add bp,8 				;
	loop .1
	mov word[8h*8],kitmr		; timer
	mov word[9h*8],kikbd		; keyb
	mov word[0eh*8],kipf		; page fault
	lidt [k0s.idtr]
	mov dword[es:24],0x05050505
;	jmp $
setup_pages:
	mov bp,mem.peta             ;
	mov eax,0x00000001          ; page entry template
	mov cx,(mem.petl+1)>>2      ; 256x4K pages
	.2	mov dword[bp],eax       ; mapped to 00:0000h-10:0000h
		add eax,0x00001000      ;
		add bp,4                ;
	loop .2                     ;

;	xor byte[mem.peta+4*0b0h],1 ; make page at b:0000h not present

	mov eax,mem.peta            ; ptr to page entry table
	or eax,1                    ; present
	mov dword[mem.pdta],eax     ; page directory table

	mov eax,cr3         ; cr3 ptr to page directory table
	and eax,0fffh		; preserved reserved bits
	or eax,mem.pdta     ; address
	mov cr3,eax         ;

	mov dword[es:32],0x06060606
;	jmp $
setup_pe:
	mov eax,cr0			; enable paging
	or eax,80000000h	; pe bit
	mov cr0,eax			; paging on

	mov dword[es:40],0x07070707
;	mov byte[100000h],1 ; generates page fault, address not mapped in pte
;	jmp $
setup_usermode:
;	mov dword[es:(320*200-4)],0x02020202
;	mov ax,.gdt.bmp
	mov ax,0x10
	mov ds,ax
	mov fs,ax
	mov gs,ax
	mov ss,ax
	mov esp,stke

	mov ax,0x18
	mov es,ax

	mov dword[es:48],0x08080808
;	jmp $
	sti		; enable interrupts
	jmp k1s	; jump to program

;...............................
;  isr
;...............................
align 16
kierr:
	cli
	mov dword[es:000],0x06060606
	mov dword[es:320],0x06060606
	mov dword[es:640],0x06060606
	mov dword[es:960],0x06060606
	jmp $

align 16
kipf:
	cli
	mov dword[es:000],0x04040404
	mov dword[es:320],0x04040404
	mov dword[es:640],0x04040404
	mov dword[es:960],0x04040404
	jmp $

align 16
kikbd:
	cli
	push ax
	in ax,60h
	mov word[es:310],ax
	mov word[k0s.sc],ax
;	mov word[k1.data.dcolr],ax
;	mov word[k1.data.dchp],ax
	cmp ax,3920h
	jne .11
		mov dword[k1s.data.dchp],0
	.11:
	mov al,0x20		;ack intrp
	out 0x20,al     ;mstr pic
	pop ax
	sti
	iret

align 16
kitmr:
	cli
	push eax
	mov eax,dword[k0s.tc]
	inc eax
	mov dword[k0s.tc],eax
	mov dword[es:316],eax

	push ebx
	mov ebx,win.t.8
	dec dword[ebx+win.x]
;	dec dword[ebx+win.y]
	pop ebx

	;call kw.itmr
	mov al,0x20		;ack intrp
	out 0xa0,al
	out 0x20,al
	pop eax
	sti
	iret
;
; 4x4 font
;
fnt:
.wi		equ 4
.hi		equ 4
.s		equ 2
.b		equ 1
.t:
.nbr.i equ ($-fnt.t)>>fnt.b
; .o..
; o.o.
; .o..
; ....
dw 0100101001000000b
; oo..
; .o..
; .o..
; ....
dw 1100010001000000b
; o...
; .o..
; .oo.
; ....
dw 1000010001100000b
; .o..
; ..o.
; .oo.
; ....
dw 0100001001100000b
; o...
; o.o.
; ..o.
; ....
dw 1000101000100000b
; ..o.
; .o..
; oo..
; ....
dw 0010010011000000b
; .o..
; o...
; .oo.
; ....
dw 0100100001100000b
; ooo.
; ..o.
; .o..
; ....
dw 1110001001000000b
; .o..
; ....
; ooo.
; .o..
dw 0100000011100100b
; .o..
; o.o.
; ..o.
; .o..
dw 0100101000100100b
; .o..
; o.o.
; o.o.
; ....
dw 0100101010100000b
; o...
; ooo.
; ooo.
; ....
dw 1000111011100000b
; .oo.
; o...
; .oo.
; ....
dw 0110100001100000b
; ..o.
; ooo.
; ooo.
; ....
dw 0010111011100000b
; ooo.
; oo..
; ooo.
; ....
dw 1110110011100000b
; ooo.
; oo..
; o...
; ....
dw 1110110010000000b
; o...
; o.o.
; .oo.
; ....
dw 1000101001100000b
; o...
; ooo.
; o.o.
; ....
dw 1000111010100000b
; ....
; .o..
; .o..
; ....
dw 0000010001000000b
; .o..
; .o..
; o...
; ....
dw 0100010010000000b
; o...
; oo..
; o.o.
; ....
dw 1000110010100000b
; ....
; o...
; .oo.
; ....
dw 0000100001100000b
; ....
; ooo.
; ooo.
; ....
dw 0000111011100000b
; ....
; oo..
; o.o.
; ....
dw 0000110010100000b
; .o..
; o.o.
; .o..
; ....
dw 0100101001000000b
; ....
; ooo.
; ooo.
; o...
dw 0000111011101000b
; ....
; ooo.
; ooo.
; ..o.
dw 0000111011100010b
; ....
; .oo.
; o...
; ....
dw 0000011010000000b
; ..o.
; .o..
; oo..
; ....
dw 0010010011000000b
; ....
; ooo.
; .o..
; ....
dw 0000111001000000b
; ....
; o.o.
; .oo.
; ....
dw 0000101001100000b
; ....
; o.o.
; .o..
; ....
dw 0000101001000000b
; o.o.
; ooo.
; ooo.
; ....
dw 1010111011100000b
; o.o.
; .o..
; o.o.
; ....
dw 1010010010100000b
; o.o.
; .o..
; .o..
; ....
dw 1010010001000000b
; ....
; oo..
; .oo.
; ....
dw 0000110001100000b
; ....
; ....
; ....
; ....
.spc.i equ ($-fnt.t)>>fnt.b
.spc dw 0b
; ....
; ....
; .o..
; ....
.prd.i equ ($-fnt.t)>>fnt.b
.prd dw 0000000001000000b
; ....
; .o..
; ....
; ....
.dot.i equ ($-fnt.t)>>fnt.b
.dot dw 0000010000000000b
; .o..
; ....
; ....
dw 0100000000000000b
; .o..
; ....
; .o..
; ....
.cln.i equ ($-fnt.t)>>fnt.b
.cln dw 0100000001000000b
; .o..
; ..o.
; ....
; .o..
dw 0100001000000100b
; ....
; ....
; .o..
; o...
dw 0000000001001000b
; .oo.
; .o..
; .oo.
; ....
dw 0110010001100000b
; o...
; o...
; o...
; ....
dw 1000100010000000b
; oo..
; .o..
; oo..
; ....
dw 1100010011000000b
; ..o.
; ..o.
; ..o.
; ....
dw 0010001000100000b
; ..o.
; .o..
; o...
; ....
dw 0010010010000000b
; ....
; ooo.
; ....
; ....
.mns.i equ ($-fnt.t)>>fnt.b
.mns dw 0000111000000000b
; oooo
; ..oo
; ....
; ....
.gn	dw 1111001100000000b
; o..o
; ...o
; o..o
; oooo
.oj	dw 1001000110011111b
; .o.o
; ...o
; oo..
; oooo
.gm	dw 0101000111001111b
; o.o.
; ....
; ooo.
; .oo.
.gm2	dw 1010000011100110b
; o.o.
; ....
; ooo.
; ooo.
.gm3	dw 1010000011101110b
; .o..
; ooo.
; .o..
; ....
.pls.i equ ($-fnt.t)>>fnt.b
.pls    dw 0100111001000000b
; ooo.
; ....
; ooo.
; ....
.eq.i equ ($-fnt.t)>>fnt.b
.eq    dw 1110000011100000b
.t.c	equ ($-.t)>>.b
;...............................

;times 0200h-($-sector2) db 15
;-----------------------------------------------------------
;sector3: ; 8000h
;
; bmp writer
;
bmp:
.wi	equ 320
.hi	equ 200
align 16
bmp.drw_chset:;bx:fnt,cx:cntr,di:px
	push ax
	push bx
	push cx
	push dx
	.1	mov dx,[bx]
		call bmp.type
		add bx,fnt.s
	loop .1
	pop dx
	pop cx
	pop bx
	pop ax
	ret

align 16
bmp.type:;*dx:4x4x1bmp,al:colr,*di:bmp
	push cx
	mov cx,fnt.hi
	.1	push cx
		mov cx,fnt.wi
		.2	sal dx,1
			jnc .3
			mov [es:di],al
		.3	inc di
		loop .2
		pop cx
		add di,bmp.wi-fnt.wi
	loop .1
	pop cx
	add di,-bmp.wi*fnt.hi+fnt.wi
	ret

align 16
bmp.cls:; clear screen
		; edi=0
	push ecx
	mov ecx,320*200>>2
	xor edi,edi
	rep stosd
	pop ecx
	xor edi,edi
	ret

align 16
bmp.drw_code:; draw code on bmp
	push esi
	push edi
	push ecx
	mov ecx,(end-$$)>>2
	mov esi,$$	
	mov edi,320*200-(end-$$)
	rep movsd
	pop ecx
	pop edi
	pop esi
	ret

align 16
bmp.type_spc:
    add di,fnt.wi
    ret

align 16
bmp.xof  dd 0
bmp.yof  dd 0

align 16
bmp.drw_nl:;*di:px
        push ax
        add di,bmp.wi*(fnt.hi+1)
        mov ax,di
        .1      sub ax,bmp.wi
                cmp ax,bmp.wi
        jg .1
        sub di,ax
        add di,[bmp.xof]
        pop ax
		ret

align 16
bmp.drw_ascii:;bx:fnt,*di:px,*si:asciiz,*dx
	xor dx,dx
.1	push bx
	mov dl,[si]
	cmp dl,0        ; \0
	jne .spc
	pop bx
	ret
.spc	cmp dl,32	; [space]
	jne .mns
	mov dl,fnt.spc.i
	jmp .ix
.mns	cmp dl,45	; -
	jne .cln
	mov dl,fnt.mns.i
	jmp .ix
.cln    cmp dl,58       ; :
    jne .prd
    mov dl,fnt.cln.i
    jmp .ix
.prd	cmp dl,46       ; .
    jne .pls
    mov dl,fnt.prd.i
    jmp .ix
.pls	cmp dl,43       ; +
    jne .eq
    mov dl,fnt.pls.i
    jmp .ix
.eq	cmp dl,61       ; =
    jne .ltr
    mov dl,fnt.eq.i
    jmp .ix
.ltr    cmp dl,97	; a..z
	jl .nbr
	sub dl,87
	jmp .ix
.nbr    sub dl,48	; 0..9
.ix	shl dl,fnt.b
	add bx,dx
.0	mov dx,[bx]
	call bmp.type
	inc si
	pop bx
	jmp .1
.2	pop bx
	ret
	
align 16
bmp.type_byte_hex:; ebx:byte edx:font
    push ebx
	shr ebx,4
	and ebx,0xf
    shl ebx,1
	add ebx,edx
	push edx
	mov edx,[ebx]
	call bmp.type
	pop edx
	pop ebx
	push ebx
	and ebx,0xf
	shl ebx,1
	add ebx,edx
    push edx
	mov edx,[ebx]
	call bmp.type
	pop edx
	pop ebx
    ret
;times 0200h-($-sector3) db 0

align 16
k1s:                              ; 8200h
.loop:
	mov edx,[k0s.tc]
	and edx,111b
	shl edx,2
	add edx,.data.bg
	
	mov eax,[edx]               ;
	mov edi,bmp.wi              ; from row 1
	mov ecx,(bmp.wi*bmp.hi)>>3  ; clear ½ scr dword
	rep stosd                   ; clear half scr
	
	call bmp.drw_code                 ; drw prg
	call k4.drw                 ; drw windows
		
	mov al,1eh
	mov bx,fnt.t
	mov cx,fnt.t.c
	mov di,(bmp.wi*3+bmp.wi-fnt.wi*fnt.t.c-(fnt.wi<<2)+1)
	call bmp.drw_chset

	mov bx,fnt.t
	mov di,bmp.wi*11+11
    mov dword[bmp.xof],11
	mov esi,[k1s.data.chp]
	mov edx,[k1s.data.dchp]
    add [k1s.data.chp],edx
;    mov eax,esi
	mov cx,17
	.1    call bmp.drw_ascii
	      call bmp.drw_nl
	      add eax,[k1s.data.dcolr]
          inc esi
    loop .1
    
;    inc dword[9000h]
;    inc dword[pet]
;    inc dword[pdt]
;    inc dword[0a000h]
	inc dword[k0s.fc]
	hlt
;	jmp $
;	call .loop                   ; stack train bug	
	jmp .loop
;...............................
;txt:
align 4
ne2000 db "ne2000 compatible ethernet",0
k1s.data:
.bg	dd 0x11000000
	dd 0x00110000
	dd 0x00001100
	dd 0x00000011
	dd 0x11000000
	dd 0x00110000
	dd 0x00001100
	dd 0x00000011
.chp dd txt
.dchp dd 1
.dcolr dd 1
align 4
win:
.x	equ	0
.y	equ 4
.w	equ 8
.h	equ 12
.cdrw	equ 16	; override
.cini	equ 20	; functions
.ikey	equ 24	; 
.itmr	equ 28	;
.s	equ 32
.b	equ 5
.t:        ; x ; y ; w ; h ; cdrw     ; ikbd     ;          ;          
.t.0	dd 002,003,315,088,0x00000000,0x00000000,0x00000000,0x00000000
.t.1	dd 003,003,223,088,0x00000000,0x00000000,0x00000000,0x00000000
.t.2	dd 254,003,040,008,0x00000000,0x00000000,0x00000000,0x00000000
.t.3	dd 254,011,040,008,0x00000000,0x00000000,0x00000000,0x00000000
.t.4	dd 254,019,040,008,0x00000000,0x00000000,0x00000000,0x00000000
.t.5	dd 254,027,040,008,0x00000000,0x00000000,0x00000000,0x00000000
.t.6	dd 254,035,040,008,0x00000000,0x00000000,0x00000000,0x00000000
.t.7	dd 254,043,040,008,0x00000000,0x00000000,0x00000000,0x00000000
.t.8	dd 243,072,032,020,win.2.drw ,0x00000000,0x00000000,0x00000000
.t.c	equ ($-.t)>>.b
;...............................
win.2.drw:
;	mov dword[gs:0],0x04040404
	mov bx,0x0002
;	mov edx,bmp.wi
;	mov eax,[esi+win.y]
;	mul edx
;	add eax,[esi+win.x]
;	mov edi,eax
;	mov [es:edi],bl
	call win.drw
	mov byte[es:edi],15
	ret
win.2.itmr:
;	mov dword[gs:12],0xeefeeffe
	ret
;...............................
k4.drw:; redraw windows
	pushad
	mov esi,win.t
	mov ecx,win.t.c
	mov ebx,0x01020304
.3	pushad
	mov eax,[esi+win.cdrw]
	cmp eax,0
	jz .4
	call eax
	jmp .5
.4	call win.drw
.5	popad
	add esi,win.s
	loop .3
	popad
	ret
;...............................
;	times 0400h-($-sector3) db 2
;...............................
k2s:                             ; 8400h
;...............................
;...............................
win.drw2:; esi:win bl:col
	mov edx,bmp.wi
	mov eax,[esi+win.y]
	mul edx
	add eax,[esi+win.x]
	mov edi,eax
	mov [es:edi],bl

	add edi,[esi+win.w]
	mov [es:edi],bl

	mov edx,bmp.wi
	mov eax,[esi+win.h]
	mul edx
	add edi,eax
	mov [es:edi],bl

	sub edi,[esi+win.w]
	mov [es:edi],bl	
	ret
	
	push ecx
;	mov dword[es:0],0x03030303
	mov ecx,[esi+win.w]
.1	mov [es:edi],bl
	inc edi
;	dec ecx
;	jnz .1
	loop .1
;	rep stosb
	mov cx,[bx+win.h]
.2	mov [es:edi],si
	add edi,320
	loop .2
	mov ecx,[ebx+win.w]
.3	mov [es:edi],si
	dec edi
	loop .3
	mov ecx,[ebx+win.h]
.4	mov [es:edi],si
	sub edi,320
	loop .4
	pop ecx
	
	pop edi	
	ret

align 16
win.drw:		; ro esi:win ebx:colr
            ; wr edx,eax,edi
	mov edx,bmp.wi
	mov eax,[esi+win.y]
	mul edx
	add eax,[esi+win.x]
	mov edi,eax
	mov [es:edi],bl

	mov ecx,[esi+win.w]
.1	mov [es:edi],bl
	add	edi,1
	loop .1

	mov ecx,[esi+win.h]
.2	add	edi,bmp.wi
	mov [es:edi],bx
	loop .2

	mov ecx,[esi+win.w]
.3	mov [es:edi],bl
	sub edi,1
	loop .3

	mov ecx,[esi+win.h]
.4	mov [es:edi],bx
	sub	edi,bmp.wi
	loop .4
	ret
;...............................
win.drw3:; esi:win ebx:col
	mov edx,bmp.wi
	mov eax,[esi+win.y]
	mul edx
	add eax,[esi+win.x]
	mov edi,eax
	mov [edi],bl

	push ecx

	mov ecx,[esi+win.w]
.1	mov [es:edi],bl
	add	edi,1
	loop .1

	mov ecx,[esi+win.h]
.2	mov [es:edi],bl
	add	edi,bmp.wi
	loop .2

	mov ecx,[esi+win.w]
.3	mov [es:edi],bl
	sub edi,1
	loop .3

	mov ecx,[esi+win.h]
.4	mov [es:edi],bl
	sub	edi,bmp.wi
	loop .4

	pop ecx
	ret
;txt
sh.init:ret
sh.tick:	;ebx=gpra
.0	mov esi,[ebx+128*4]
	mov edx,esi
	and edx,0x0fffffff
	mov edx,[edx+ebx+129*4]
	mov eax,edx
	and eax,3
	jz .1
		cmp eax,3
		jne .2
			xor eax,eax
		.2 cmp eax,[ebx+130*4]
		je .1
		inc esi
		mov [ebx+128*4],esi
		jmp .0
	.1 shr edx,2
	mov eax,edx
	and eax,3
	jz .3;if(z|n|p)
	    cmp eax,3
	    jne .5;ldo
	    	shr edx,2
	    	mov [ebx],edx		
			inc esi
			mov [ebx+128*4],esi
			jmp .0
		.5:
		cmp eax,1
		jne .6;cal
		.6:
		cmp eax,2
		jne .7;ret
		.7:
	.3:;op
	shr edx,2
	mov eax,edx
	and eax,0xf
.f0	jnz .f1
	
.f1	cmp eax,1
	jne .f2

.f2	cmp eax,2
	jne .f3

.f3	cmp eax,3
	jne .f4

.f4	ret


;txt
db "                                                                         ",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0                          
db "osmo. view page 0 - frumusetile anapoda       741  ",0
db 0,0,0,0,0,0,0,0,0,0,0,"                         ",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "hello","                                ",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "        osmo says u c eye-in-the-sky system supervisor",0,0,0
db "  1k digital pins to sensors  switches  rams  mics  cams",0,0
db "  vga output 10hz  320 x 200 x 8 bmp",0,0
db "  2 earphones outputs",0,0
db "  2 mic inputs",0,0
db "  serial wirez to many terra storage device",0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,"                        ",0
db "[ extract from eye-in-the-sky log 2551 ]",0,0,0
db "u c a 32 bit sticky ticky clicky windows device",0
db " in a ciggy pack sized plastic container",0
db " it has a four key keyboard",0
db "  thumb holder",0
db "  retina reader",0
db "  serial wirez socket",0
db "  paletted light absorbing display",0
db "  earphones output",0
db " and power input",0,0
db ": take",0,0
db "u grab the device",0
db " u notice that holding it",0
db " changes the display to a faint but readable surface for the left thumb to hold",0,0,0,0,0,0
db ": look",0,0
db "                 ",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;txt
db "u c an 8000 year old shain loop device      ",0,0
db "   0          8        16       24       32",0
db "   :zn cr     :    :    :        :        :",0
db "   :--:--:----:----:----:--------:--------:",0
db "cal:..:10:....:....:....:........:........:",0
db "ret:..:01:....:....:....:........:........:",0
db "jif:..:11:....:....:....:........:........:",0
db "ldo:00:11:....:....:....:........:........:",0
db "   :..:0.:pixl:niah:shif:w......d: ......a:",0
db "   :--:--:----:----:----:--------:--------:",0
db "      loop imm nx load                     ",0
db "              not inc add hlf              ",0
db "                hilo store incas flgzn wbr ",0
db 0,0
db "osmo. view my machine",0,0
db 0,0,0,0,0,0,0,0,0,0,0,0
times 0800h-($-k2s) db 3	
;...............................; 8c00h
k4s:
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "u c a 32b risc     256m instructions   4g data   128 registers -",0
db "    0          8       16       24       32                    -",0
db "    :zn:cr:    :        :        :        :                    -",0
db "    :--:--:----:--------:--------:--------:                    -",0
db " cal:..:10:....:........:........:........:                    -",0
db " ret:..:01:....:........:........:........:                    -",0
db " jif:..:11:....:........:........:........:                    -",0
db " ldi:00:11:....:........:........:........:                    -",0
db "    :..:00:xxxx: ......d: ......a: ......b:                    -",0
db "                                                               -",0
db "xxxx 00  01  10  11     yyyy 00  01  10  11                    -",0
db "  00 is  add sub shf      00 ... ... ... ...                   -",0
db "  01 ld  st  ldc stc      01 ... ... ... ...                   -",0
db "  10 and or  xor not      10 ... ... ... ...                   -",0
db "  11 lp  ... lpi fwd      11 ... ... ... ...                   -",0
db "                                                               -",0
db "osmo. view my other device              rabi-22t               -",0,0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "                 figure 3. read-only file system",0,0
db ": size : field : comment                        :",0
db ":------:-------:--------------------------------:",0
db ":    1 : bits  : 8b                             :",0
db ":    1 : byte  : 8b                             :",0
db ":    2 : word  : 16b                            :",0
db ":    4 : dword : 32b                            :",0
db ":    8 : quad  : 64b                            :",0
db ":   16 : label : 16 bytes                       :",0
db ": -----:-------:--------------------------------:",0
db ":   32 :       : baba struct                    :",0
db ":    n : data  : binary                         :",0
db ":------:-------:--------------------------------:",0
db 0,0,0,0,0,0,0,0,0,0,0,0,0
db "my fgpa does triangles on symetric bitmap",0,0
db "divison table 64k",0,0
;txt
db "list of words used in this machine less than 4k",0,0
db "coherent text displayed until stopped",0,0
db "each word used once  ",0,0
db 0,0,0,0,0,0,0,0,0,0,0,0," ",0,"  ",0,"   ",0,"       ",0
db "the only way i would want my machine to interface",0,0
db "baba structs bebe operating system of alternating displays",0,0
db "in a complete self explanatory computational machinery",0,0
db "of a coherent instrument to store  search and display a 64b data space",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "can                        ",0
db "play stereo music through headphones",0
db "display pictures in fancy transitions",0
db "record sound",0
db "take 3d photos",0
db "recreate user activity",0
db "make meaningfull display of the state of machinery",0
db "compressed input through optimal state diagram",0
db "sequenced timed output of parallell tracks",0
db "superscalar pipelined processing of reduced instruction set",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "has         ",0
db "instruction pointer",0
db "1k bits accumulator for huge stuff dispersed in 128 64 bit registers",0
db "32 instructions with immediate access to 256m 32b code space and 4g 64 bit data",0
db "8 loops construction",0
db "32 x 32b cal-msr stack",0
db "decoder of what is to be displayed 4k",0
db "fixed size processing frame 64k",0
db "alternating color displays 10hz",0
db "64k x 64k sticky memory",0
db "16 pin human interaction port",0
db "4t-disk-device",0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
db "                                 ",0
db "figure 1. instruction set",0
db 0,0,0
times 1000h-($-k4s) db 1
k8:
db "              0    4  6  8    12   16      24      32",0
db " pc   000:0000:zncr:li:nl:niah:sfia:.......d.......a:",0
db "  rom         :                                     :",0
db "   ir         : cal        ret                      :",0
db "    imm a     :  push pcf   pop pcf                 :",0
db "       ram    :   pc imm28    ...                   :",0
db "      not     :     ...       ...                   :",0
db "  d  inc      :-------------------------------------:",0
db "    add       : loop                nx              :",0
db "   hlf        :    lc a or imm26     lc dec         :",0
db "  ram         :    lpa pc             pc lpa        :",0
db "  fzn         :      ...               ...   pc inc :",0
db " inca         :      ...   a inc       ...     ...  :",0
db "wbd wba       :wbd   ...    wba        ...     ...  :",0
db " ...          : ...  ...     ...       ...     ...  :",0
db "osmo. view my notinca                     25511117   ",0  
db "     :zn:cr:    :        :        :        :",0
db "     :--:--:----:--------:--------:--------:",0
;txt
db "  cal:..:10:....:........:........:........:",0
db "  ret:..:01:....:........:........:........:",0 
db "  jif:..:11:....:........:........:........:",0
db "  lda:00:11:....:........:........:........:",0
db "  fwd:00:00:0011:........:........:........:",0
db " loop:00:00:0110:........:........:........:",0
db "loopi:00:00:0111:........:........:........:",0
db "     :--:--:----:--------:--------:--------:",0
db "  add:..:0.:0010:d...... :a...... :b...... :",0
db "  not:..:0.:0001:....... :....... :....... :",0
db "  shf:..:0.:0100:....... :....... :....... :",0
db "   ld:..:0.:11c0:....... :....... :....... :",0
db "   st:..:0.:11c1:....... :....... :....... :",0
db "     :..:0.:1000:....... :....... :....... :",0
db "     :..:0.:1001:....... :....... :....... :",0
db "     :..:0.:1010:....... :....... :....... :",0
db "     :..:0.:1011:....... :....... :....... :",0
db "     :..:0.:0000:....... :....... :....... :",0
db "                             keyword junza  ",0
db 0
;txt
db ":..:oo:....:.:...:o.o.:........:........:0 ldf 00a0000h       :",0                          
db ":..:..:o...:.:...:....:........:.oo..o..:1 lpi 00064h         :",0                          
db ":..:..:o...:.:...:....:.......o:.o......:2   lpi 00140h       :",0                          
db ":..:..:.o..:.:...:..oo:....oooo:....oo.o:3     f stc d        :",0                          
db ":..:..:..o.:o:...:o...:....oo.o:.......o:4     d add 1   nx   :",0                          
db ":.o:.o:....:o:...:....:....oo.o:........:5   d is 0  nx  ret  :",0
db ":..:..:o...:.:...:....:........:.....o..:6 lpi 00004h         :",0                          
db ":..:..:o...:.:...:....:........:.....o..:7   lpi 00004h       :",0                          
db ":..:..:.o..:.:...:.o.o:....oooo:.......o:8     c shr 1        :",0                          
db ":.o:..:..o.:.:...:o...:....oo.o:.......o:9     ifn f stc a    :",0                          
db ":oo:..:..o.:o:...:o...:....oo.o:.......o:a     ifp f add 1  nx:",0                          
db ":..:..:..o.:o:...:o...:....oo.o:.......o:b   d add b   nx     :",0                          
db ":..:.o:....:.:...:....:........:........:c d add c   ret      :",0
db 0,0,0,0,0,0,0,0,0                          
db "osmo. view my half k",0,0
;txt
db ":..:oo:o.o.:....:....:000  ldo a00h         00  01  10  11     :",0                          
db ":..:..:...o:....:o...:001  o shl 8       00  =  shl ldc stc    :",0                          
db ":..:..:....:..o.:....:002  2 is o        01 xor shr nx loop    :",0                          
db ":..:oo:....:.oo.:.o..:003  ldo 064h      10 add not or  and    :",0                          
db ":..:..:....:..oo:....:004  3 is o        11 sub cmp fwd        :",0                          
db ":..:oo:...o:.o..:....:005  ldo is 140h                         :",0                          
db ":..:..:....:.o..:....:006  4 is o                              :",0                          
db ":..:..:.ooo:..oo:....:007  loop  3                             :",0                          
db ":..:..:.o..:oo.o:oo.o:008    d xor d                           :",0                          
db ":..:..:.ooo:.o..:....:009    loop 4                            :",0                          
db ":..:..:..oo:..o.:oo.o:00a      2 stc d                         :",0                          
db ":..:..:o...:oo.o:...o:00b      d add 1                         :",0                          
db ":..:..:.oo.:....:....:00c    nx                                :",0
db ":..:.o:.oo.:....:....:00d  nx   ret 000h                       :",0
db ":..:..:....:....:....:00e                                      :",0
db ":..:..:....:....:....:00f                                      :",0
db "osmo. view my k",0,0
;txt
db ":..:oo:o.o.:....:....:010  ldo a00h                            :",0                          
db ":..:..:...o:....:o...:011  o shl 8                             :",0                          
db ":..:..:....:oo.o:....:012  d is o                              :",0                          
db ":..:oo:....:....:.o..:013  ldo 004h                            :",0                          
db ":..:..:.ooo:....:....:014  loop o                              :",0                          
db ":..:..:.ooo:....:....:015    loop o                            :",0                          
db ":..:..:.o.o:oo..:...o:016      c shr 1                         :",0                          
db ":.o:..:..oo:oo.o:o.o.:017      ifn d stc a                     :",0                          
db ":o.:..:o...:oo.o:...o:018      ifz d add 1                     :",0                          
db ":..:..:.oo.:....:....:019    nx                                :",0                          
db ":..:..:o...:oo.o:ooo.:01a    d add e                           :",0                          
db ":..:..:.oo.:....:....:01b  nx                                  :",0                          
db ":..:.o:o...:oo.o:o.oo:01c  d add b   ret                       :",0
db ":..:..:....:....:....:01d                                      :",0
db ":..:..:....:....:....:01e                                      :",0
db ":..:..:....:....:....:01f                                      :",0
db "osmo. view another k",0,0
times 2000h-($-k8) db 1
k16:
db 0,0,0,0,0,0,0,"                                                        "
db "figure 2. memory map",0,0
db ":   from :    to : area                           :",0
db ":--------:-------:--------------------------------:",0
db ":   0000 :  0400 : interrupt descriptor table     :",0
db ":   1000 :  1004 : page directory table           :",0
db ":   2000 :  2400 : page entry table               :",0
db ":   7c00 :  7e00 : boot sector                    :",0
db ":   7e00 :  8000 : character set                  :",0
db ":   8000 :  8400 : loop                           :",0
db ":  10000 : a0000 : free                           :",0
db ":  a0000 : e0000 : vga                            :",0
db ": 100000 :       : unmapped                       :",0
db ":--------:-------:--------------------------------:",0
db 0,0,0,0,0,0
db "my-inventory digital ark",0
db "roome text adventure game",0
db "stereo photo taker and displayer goggles",0
db "clip-book-digital public domain server for screen snapshots",0
db 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
stk:
times 1000h db 0
stke:
times 4000h-($-k16) db 4
end:
;k32
;times 8000h-($-k32) db 4
; figure 2 ascii table
; 32: 
; 33:!
; 34:"
; 35:#
; 36:$
; 37:%
; 38:&
; 39:'
; 40:(
; 41:)
; 42:*
; 43:+
; 44:,
; 45:-
; 46:.
; 47:/
; 48:0
; 49:1
; 50:2
; 51:3
; 52:4
; 53:5
; 54:6
; 55:7
; 56:8
; 57:9
; 58::
; 59:;
; 60:<
; 61:=
; 62:>
; 63:?
; 64:@
; 65:A 01000001
; 66:B
; 67:C
; 68:D
; 69:E
; 70:F
; 71:G
; 72:H
; 73:I
; 74:J
; 75:K
; 76:L
; 77:M
; 78:N
; 79:O
; 80:P
; 81:Q
; 82:R
; 83:S
; 84:T
; 85:U
; 86:V
; 87:W
; 88:X
; 89:Y
; 90:Z
; 91:[
; 92:\
; 93:]
; 94:^
; 95:_
; 96:`
; 97:a 011000001 xor al,00100000b
; 98:b
; 99:c
; 100:d
; 101:e
; 102:f
; 103:g
; 104:h
; 105:i
; 106:j
; 107:k
; 108:l
; 109:m
; 110:n
; 111:o
; 112:p
; 113:q
; 114:r
; 115:s
; 116:t
; 117:u
; 118:v
; 119:w
; 120:x
; 121:y
; 122:z
; 123:{
; 124:|
; 125:}
; 126:~
; 127:
;...............................
