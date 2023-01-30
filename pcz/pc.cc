//tested on:
// * dell inspiron 1545
// * asus-eeepc-4g
// * hp-compaq-mini-110
// * sony-vaio-vgnfw11m
// * qemu 0.11.0 on linux 2.6
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".set IDT,0x600");//interrupt descriptor table address
asm(".set LOAD_SECTORS,0x1f");//15½K
asm(".set PROG_SIZE,0x200+0x1f*0x200");
asm(".global osca_key");
asm(".global osca_t");
asm(".global osca_t1");
asm(".global _start");
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
asm("movw $0x0404,%es:0");// red dot
asm("movw $0x0303,%es:4");// cyan dot
asm("movw $0x0404,%es:8");
asm("movw $0x0505,%es:0xc");
asm("movw $0x0606,%es:0x10");
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
asm("jnc 1f");// if error display 'E' and hang
asm("  movw $0x0445,%fs:2");// E
asm("  2:cli");// hang
asm("    hlt");
asm("    jmp 2b");
asm("1:");
asm("cmp $0x1f,%al");// check if all specified sectors have been read
asm("jz 3f");
asm("  movw $0x0445,%fs:4");// E
asm("  4:cli");// hang
asm("    hlt");
asm("    jmp 4b");
asm("3:");
asm("movw $0x0441,%fs:6");// A
*/


asm("mov $0,%ah");// wait for key press
asm("int $0x16");

//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("mov $0x13,%ax");//vga mode 320x200x8 bmp @ 0xa0000
asm("int $0x10");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -memcpy
asm("mov $0xa000,%ax");// destination es:di
asm("mov %ax,%es");
asm("mov %ax,%gs");
asm("mov $0x8000,%di");
// source ds:si  (ds is 0h)
asm("mov $0x7c00,%si");
asm("mov $0x4000,%cx");// 16KB
asm("rep movsb");
asm("  4:cli");// hang
asm("    hlt");
asm("    jmp 4b");

//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("movw $0x0202,%gs:0x1c");// green bar
asm("in $0x92,%al");// enable a20 line (odd megs)
asm("or $2,%al");
asm("out %al,$0x92");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -32b
asm("movw $0x0202,%gs:0x10");// green bar

asm("lgdt gdtr");// load global descriptor tables
asm("mov %cr0,%eax");// enter 32b protected mode
asm("or $0x1,%al");
asm("mov %eax,%cr0");
asm("jmp $8,$pm");// jmp to flush
asm(".align 16,0x00");
asm("gdt:.quad 0x0000000000000000");//0x00:
asm("    .quad 0x00cf9a000000ffff");//0x08: 32b code 4g pl0 rx
asm("	 .quad 0x00cf92000000ffff");//0x10: 32b data 4g pl0 rw
asm("    .quad 0x009f9a000000ffff");//0x18: 16b code 1m rx
asm("    .quad 0x009f92000000ffff");//0x20: 16b data 1m rw
asm("gdtr:.word gdtr-gdt-1,gdt,0,0");
asm("ivtr:.word 0x03ff");
asm("     .long 0x00000000");
asm(".align 8,0");
asm("idtr:.word 0x03ff");
asm("     .long IDT");// idt address
asm(".align 8,0");
asm("pm:");
asm(".code32");
asm("movw $0x10,%ax");
asm("mov %ax,%ss");
asm("mov %ax,%ds");
asm("mov %ax,%es");
asm("mov %ax,%fs");
asm("mov %ax,%gs");
asm("xor %eax,%eax");
asm("xor %ebx,%ebx");
asm("xor %ecx,%ecx");
asm("xor %edx,%edx");
asm("xor %edi,%edi");
asm("xor %esi,%esi");
asm("xor %ebp,%ebp");

//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -isr
asm("movw $0x0202,0xa0014");// green bar

//asm("cli");// ? disable interrupts before setup of interrupts?
asm("movl $IDT,%ebx");//idt address
asm("movl $0x0040,%ecx");//interrupt count
asm("1:");
asm("    movw $isr_err,(%ebx)");//offset 0..15
asm("    movw $0x0008,2(%ebx)");//selector in gdt
asm("    movb $0x00,  4(%ebx)");//unused
asm("    movb $0x8e,  5(%ebx)");//type_attrs p,pv0,!s,i32b
asm("    movw $0x0000,6(%ebx)");//offfset 16..31
asm("    add $8,%bx");
asm("loop 1b");
asm("movl $0x0f0f0f0f,0xa0018");// white bar
asm("movw $isr_tck,(IDT+0x40)");
asm("movw $isr_kbd,(IDT+0x48)");
asm("lidt idtr");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -start
asm("movl $0x02020202,0xa001c");// green bar

///////////////////////////////////////////////////////////////////////
asm("99:cli");// hang
asm("  hlt");
asm("  jmp 99b");
///////////////////////////////////////////////////////////////////////

asm("movl (osca_tsk_a),%ebx");// active task eip to ebx (initially first task)
asm("movl 4(%ebx),%esp");// restore stack pointer esp
asm("sti");// enable interrupts and jump to task. racing?
asm("jmp *(%ebx)");// jmp to restored eip. are registers initiated to first task?
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".align 16");
asm("isr_err:cli");
asm("  incw 0xa0000");
asm("  jmp isr_err");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".align 16");
asm("isr_kbd:");
asm("  pushw %ax");
asm("  inb $0x60,%al");//read keyboard port
asm("  movb %al,osca_key");//
asm("  movb %al,0xa0100");
asm("  pushal");//save register
asm("  call osca_keyb_ev");//call device keyb function ev
asm("  popal");//restore register
asm("  movb $0x20,%al");//ack interrupt
asm("  outb %al,$0x20");//
asm("  popw %ax");
asm("  iret");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".align 16");
asm("osca_drv_b:.byte 0x00");//boot drive
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".space _start+436-.,0");//reserved
asm(".space 10,0");//partition table
asm(".space 16,0");// #1
asm(".space 16,0");// #2
asm(".space 16,0");// #3
asm(".space 16,0");// #4
asm(".word 0xaa55");//pc boot sector signature
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("sector2:");//0x7e00 (saved at shutdown)
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("osca_t:.long 0x00000000");
asm("osca_t1:.long 0x00000000");
asm("osca_key:.long 0x00000000");
asm(".space sector2+512-.");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("sector3:");//0x8000 tasks switcher
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("osca_tsk_a:.long tsk");// current task pointer
asm("isr_tck_eax:.long 0x00000000");// used when switching task in 
asm("isr_tck_ebx:.long 0x00000000");//   isr_tick to temporarily
asm("isr_tck_esp:.long 0x00000000");//   save registers
asm("isr_tck_eip:.long 0x00000000");//   |
asm(".align 16");
asm("isr_tck:");// called by timer interrupt to switch task
asm("  movw $0x0e0e,0xa0200");
asm("  mov %eax,(isr_tck_eax)");//save eax,ebx
asm("  mov %ebx,(isr_tck_ebx)");
asm("  incl (osca_t)");//increase 64b ticker
asm("  adcl $0,(osca_t1)");
asm("  movl (osca_t),%eax");//on screen
asm("  movl %eax,0xa0130");
asm("  mov (osca_tsk_a),%ebx");//ebx points to active task
asm("  mov (%esp),%eax");//get eip before irq from stack
asm("  mov %eax,(%ebx)");//save to task.eip
asm("  mov 8(%esp),%eax");//get eflags from stack
asm("  mov %eax,8(%ebx)");//save to task.eflags
asm("  mov %esp,%eax");//adjust esp
asm("  add $12,%eax");//eip,cs,eflags
asm("  mov %eax,4(%ebx)");//save to task.esp
asm("  mov %ebx,%esp");//save gprs
asm("  add $48,%esp");//move to end of task record
asm("  pushal");//pushes eax,ecx,edx,ebx,esp0,ebp,esi,edi
asm("  mov (isr_tck_eax),%eax");//save proper eax,ebx
asm("  mov %eax,44(%ebx)");//task.eax
asm("  mov (isr_tck_ebx),%eax");
asm("  mov %eax,32(%ebx)");//task.ebx
asm("  add $48,%ebx");//next task
asm("  cmp $tsk_eot,%ebx");//if last
asm("  jl 7f");
asm("    mov $tsk,%ebx");//roll
asm("  7:");
asm("  mov %ebx,(osca_tsk_a)");//save ptr to task
asm("  mov 4(%ebx),%esp");//restore esp
asm("  mov %esp,(isr_tck_esp)");//save
asm("  mov (%ebx),%esp");//restore eip
asm("  mov %esp,(isr_tck_eip)");//save
asm("  mov %ebx,%esp");//restore gprs
asm("  add $16,%esp");//move stack pointer to point at edi
asm("  popal");//store poped registers in task table
asm("  mov (isr_tck_esp),%esp");//restore esp
asm("  push %ax");//ack irq
asm("  mov $0x20,%al");
asm("  out %al,$0x20");
asm("  pop %ax");
asm("  sti");//enable irq and jmp. racing?
asm("  jmp *isr_tck_eip");//jmp to restored eip
asm(".space sector3+512-.");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("sector4:");//0x8200 tasks state table
asm("tsk:");// eip,  esp,       eflags,     bits,       edi        esi        ebp        esp0       ebx        edx        ecx        eax
asm("  .long tsk0,0x000afa00,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk1,0x000af780,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk2,0x000af500,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk3,0x000af280,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk4,0x000af000,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk5,0x000aed80,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk6,0x000aeb00,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk7,0x000ae880,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");

asm("  .long tsk8,0x000ae600,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000ae380,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000ae100,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000ade80,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000adc00,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000ad980,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000ad700,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000ad480,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");

asm("  .long tsk9,0x000ad200,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk10,0x000acf80,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000acd00,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("  .long tsk8,0x000aca80,0x00000000,0x00000000, 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x00000000");
asm("tsk_eot:");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm(".align 16,0");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("mode16:");// 16b mode from protected mode
asm(".code16");
asm("mov $0x20,%ax");
asm("mov %ax,%ds");
asm("mov %ax,%ss");
asm("mov $0x7c00,%sp");
asm("lidt ivtr");
asm("mov %cr0,%eax");
asm("and $0xfe,%al");
asm("mov %eax,%cr0");
asm("jmp $0x0,$rm");
asm(".align 16");
asm("rm:");
asm("xor %ax,%ax");
asm("mov %ax,%ds");
asm("mov %ax,%ss");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
//asm("mov $0x0301,%ax");//save 2nd sector
//asm("mov $0x0002,%cx");//
//asm("mov $0x07e0,%bx");//
//asm("mov %bx,%es");
//asm("xor %bx,%bx");
//asm("mov (osca_drv_b),%dl");
//asm("xor %dh,%dh");
//asm("int $0x13");
//asm("jc 8f");
//! dot write-ack
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("8:cli");
asm("  hlt");
asm("  jmp 8b");
asm("page0:");
asm(".align 0x400");
asm(".space 0x1000,1");

/*
  00000-003FF  IVT (Interrupt Vector Table)
  00400-005FF  BDA (BIOS Data Area)
  00600-9FFFF  Ordinary application RAM
  A0000-BFFFF  Video memory
  C0000-EFFFF  Optional ROMs (The VGA ROM is usually located at C0000)
  F0000-FFFFF  BIOS ROM
*/
