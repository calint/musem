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
asm(".code16");
asm(".global osca_key");
asm(".global osca_t");
asm(".global osca_t1");
asm(".global _start");
asm("_start:");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("xor %bx,%bx");
asm("movw %bx,%ds");
asm("movb %dl,(osca_drv_b)");//save boot drive
asm("movw %bx,%ss");
asm("movw $_start,%sp");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("movw $(0x0200+LOAD_SECTORS),%ax");
asm("movw $0x0002,%cx");//from sector 2
asm("movw $0x07e0,%bx");//to 0x7e00
asm("movw %bx,%es");
asm("xor %bx,%bx");
asm("int $0x13");
//asm("jnc 1f");
//asm("  movw $0xb800,%ax");//console segment
//asm("  mov %ax,%fs");
//asm("  movw $0xffff,%fs:0");//top left corner
//asm("  2:cli");//hlt
//asm("    hlt");
//asm("    jmp 2b");
//asm("1:");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("mov $0x13,%ax");//vga mode 320x200x8 bmp @ 0xa0000
asm("int $0x10");
asm("mov $0xa000,%ax");
asm("mov %ax,%gs");//gs to vgabmp
asm("mov %ax,%es");//es
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -memcpy
//asm("movw $0x0404,%gs:0x100");
//asm("cld");
asm("mov $0xa000,%ax");
asm("mov %ax,%es");
asm("mov $0x8000,%di");
asm("mov $0x7c00,%si");
asm("mov $PROG_SIZE>>1,%cx");
asm("rep movsw");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
//asm("movw $0x0404,%gs:0x104");
asm("in $0x92,%al");// enable a20 line (odd megs)
asm("or $2,%al");
asm("out %al,$0x92");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -32b
//asm("movw $0x0404,%gs:0x108");
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
//asm("movw $0x0404,0xa0110");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -isr
//asm("movw $0x0404,0xa0114");
//asm("cli");
asm("movl $IDT,%ebx");//idt address
asm("movl $0x0040,%ecx");//interrupt count
asm("1:");
asm("    movw $isr_err,   (%ebx)");//offset 0..15
asm("    movw $0x0008,2(%ebx)");//selector in gdt
asm("    movb $0x00,  4(%ebx)");//unused
asm("    movb $0x8e,  5(%ebx)");//type_attrs p,pv0,!s,i32b
asm("    movw $0x0000,6(%ebx)");//offfset 16..31
asm("    add $8,%bx");
asm("loop 1b");
asm("movl $0x0e0e0f0f,0xa0118");
asm("movw $isr_tck,(IDT+0x40)");
asm("movw $isr_kbd,(IDT+0x48)");
asm("lidt idtr");
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -start
//asm("movw $0x0404,0xa4000");// dot in middle of vga buffer
asm("movl (osca_tsk_a),%ebx");//ebx points to active task record
asm("movl 4(%ebx),%esp");//restore esp
asm("sti");
asm("jmp *(%ebx)");//jmp to restored eip
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
asm("osca_tsk_a:.long tsk");
asm("isr_tck_eax:.long 0x00000000");
asm("isr_tck_ebx:.long 0x00000000");
asm("isr_tck_esp:.long 0x00000000");
asm("isr_tck_eip:.long 0x00000000");
asm(".align 16");
asm("isr_tck:");
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
asm("  add $16,%esp");
asm("  popal");
asm("  mov (isr_tck_esp),%esp");//restore esp
asm("  push %ax");
asm("  mov $0x20,%al");
asm("  out %al,$0x20");//ack irq
asm("  pop %ax");
asm("  sti");//enable irq
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
asm("mov $0x0301,%ax");//save 2nd sector
asm("mov $0x0002,%cx");//
asm("mov $0x07e0,%bx");//
asm("mov %bx,%es");
asm("xor %bx,%bx");
asm("mov (osca_drv_b),%dl");
asm("xor %dh,%dh");
asm("int $0x13");
asm("jc 8f");
//! dot write-ack
//-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
asm("8:cli");
asm("  hlt");
asm("  jmp 8b");
asm("page0:");
asm(".align 0x400");
asm(".space 0x1000,1");
//asm("mov $0x4f02,%ax");//vesa mode
//asm("mov $0x410f,%bx");// 320x200x24 bmp
//asm("mov $0x8100,%bx");// 640x400x256 graphics
//asm("mov $0x8112,%bx");// 640x480x16.8M
//asm("mov $0x8115,%bx");// 800x600x16.8M
//asm("mov $0x8118,%bx");// 1024x768x16.8M
//AX = 4F01h; ES:DI = pointer to 256 byte buffer;
//CX = mode number
//INT 10h
//hx
//00	ModeAttributes	WORD	bit 7 (v2.0+) Set if linear framebuffer mode supported
//28	PhysBasePtr	DWORD	(v2.0+) Physical address of linear framebuffer
//2C	OffScreenMemOffset	DWORD	(v2.0+) Offset from start of frame buffer to first application usable video memory which is not normally visible. It is possible that there will be offsets between normal onscreen memory and this field value which should not be altered.
//30	OffScreenMemSize	WORD	(v2.0+) Number of kilobytes of application usable offscreen memory.

/*
  00000-003FF  IVT (Interrupt Vector Table)
  00400-005FF  BDA (BIOS Data Area)
  00600-9FFFF  Ordinary application RAM
  A0000-BFFFF  Video memory
  C0000-EFFFF  Optional ROMs (The VGA ROM is usually located at C0000)
  F0000-FFFFF  BIOS ROM
*/
