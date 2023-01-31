#include "file.h"

extern "C" void pz_memcpy(Addr addr,Addr to,Size size){
	// ? works with aligned mem, call inline assembler
	int c=size>>2;
	int*s=(int*)addr;
	int*d=(int*)to;
	while(c--)
		*d++=*s++;
//	asm("movl addr,%esi;movl to,%edi;movl size,%ecx;rep movsl;"::"S"(addr),"D"(to),"C"(size):"%esi","%edi","%ecx");
//	asm("rep;movsl;"::"S"(addr),"D"(to),"c"(size):);
}
/*
kcp     push ebp
		mov ebp,esp
		push esi
		push edi
		push ecx
		mov esi,[ebp+8]
		mov edi,[ebp+12]
		mov ecx,[ebp+16]
		rep movsd
		pop ecx
		pop edi
		pop esi
		mov esp,ebp
		pop ebp
        ret

extern "C" void kcp(int*src,int*dst,int dwords);
*/


void File::to(File file){
	pz_memcpy(get_addr(),file.get_addr(),size);
}
void File::to(File file,Size len){
	pz_memcpy(get_addr(),file.get_addr(),len);
}

//File screen=File(Addr(0xa0000),Size(100*320));
