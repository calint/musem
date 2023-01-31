#include "std.h"

extern "C" void pz_memcpy(Address from,Address to,Size size){
	// ? works with aligned mem, call inline assembler
	int c=size>>2;
	int*s=(int*)from;
	int*d=(int*)to;
	while(c--)
		*d++=*s++;
	//	asm("movl addr,%esi;movl to,%edi;movl size,%ecx;rep movsl;"::"S"(addr),"D"(to),"C"(size):"%esi","%edi","%ecx");
	//	asm("rep;movsl;"::"S"(addr),"D"(to),"c"(size):);}

}

inline void pz_write(Address a,const char b){
	*(char*)a=b;
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


auto File::to(File file)->void{
	// ? check buffer overrrun
	pz_memcpy(get_address(),file.get_address(),size_B);
}
auto File::to(File file,Size len)->void{
	// ? check buffer overrrun
	pz_memcpy(get_address(),file.get_address(),len);
}
auto Bitmap::to(Bitmap&bmp,const Coordinates&c)->void{
	Ref p=bmp.offset(c.get_y()*bmp.get_width_px()+c.get_x());
	p.write_int(0x04040404);
}
//File screen=File(Addr(0xa0000),Size(100*320));
