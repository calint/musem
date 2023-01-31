typedef char* Addr;
class Ref{
	Addr addr;
public:
	inline Ref(Addr addr0):addr{addr0}{}
	inline Addr get_addr()const{return addr;}
};

typedef int Size;

class File:public Ref{
	Size size;
public:
	inline File(Addr addr0,Size size0):Ref{addr0},size{size0}{}
	void to(File file);
	void to(File file,Size nbytes);
	inline Size get_size()const{return size;}
};
