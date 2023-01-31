typedef char* Addr;
class Ref{
	Addr addr;
public:
	inline Ref(Addr addr):addr{addr}{}
	inline Addr get_addr()const{return addr;}
};

typedef int Size;

class File:Ref{
	Size size;
public:
	inline File(Addr addr,Size size):Ref{addr},size{size}{}
	void to(File file);
	void to(File file,Size nbytes);
	inline Size get_size()const{return size;}
};
