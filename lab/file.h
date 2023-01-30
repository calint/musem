typedef char* Addr;
class Ref{
	Addr addr;
public:
	Ref(Addr addr);
	Addr get_addr();
};
typedef int Size;
class File:Ref{
	Size size;
public:
	File(Addr addr,Size size);
	void to(File file);
	void to(File file,int len);
	Size get_size();
};
