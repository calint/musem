typedef char* Address;

class Ref{
	Address address;
public:
	inline Ref(Address address0):address{address0}{}
	inline Address get_address()const{return address;}
};

typedef int Size;

class File:public Ref{
	Size size_B;
public:
	inline File(Address address0,Size size0_B):
		Ref{address0},size_B{size0_B}
	{}
	void to(File file);
	void to(File file,Size nbytes);
	inline Size get_size_B()const{return size_B;}
};

typedef int Width;
typedef int Height;

class Bitmap:public File{
	Width width_px;
	Height height_px;
public:
	inline Bitmap(Address address0,Width width0_px,Height height0_px):
		File{address0,width0_px*height0_px},
		width_px{width0_px},height_px{height0_px}
	{}
	inline Width get_width_px()const{return width_px;}
	inline Height get_height_px()const{return height_px;}
};