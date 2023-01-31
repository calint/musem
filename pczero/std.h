typedef char* Address;

class Ref{
	Address address;
public:
	inline Ref(Address address_):address{address_}{}
	inline auto get_address()const->Address{return address;}
};

typedef int Size;

class File:public Ref{
	Size size_B;
public:
	inline File(Address address_,Size size_B_):
		Ref{address_},size_B{size_B_}
	{}
	void to(File file);
	void to(File file,Size nbytes);
	inline auto get_size_B()const->Size{return size_B;}
};

typedef int Width;
typedef int Height;

class Bitmap:public File{
	Width width_px;
	Height height_px;
public:
	inline Bitmap(Address address_,Width width_px_,Height height_px_):
		File{address_,width_px_*height_px_},
		width_px{width_px_},height_px{height_px_}
	{}
	inline auto get_width_px()const->Width{return width_px;}
	inline auto get_height_px()const->Height{return height_px;}
};
