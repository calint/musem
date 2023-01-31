typedef void* Address;
typedef int Size;

class Ref{
	Address address;
public:
	inline Ref(Address address_):address{address_}{}
	inline auto get_address()const->Address{return address;}
	inline auto write_byte(char b)->void{*(char*)address=b;}
	inline auto write_int(int i)->void{*(int*)address=i;}
	inline auto offset(Size s)->Ref{return Ref{(char*)address+s};}
};

class File:public Ref{
	Size size_B;
public:
	inline File(Address address_,Size size_B_):
		Ref{address_},size_B{size_B_}
	{}
	auto to(File file)->void;
	auto to(File file,Size nbytes)->void;
	inline auto get_size_B()const->Size{return size_B;}
};

typedef int Coordinate;

class Coordinates{
	Coordinate x;
	Coordinate y;
public:
	inline Coordinates(Coordinate x_,Coordinate y_):x{x_},y{y_}{}
	inline auto get_x()const->Coordinate{return x;}
	inline auto get_y()const->Coordinate{return y;}
	inline auto set_x(Coordinate x_){x=x_;}
	inline auto set_y(Coordinate y_){y=y_;}
	inline auto set(Coordinate x_,Coordinate y_){set_x(x_);set_y(y_);}
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
	auto to(Bitmap&bmp,const Coordinates&coordinates)->void;
};
