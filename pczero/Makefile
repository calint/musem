INSTALL_TO=/dev/sdb
IMG=install.img
BIN=pc.img
SRC=pc.cc file.cc main.cc
CC=g++ -m32 -O0 -fno-pie
CW=-Wall -Wextra -Wfatal-errors
CF=-nostdlib -Wl,--oformat,binary -Wl,-Ttext,0x7c00

all:	clean build print display

build:
	@clear
	@$(CC) -o $(BIN) $(SRC) $(CF) $(CW)
	@chmod -x $(BIN)
	@cp $(BIN) $(IMG)
	@date>>$(IMG)
	@ls -l Makefile>>$(IMG)
	@ls -l $(SRC)>>$(IMG)
	@cat Makefile>>$(IMG)
	@cat $(SRC)>>$(IMG)
	
print:
	@pwd
	@du -bh $(IMG) $(BIN) $(SRC)
	
clean:
	@rm -f $(IMG) $(BIN)

display:
	@qemu-system-i386 -m 1 $(IMG)
	 
install:
	sudo dd if=$(IMG) of=$(INSTALL_TO)&&sync

readusb:
	sudo dd if=$(INSTALL_TO) count=2|hx|f 00000200

