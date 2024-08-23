FBC=fbc32
BC=..\..\bc

fb_main: fb_sub.o bc_sub.o
	$(FBC) -s console fb_main.bas  fb_sub.o bc_sub.o

fb_sub.o:
	$(FBC) -c -nodeflibs fb_sub.bas

bc_sub.o:
	$(BC) -int 32 bc_sub.bas > bc_sub.asm
	nasm -fwin32 --prefix _ -o bc_sub.o bc_sub.asm
