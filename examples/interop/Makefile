FBC32=fbc32
FBC64=fbc64
BC=..\..\bc

fb_main: fb_sub.o bc_sub.o
	$(FBC32) -s console fb_main.bas  fb_sub.o bc_sub.o
fb_sub.o:
	$(FBC32) -c -nodeflibs fb_sub.bas
bc_sub.o:
	$(BC) -int 32 bc_sub.bas > bc_sub.asm
	nasm -fwin32 --prefix _ -o bc_sub.o bc_sub.asm

fb_main_64: fb_sub_64.o bc_sub_64.o
	$(FBC64) -s console  -o fb_sub_64.exe  fb_main.bas  fb_sub_64.o bc_sub_64.o
fb_sub_64.o:
	$(FBC64) -c -nodeflibs -o fb_sub_64.o fb_sub.bas
bc_sub_64.o:
	$(BC) -int 64 bc_sub.bas > bc_sub_64.asm
	nasm -fwin64 -o bc_sub_64.o bc_sub_64.asm
