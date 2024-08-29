CC=gcc
BC=..\bc

input: input.o
	$(CC) -o input.exe  tes.c  input.o
input.o:
	$(BC) -int 32 input.txt > input.asm
	nasm -fwin32 --prefix _ -o input.o input.asm

primes: primes.o
	$(CC) -o primes.exe  tes.c  primes.o
primes.o:
	$(BC) -int 32 primes.txt > primes.asm
	nasm -fwin32 --prefix _ -o primes.o primes.asm

array: array.o
	$(CC) -o array.exe  tes.c  array.o
array.o:
	$(BC) -int 32 array.bas > array.asm
	nasm -fwin32 --prefix _ -o array.o array.asm

extern_func: extern_func.o
	$(CC) -o extern_func.exe  extern_func_sub.c  extern_func.o
extern_func.o:
	$(BC) -int 32 extern_func.bas > extern_func.asm
	nasm -fwin32 --prefix _ -o extern_func.o extern_func.asm
