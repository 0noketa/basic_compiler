#include <stdio.h>

extern void ENTRY();
extern int I;

int PRINTI(){
	printf("%d\n", I);
}

int main(){
	ENTRY();

	return 0;
}
