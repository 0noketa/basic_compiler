#include <stdio.h>
#include <stdlib.h>

extern void ENTRY();
extern int I;

int PRINTI(){
	printf("%d\n", I);
}

void INPUTI(){
	char buf[128];
	fgets(buf, 120, stdin);
	I = atoi(buf);
}

int main(){
	ENTRY();

	return 0;
}
