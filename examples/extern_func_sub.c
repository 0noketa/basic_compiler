#include <stdio.h>
#include <stdlib.h>

extern void ENTRY();

void println(int i){
	printf("%d\n", i);
}

int f0() {
static int initialized = 0;
static int val;
	char buf[128];
	if (!initialized) {
		fgets(buf, 120, stdin);

		val = atoi(buf);
		initialized = 1;
	}

	return val;
}

int f1(int i) { return i + 111; }
int f2(int i) { return i * 1000; }

int main(){
	ENTRY();

	puts("ans:");
    int I, J, it;
	I = f0();
    J = f1(123) + f2(f1(88) + 1);
    println(I);
    println(J);

	return 0;
}
