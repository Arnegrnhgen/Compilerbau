#define A 2

#define B 3

int foo(){return 1;}
int main() {
        #define C 4
	int a = C;
	int BB = B;
	foo ();
	return a+(a)+BB+A+B+C+a;
}
