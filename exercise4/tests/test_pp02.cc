#define A 2

#define B 3

int main() {
        #define C 4
	int a = C;
	int b = B;
	return a + a + b + A + B + C + a;
}
