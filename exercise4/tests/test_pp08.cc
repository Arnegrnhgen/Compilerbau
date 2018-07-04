#define A

#ifdef A
int foo() { return 3; }
#endif

#ifdef B
int foo() { return 4; }
#endif

int main() {
	return foo();
}
