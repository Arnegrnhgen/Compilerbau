#define A 3
#ifdef B
#define A 4
#endif

int main() {
	return A;
}
