// simple call testing

int foo() {
	return 42;
}

int bar(int i) {
	return 2 * i;
}

int foobar(int i, int j) {
	return i * j;
}

int fib(int i) {
	if (i == 0) {
		return 1;
	} else if (i == 1) {
		return 1;
	} else {
		return fib(i-1) + fib(i-2);
	}

	return 0;
}

int main() {
	int i = 2;
	return foo() + bar(i + 1) + foobar(fib(6), bar(42));
}
