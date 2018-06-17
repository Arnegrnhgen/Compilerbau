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

int main() {
	int i = 2;
	return foo() + bar(i + 1) + foobar(foo(), bar(42));
}
