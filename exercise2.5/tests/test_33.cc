struct a {
	double b;
	int c;
};

a foo(a s) {
	s.b = 3.0;
	return s;
}

int main() {
	a s;
	s.b = 2.0;
	s.c = 2;

	s = foo(s);

	if (s.b == 3.0 && s.c == 2) {
		return 2;
	} else {
		return 3;
	}
}
