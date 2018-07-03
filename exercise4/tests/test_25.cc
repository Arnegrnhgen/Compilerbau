double foo(double a, double b) {
	return a * b;
}

int main() {
	double a = 1.5;
	double b;
	b = foo(2.0, a);

	if ( b == 4.0 ) {
		return 1;
	} else {
		return 2;
	}
	return 3;
}
