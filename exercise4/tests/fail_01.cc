struct s { int a; };

s foo(int i) {
	s a, b;
	a.a = 1;
	b.a = 2;
	if (i == 1) {
		return a;
	} else {
		return b;
	}


}

int main() {
	s c = foo(1);
	return c.a;
}
