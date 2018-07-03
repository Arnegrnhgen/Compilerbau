// simple struct usage

struct a {
	double b;
	int c;
};

int main() {
	a s;
	s.b = 3.0;
	s.c = 1;
	int x = s.c;
	double y = s.b;
	if (x == 1 && y == 3.0) {
		return 2;
	} else {
		return 3;
	}
}
