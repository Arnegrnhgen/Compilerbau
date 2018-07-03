// test bools

int main() {
	bool a;
	bool b = true;
	a = b;
	if (a == true) {
		b = false;
	} else {
		a = false;
	}

	if (b == false) {
		return 1;
	} else {
		return 2;
	}

	return 3;
}
