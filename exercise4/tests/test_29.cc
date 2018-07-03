int main() {
	bool a = false;
	bool b;
	b = true;
	bool c = b && a;
	c = a || b;
	c = a == b;
	c = a != b;
	c = a == false;
	c = true != b;
	c = a || true;
	c = false && b;

	return 5;
}
