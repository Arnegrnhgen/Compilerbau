// junctions with sideeffects

int main() {
	int i = 5;

	bool a = (i < 5) && (++i < 10);

	bool b = (i >= 1) || (++i < 10);

	return i;
}
