// junctions

int main() {
	int i = 5;

	bool a = ((i == 5) && (i < 6)) || (i < 4);

	if (a) {
		return 2;
	} else {
		return 3;
	}
	return 4;
}
