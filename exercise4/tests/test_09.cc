// basic while test

int main () {
	int i,j;

	i = 0;
	j = 1;

	while (i == 10) {
		j = j + 1;
	}

	while (i != 4) {
		j = j * 2;
		i = i + 1;
	}

	return j;
}
