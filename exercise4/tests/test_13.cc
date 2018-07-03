// comparissions

int main() {
	int i = 1;

	if (i > 4) {
		i = i + 2;
	} else {
		i = i + 4;
	}

	if (i > 1) {
		i = i + 8;
	} else {
		i = i + 16;
	}

	if ( i >= 1 ) {
		i = i + 32;
	} else {
		i = i + 64;
	}

	if ( i < 10 ) {
		i = i + 128;
	} else { 
		i = i + 256;
	}

	if ( i <= 40000 ) {
		i = i + 512;
	} else {
		i = i + 1024;
	}

	return i;
}
