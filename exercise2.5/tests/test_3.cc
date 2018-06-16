int main() {
	int i1;
	int i2, i3, i4;
	int i5 = 2;
	int i6 = i5;
	i2 = i1 = 3;
	i4 = i3 = i1;
	i1 = i1 * 2;
	i2 = 2 * i2;
	i3 = 2 - i4 - 3 - i5;
	i6 = i6 / 2;

	return i1 + i2 + i3 + i4 + i5 + i6;
}
