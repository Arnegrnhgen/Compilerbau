//scoping

int main() {
	int i = 2;
	int j = 0;

	{
		int i = 3;
		i = i + 1;
		j = i + j;
	}

	{
		int i = 4;
		j = j + i;
		
		{
			int i = 5;
			j = j + i;
		}
	}


	return i + j;
}
