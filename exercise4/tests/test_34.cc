struct data {
        int i ;
        double f ;
};

struct data2 {
        data d ;
};

int main() {
        int x = 4;
        data d;
	d.i = 3;
	d.f = 5.0;
        data2 d2;
        d2.d = d;
        d2.d.i = x;
	d2.d.f = d.f;

	if (d2.d.i == 4 && d2.d.f == 5.0) {
		return 2;
	} else {
		return 3;
	}
}
