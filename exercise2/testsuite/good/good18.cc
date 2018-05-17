struct data {
	int i ;
	double f ;
};

struct data2 {
	data d ;
};

data f(data d) {
	return d ;
}

int main() {
	int x ;
	double y ;
	bool b ;
	data d ;
	data2 d2 ;
	x = 6 ;
	y = 1.0;
	d.i = x ;
	x = d.i ;
	d.f = y ;
	y = d.f ;
	f(d) ;
	y = x + 1.0 ;
	y = x - 1.0 ;
	y = x * 1.0 ;
	y = x / 1.0 ;
	b = x < y ;
	b = x <= y ;
	b = x > y ;
	b = x >= y ;
	b = x == y ;
	d2.d = d;
	d2.d.i = x;
}
