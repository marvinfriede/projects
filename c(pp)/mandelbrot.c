#include <stdio.h>
#include <stdlib.h>


void mandelbrot(double c_real, double c_imag){
	unsigned int i, niter = 100000;
	unsigned short threshold = 4;
	double temp; // otherwise y would be updated with new value of x
	double x = c_real, y = c_imag; // z_real = x, z_imag = y; z0 = c
	
	for (i = 0; i < niter; i++){
		// z² + c = (x + yi)² + (a + bi) = (x² - y² + 2xyi) + (a + bi)
		// z² + c = (x² - y² + a) + (2xy + b)i
		temp = x;
		x = (x*x) - (y*y) + c_real; // real part of z² + real part of c
		y = (2*temp*y) + c_imag; // imag part of z² + imag part of c

		// actually |z| = sqrt(x² + y²) > 2, but we can square the absolute
		// value to avoid calculating the root -> |z|² > 4
		if ((x*x) + (y*y) > threshold){
			printf(" ");
			return;
		}
	}
	printf(".");
	return;
}


int main(){
	int n = 80, m = 40; // terminal size (height = n; width = m + 1)
	int c_imag_start = -1, c_imag_end = 1;
	int c_real_start = -2, c_real_end = 1;
	double c_real, c_imag;

	// stepsize along imag and real axis; needs cast->otherwise int division
	double c_imag_stepsize = (double) (abs(c_imag_start) + abs(c_imag_end)) / m;
	double c_real_stepsize = (double) (abs(c_real_start) + abs(c_real_end)) / n;
	
	for (unsigned int a = 0; a <= m; a++){
		c_imag = c_imag_start + c_imag_stepsize*a;
		for (unsigned int b = 0; b <= n; b++){
			c_real = c_real_start + c_real_stepsize*b;
			mandelbrot(c_real, c_imag);
		}
		printf("\n");
	}
	
	return 0;
}
