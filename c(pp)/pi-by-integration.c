#include <stdio.h>

// function for approximating pi
double function(double *x)
{
	return 1 / (1 + *x * *x);
}

// numerical integration with trapezoidal rule
double trapezoidal(double *a, double *b, int *n)
{
	double h, x, sum = 0, integral;
	int i;

	h = (*b - *a) / *n; // prefactor
	for (i = 0; i < *n; i++)
	{
		x = *a + i * h;
		sum += function(&x); // summation part of trapezoidal rule
	}

	integral = h * (sum + 0.5 * (function(a) + function(b)));

	return integral;
}

int main(void)
{
	double integral, pi_approx;
	double a = 0, b = 1; // lower and upper bound of integral
	int n = 200000000;	 // stepsize, increase n for higher accuracy

	integral = trapezoidal(&a, &b, &n);
	pi_approx = 4 * integral;

	printf("Correct value of pi:\t\t3.1415926536\n");
	printf("Appoximated value of pi:\t%.10lf\n", pi_approx);
	return 0;
}

// each process needs to know number of intervals in mpibcast
