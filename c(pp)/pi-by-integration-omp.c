#include <stdio.h>
#include <omp.h>
#include <time.h>


// function for approximating pi 
double function(double *x){
	return 1.0 / (1 + *x * *x);
}


// numerical integration with trapezoidal rule
double trapezoidal(unsigned short *a, unsigned short *b, unsigned long *n){
	double h, x, sum = 0;
	h = (double) (*b - *a) / *n; // prefactor

	#pragma omp parallel for firstprivate(a, b) reduction(+:sum)
	for (unsigned long i = 0; i <= *n; i++){
		x = *a + i*h;
		sum += function(&x); // summation part of trapezoidal rule
	}
	
	return h*(sum + 0.5*(function( (double*)a ) + function( (double*)b ))); 
}


int main(void){
	omp_set_dynamic(0); // explicitly disable dynamic teams
	omp_set_num_threads(4); // n threads for all parallel regions
	// printf("%d\n", omp_get_num_threads()); // only works in parallel region

	double start_time, end_time;
	start_time = omp_get_wtime();

	double integral, pi_approx;
	unsigned short a = 0, b = 1; // lower and upper bound of integral
	unsigned long n = 100000000; // stepsize, increase n for higher accuracy
	
	pi_approx = 4*trapezoidal(&a, &b, &n); // four times the integral
	end_time = omp_get_wtime();
	
	printf("Correct value of pi:\t\t3.1415926536\n");
	printf("Appoximated value of pi:\t%.10lf\n\n", pi_approx);
	printf("Total execution time: %.2lfs\n", end_time - start_time);
	
	return 0;
}


/*
Example-Output:

Correct value of pi: 3.1415926536
Appoximated value of pi: 3.1415926596

Total execution time: 0.89


Note: 
 - using 1 thread, the results are consistent
 - however, with more threads the results vary every time
 - fix: compiling with -O2 or -O3 (-O1 does not work)


// function is inlined, but avoiding calls does not enhance performance
double trapezoidal(unsigned short *a, unsigned short *b, unsigned long *n){
    double h, x, sum = 0;
	h = (double) (*b - *a) / *n; // prefactor

	#pragma omp parallel for firstprivate(a, b) reduction(+:sum)
	for (unsigned long i = 0; i <= *n; i++){
		x = *a + i*h;
		sum += 1.0 / (1 + x*x); // summation part of trapezoidal rule
	}
	
	return h*(sum + 0.5*(1.0 / (1 + *a * *a) + 1.0 / (1 + *b * *b) )); 
}
*/