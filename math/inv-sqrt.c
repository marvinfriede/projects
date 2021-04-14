#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <time.h>


float fastInvSqrt(float number) {
	// define 32 bit long and floats
	// floats do not allow bit manipulation -> c.f. bit hack
	long i; 
	float y = number;
	const float threehalfs = 1.5F;
	const float x2 = number * 0.5F;

	// bit hack:
	// 1. get address of y (float)
	// 2. convert float's address to long's address (translate bits 1:1)
	// 3. read number from address as long
	i = * (long *) &y;

	// number from rather complicated approximation
	// bit shift once to right for division by 2
	i = 0x5f3759df - (i >> 1);
	
	// reverse bit hack
	y = * (float *) &i;

	// newton iterations to improve approximation
	// f(y) = 1/y**2 - x = 0 -> y = 1 / sqrt(x)
	// one iteration gives error of 1%
	y = y * (threehalfs - (x2 * y * y));

	return y;
}

float normalInvSqrt(float number) {
	return 1 / sqrt(number);
}


int main(int argc, char const *argv[])
{	
	const long nIter = 1E9;
	float num = 0;
	time_t t;

	// seed random
	srand((unsigned) time(&t));

	printf("Calculating the inverse square roots of %.1e random numbers...\n\n", (double)nIter);

	// fast algorithm
	clock_t fastStart = clock(); 
	for (long i = 0; i < nIter; ++i)
	{
		float num = (float)rand() / (float)(RAND_MAX);
		fastInvSqrt(num);
	}
	clock_t fastEnd = clock();

	printf("Quake Algorithm:\t\t%.5lfs\n", (double)(fastEnd - fastStart) / CLOCKS_PER_SEC);


	// normal calculation
	clock_t normalStart = clock(); 
	for (long i = 0; i < nIter; ++i)
	{
		float num = (float)rand() / (float)(RAND_MAX);
		normalInvSqrt(num);
	}
	clock_t normalEnd = clock();

	printf("Library sqrt() function:\t%.5lfs\n", (double)(normalEnd - normalStart) / CLOCKS_PER_SEC);
	return 0;
}

// compiled with: gcc invSqrt.c -o invSqrt.exe -lm -O3

// -O1: Quake algorithm faster 
// -O2: speeds up normal calculation -> now faster than Quake
// -O3: slightly improves times from -O2 -> fastest, 

// Nice explantion: https://www.youtube.com/watch?v=p8u_k2LIZyo