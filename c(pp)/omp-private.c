#include <stdio.h>
#include <omp.h>

int main(void){
	omp_set_dynamic(0);
	omp_set_num_threads(4);
	printf("%d\n\n", omp_get_num_threads());

	int i, x = 42;
	
	
	printf("----------------------------------\n");
	printf("private\n\n");
	printf("start x is %d\n", x);

	#pragma omp parallel for private(x)
	for(i = 0; i < 4; i++){
		x += i;
		printf("Thread number: %d     x: %d\n", omp_get_thread_num(), x);
	}
	
	printf("end x is %d\n\n", x);

	
	printf("----------------------------------\n");
	printf("lastprivate\n\n");
	int y = 42;
	printf("start y is %d\n", y);

	#pragma omp parallel for lastprivate(y)
	for(i = 0; i < 4; i++){
		y += i;
		printf("Thread number: %d     y: %d\n", omp_get_thread_num(), y);
	}
	
	printf("end y is %d\n\n", y);


	printf("----------------------------------\n");
	printf("firstprivate\n\n");
	int z = 42;
	printf("start z is %d\n", z);

	#pragma omp parallel for firstprivate(z)
	for(i = 0; i < 4; i++){
		z += i;
		printf("Thread number: %d     x: %d\n", omp_get_thread_num(), z);
	}
	
	printf("end z is %d\n", z);
	
 
    return 0;
}

// private: not initialised inside parallel region->random value, modifications not kept 

// firstprivate: initialise with value before parallel region, modifications not kept

// lastprivate: keep last value of parallel region

