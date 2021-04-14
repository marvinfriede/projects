#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include <math.h>

// just for debugging
void printMatrix(double **matrix, int dim){
	int i, j;
	for (i = 0; i < dim; i++){
			for (j = 0; j < dim; j++){
					printf("%.8lf ", matrix[i][j]); // print each row
			}
			printf("\n"); // linebreak for new row
	}
	printf("\n");
	return;
}

// just for debugging
void printVector(double *vector, int dim){
	int i;
	printf("\n");
	for (i = 0; i < dim; i++){
		printf("%.8lf \n", vector[i]);
	}
	printf("\n");
}


int main(void){
	omp_set_dynamic(0);
	omp_set_num_threads(4);

	int dim = 17000;
	int i, j, k, exp; 
	double norm, temp;
	double **A, **DLU;
	double *b, *Dinv_b, *u, *u_new, *result, *DLU_times_u, *residual;
	double start_time, end_time, time_matrices, time_iterations;


	// start timer
	start_time = omp_get_wtime();

	printf("Setting up matrices and vectors...");
	fflush(stdout); // otherwise printf to buffer, since there is no \n

	// creating matrices
	A = (double **)malloc(dim * sizeof(double*));
	DLU = (double **)malloc(dim * sizeof(double*)); // -Dinv*(L+U)
	for (i = 0; i < dim; i++){
		A[i] = (double *)malloc(dim * sizeof(double));
		DLU[i] = (double *)malloc(dim * sizeof(double));
	}

	// coefficient matrix A
	#pragma omp parallel for collapse(2) // collapsing results in speedup
	for (i = 0; i < dim; i++){
		for (j = 0; j < dim; j++){
			if (i == j){
				A[i][j] = 1;
			}
			else {
				exp = - 2 * fabs(i - j);
				A[i][j] = -pow(2, exp);
			}
		}
	}

	// -Dinv*(L+U)
	#pragma omp parallel for collapse(2) // collapsing results in speedup
	for (i = 0; i < dim; i++){
		for (j = 0; j < dim; j++){
			if (i == j){
				DLU[i][j] = 0;
			}
			else {
				exp = - 2 * fabs(i - j);
				DLU[i][j] = pow(2, exp);
			}
		}
	}

	// just for debugging
	//printf("\nA \n");
	//printMatrix(A, dim);
	//printf("DLU \n");
	//printMatrix(DLU, dim);


	// create vectors
	b = malloc(dim * sizeof(double)); // RHS
	Dinv_b = malloc(dim * sizeof(double)); // Dinv*b
	u = malloc(dim * sizeof(double)); // u(n) ; initial guess
	DLU_times_u = malloc(dim * sizeof(double));
	residual = malloc(dim * sizeof(double));

	// define vectors
	#pragma omp parallel for // small speedup
	for (i = 0; i < dim; i++){
		u[i] = 0; // initial guess
		b[i] = 1; // rhs
		residual[i] = 0; // zeros, needed for addition on convergence check
		Dinv_b[i] = 1; // Dinv*b -> constant
	}

	time_matrices = omp_get_wtime();
	printf("done (%.2lfs)\n", time_matrices - start_time);


	
	printf("Starting Jacobi iterations...");
	fflush(stdout);

	// iterations
	for (i = 0; i < 200000; i++){
		// execution of formula
		for (j = 0; j < dim; j++){
			#pragma omp parallel for reduction(+:DLU_times_u[j])
			for (k = 0; k < dim; k++){
				DLU_times_u[j] += DLU[j][k] * u[k]; // Dinv*(L+U)*u
			}
			u[j] = DLU_times_u[j] + Dinv_b[j]; // Dinv*(L+U)*u + Dinv*b
		}
		

		// checking for convergence with norm of residual vector r = b - A*u
		for (j = 0; j < dim; j++){
			residual[j] += b[j];
			#pragma omp parallel for reduction(+:residual[j])
			for (k = 0; k < dim; k++){
				residual[j] -= A[j][k] * u[k];
			}	
		}

		norm = 0.0;
		#pragma omp parallel for reduction(+:norm)
		for (j = 0; j < dim; j++){
			temp = residual[j]; // small speedup
			norm += temp*temp;
		}
		norm = sqrt(norm);

		if (norm < 0.001){
			time_iterations = omp_get_wtime();
			printf("done (%.2lfs)\n\n", time_iterations - time_matrices);
			printf("Finished after %d iterations.\n(Residual norm: %f)\n\n", i, norm);
			break;
		}
		else {
			// exit if program takes too long
			if (i > 100){
				printf("\n\nHighest number of iterations is reached. Aborting...\n");
				exit(0);
			}
			// resetting vector, parallel->no speedup
			for (j = 0; j < dim; j++){
				DLU_times_u[j] = 0;
				residual[j] = 0;
			}
		}
	}


	// print result
	//printf("result:");
	//printVector(u, dim);



	//memory deallocation for (2D) arrays
	for (i = 0; i < dim; i++){
		free(A[i]);
		free(DLU[i]);
	}
	free(A);
	free(DLU);

	free(Dinv_b);
	free(residual);
	free(b);
	free(u);


	// end timer
	end_time = omp_get_wtime();
	printf("Total execution time: %.2lfs\n", end_time - start_time);

	return 0;
}

/*
compiled with:
gcc jacobi.c -fopenmp -o jacobi.exe -lm -O3



example output: 
Setting up matrices and vectors...done (12.79s)
Starting Jacobi iterations...done (6.47s)

Finished after 16 iterations.
(Residual norm: 0.000994)

Total execution time: 19.46



notes:
- with dim = 20000, memory became the limiting factor on my machine
- therefore: dim = 17000 









-------------------------------------------------------------------------------
some old functions...

void matMult(double **matrix1, double **matrix2, double **result, int dim){
	int i, j, k;
	for (i = 0; i < dim; i++){ // iterate over rows of first matrix
			for (j = 0; j < dim; j++){ // iterate over columns of second matrix
					result[i][j] = 0; // create zero matrix
					for (k = 0; k < dim; k++){ // iterate over columns of matrix1 and rows of matrix2, respectively
							result[i][j] += matrix1[i][k] * matrix2[k][j];
					}
			}
	}
	return;
}


void matAdd(double **matrix1, double **matrix2, double **result, int dim){
	int i, j;
	for (i = 0; i < dim; i++){ // iterate over rows of matrix
		for (j = 0; j < dim; j++){ // iterate over columns of matrix
			result[i][j] = matrix1[i][j] + matrix2[i][j];
		}
	}
	return;
}


void matVecMult(double **matrix, double *vector, double *result, int dim){
	int i, j;
	for (i = 0; i < dim; i++){ // iterate over rows of matrix
		for (j = 0; j < dim; j++){ // iterate over columns of matrix
			result[i] += matrix[i][j] * vector[j];
		}
	}
	return;
}

*/