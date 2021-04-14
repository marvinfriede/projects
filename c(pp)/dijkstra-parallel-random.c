#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include "mpi.h"

#ifndef INF
#define INF 999999
#endif


// printing a matrix; for debugging
void printMatrix(int **matrix, int dim){
	int i, j;
	for (i = 0; i < dim; i++){
		for (j = 0; j < dim; j++){
				printf("%d\t", matrix[i][j]);
		}
		printf("\n"); // linebreak for new row
	}
	return;
}

// printing a vector; for debugging
void printVector(int *vector, int dim){
	int i;
	for (i = 0; i < dim; i++){
		printf("%d\t", vector[i]);
	}
	printf("\n");
	return;
}


void reverseArray(int *arr, int start, int end){ 
    int temp; 
    while (start < end){ 
        temp = arr[start];    
        arr[start] = arr[end]; 
        arr[end] = temp; 
        start++; 
        end--; 
    }
    return;    
}


void printPath(int i, int start, int dimPath, int *parent, int *distance){
	int j, count, temp, path[dimPath];

	// initialize path array with no connections
	for (j = 0; j < dimPath; j++){
		path[j] = INF;
	}

	// removes "d(0-0) = 0	path: 0" from print output
	if (i != start){
		temp = distance[i];
		printf("d(%d-%d) = %d\t\tpath: ", start, i, temp);

		// catching problem of missing path between nodes, happens for small values of dim
		if (temp != INF){
			j = i;

			// going backwards through parents until start is reached
			count = 0;
			while(j != start) {
				j = parent[j];
				path[count] = j;
				count++;
			}

			// reversing path for nicer printing (start -> ... -> end)
			reverseArray(path, 0, dimPath - 1); 

			// printing the path and resetting path array
			for (j = 0; j < dimPath; j++){
				temp = path[j];
				if (temp != INF){
					printf("%d -> ", temp);
				}
				path[j] = INF;
			}
			printf("%d", i); // end node
		}
		else {
			printf("does not exist.");
		}
	}
	return;
}


// Dijkstra's algorithm
void dijkstra(int **graph, int dim, int start, int rank, int size, double *time_dijkstra_start, double *time_dijkstra_end){
	int i, j, k, minDistance, dimPath = 100, next = 1, chunksize, rest, begin, end;
	int *recv_counts = NULL, *recv_displs = NULL;
	int global_min[2], local_min[2];
	int full_dist[dim], full_parent[dim], sub_dist[dim], sub_parent[dim];
	bool visited[dim];

	// borders for MPI parallelization of arrays 
	chunksize = dim / size; // array size for each processor; int division
	begin = rank * chunksize; // start index of array in loop
	end = begin + chunksize - 1; // end index of array in loop

	// if #nodes is not divisible by #procs; remainder to last proc (imbalance)
	rest = dim % size;
	if (rank == size - 1){
		if (rest){
			chunksize += rest;			
			end += rest;
		}
	}

	// loop 1: init distance vector; distances from start to adjescent nodes 
	for (i = begin; i <= end; i++){
		sub_dist[i - begin] = graph[start][i];
	}
	/* set distance from start node to itself to zero; since the code works
	with parts of the full array, only node 0 will definitely be in the part
	assigned to rank 0, i.e. start nodes != 0 may be located in sub arrays of 
	other ranks; hence, the code will not work for other start nodes */
	if (rank == 0){
		sub_dist[start] = 0; 
	}

	// loop 1': init parent node array; no exisiting parents yet
	for (i = 0; i < chunksize; i++){
		sub_parent[i] = 0;
	}

	// loop 1'': init status array; processed nodes are flagged with true 
	for (i = 0; i < dim; i++){
		visited[i] = false; 
	}
	visited[start] = true; // not necessary, since zero is minDistance anyway; saves a bit of time as it circumvents the second if statement in loop 3 



	// start time for parallel part of dijkstra algorithm
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	*time_dijkstra_start = MPI_Wtime(); 

	// loop 2: main part of algorithm; iterating over number of nodes
	for (i = 0; i < dim; i++){
		local_min[0] = INF; // initializing minimum distance in every iteration

		/* loop 3: finding node with lowest distance value in distance vector
		(if node was not already visited); already processed nodes are
		excluded according to their visited status (true) */ 
		for (j = 0; j < chunksize ; j++){
			if (!visited[j + begin]){
				if (sub_dist[j] < local_min[0]){
					local_min[0] = sub_dist[j]; // minimal distance value
					local_min[1] = j + begin; // index of the closest node
				}
			}
		}


		// --------------------------------------------------------------------
		// MPI part 1: start

		// find global minimum 
		if (rank != 0){
			//printf("rank %d: with min = %d and next = %d \n", rank, local_min[0], local_min[1]);
			// slaves send local minimum to the master
			MPI_Send(local_min, 2, MPI_INT, 0, 0, MPI_COMM_WORLD);
		}
		else {
			//printf("rank %d: with min = %d and next = %d \n", rank, local_min[0], local_min[1]);
			// initialize global minimum with local min of master
			global_min[0] = local_min[0];
			global_min[1] = local_min[1];

			// checking for smaller values from slaves and updating
			for (j = 1; j < size; j++){
				MPI_Recv(local_min, 2, MPI_INT, j, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
				if (local_min[0] < global_min[0]){
					global_min[0] = local_min[0];
					global_min[1] = local_min[1];
				}
			}
		}

		// distribute the global minimum back to all procs
		if (rank == 0){
			for (j = 1; j < size; j++){
				MPI_Send(global_min, 2, MPI_INT, j, 0, MPI_COMM_WORLD);
			}
		}
		else {
			MPI_Recv(global_min, 2, MPI_INT, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}

		minDistance = global_min[0];
		next = global_min[1];

		// MPI part 1: end
		// --------------------------------------------------------------------


		/* closest node in current distance list is found and thus given a
		processed flag, which will leave it untouched in further iterations */
		visited[next] = true;

		/* loop 4: updating the distance vector with distances from next node 
		(graph[next][j]) -> including new connections (replacing INF) and
		shorter indirect paths; by adding the minDistance we keep track of the 
		distance already travelled w.r.t the start node; already processed
		nodes must again be excluded */
		for (j = 0; j < chunksize; j++){
			if (!visited[j + begin]){
				if (minDistance + graph[next][j + begin] < sub_dist[j]){
					sub_dist[j] = minDistance + graph[next][j + begin];
					sub_parent[j] = next;
				}
			}
		}
	}


	// ------------------------------------------------------------------------
	// MPI part 2: start

	// initialize array for start index of concatenation (recv_displs) and chunksizes (recv_counts); preparation for MPI_Gatherv
	if (rank == 0){
		recv_displs = (int *) malloc(size * sizeof(int));
		recv_counts = (int *) malloc(size * sizeof(int));
	}

	// gather chunksizes in array recv_counts for root
	MPI_Gather(&chunksize, 1, MPI_INT, recv_counts, 1, MPI_INT, 0, MPI_COMM_WORLD);

	// build recv_displs array with chunksizes from recv_counts
	if (rank == 0){
		recv_displs[0] = 0;
		for (i = 1; i < size; i++){
			recv_displs[i] = recv_displs[i - 1] + recv_counts[i - 1];
		}
	}

	// concatenate sub arrays
	MPI_Gatherv(sub_dist, chunksize, MPI_INT, full_dist, recv_counts, recv_displs, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Gatherv(sub_parent, chunksize, MPI_INT, full_parent, recv_counts, recv_displs, MPI_INT, 0, MPI_COMM_WORLD);

	// MPI part 2: end
	// ------------------------------------------------------------------------


	// time for parallel part of dijkstra algorithm
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	*time_dijkstra_end = MPI_Wtime();



	// printing; only for small values of dim (legibility in terminal, w=160)
	if (rank == 0){
		if (dim < 21){
			printf("\n");
			printf("Adjacency matrix:\n-----------------\n");
			printMatrix(graph, dim);
			printf("\n\nPaths:\n------");
			for (i = 0; i < dim; i++){
				printPath(i, start, dimPath, full_parent, full_dist);
				printf("\n");
			}
		}
		// only print path from start to last node
		else {
			printf("\nPath (start to last):\n---------------------\n");
			printPath(dim - 1, start, dimPath, full_parent, full_dist);
		}
		printf("\n");
	}
	
	free(recv_counts);
	free(recv_displs);

	return;
}


int main(int argc, char *argv[]){
	int i, j, k, dim, temp, rank, size;
	double start_time, end_time, time_init, time_random, time_dijkstra_start, time_dijkstra_end;

	// stuff for random numbers; seeding is necessary for true randomness
	int randMin = 0, randMax;
	//srand(time(NULL)); // comment out for comparability

	// MPI setup
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	start_time = MPI_Wtime();

	if (rank == 0){
		printf("Number of processors: %d\n\n", size);
	}


	// allowing to set dimension with one optional command line argument 
	if (argc != 2){ // really just one argument; default
		dim = 10000;
	}
	else {
		dim = atoi(argv[1]); // str2int (from stdlib.h)
	}
	if (dim > 100){
		randMax = dim / 10; // dynamic value for longer paths
	}
	else {
		randMax = 10;
	}


	// memory allocation for random matrix of dimension <dim>
	int **randomGraph = (int **)malloc(dim * sizeof(int*));
	for (i = 0; i < dim; i++){
		randomGraph[i] = (int *)malloc(dim * sizeof(int));
	}

	/* generate matrix of dimension <dim> depicting the adjacency matrix of a
	(bi)directed graph, which requires a symmetric matrix;
	initialized with unconnected nodes (INF), i < j gets a triangular matrix
	without the diagonal (remains INF), upper and lower triangular matrix are
	filled with the same random values if they are nonzero */
	for (i = 0; i < dim; i++){
		for (j = 0; j < dim; j++){
			randomGraph[i][j] = INF;
		}
	}

	// time for init
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	time_init = MPI_Wtime();

	for (i = 0; i < dim; i++){
		for (j = 0; j < i; j++){ // j < i avoids additional if statement
			temp = randMin + rand() / (RAND_MAX / (randMax - randMin + 1) + 1);
			if (temp != 0){
				randomGraph[i][j] = temp;
				randomGraph[j][i] = temp;
			}
		}
	}

	// time for creating the random graph
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	time_random = MPI_Wtime();


	// calling Dijkstra's algorithm
	dijkstra(randomGraph, dim, 0, rank, size, &time_dijkstra_start, &time_dijkstra_end);


	// memory deallocation for (2D) arrays
	for (i = 0; i < dim; i++){
		free(randomGraph[i]);
	}
	free(randomGraph);

	// total time
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	end_time = MPI_Wtime();

	// timings
	if (rank == 0){
		printf("\n\nTimings:\n--------\n");
		printf("Setup of graph (init):\t\t%.3lfs\n", time_init - start_time);
		printf("Setup of graph (random):\t%.3lfs\n", time_random - time_init);
		printf("Dijkstra (parallel):\t\t%.3lfs\n", time_dijkstra_end - time_dijkstra_start);
		printf("Total time:\t\t\t%.3lfs\n", end_time - start_time);
		printf("\n");
	}

	MPI_Finalize();

	return 0;
}


/*
compiling:
----------
module add openmpi/gcc/9.1.0/3.1.4
mpicc dijkstra-parallel-random.c -o dijkstra-parallel-random.exe


execution:
----------
./dijkstra-parallel-random.exe <number of nodes/vertices>
(one command line argument is allowed, otherwise defaults are applied)


fixed problems:
---------------
"next" must be initialized to 1 in order to avoid segmentation fault: If all
nodes are unconnected (all entries in matrix are INF), no closest node in is
found in loop 3. Therefore "next" is not given any value and defaults to 
32766, which is problematic for the following array indexing (graph[next][j]);
this usually just happens for graph sizes of 2 or 3


utility for debugging:
----------------------
printf("distance:\t");
printVector(distance, dim);
printf("parent:\t\t");
printVector(parent, dim);
*/