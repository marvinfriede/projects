// compiled with: gcc dijkstra-sequential-random.c -o dijkstra-sequential-random.exe

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

#ifndef INF
#define INF 99999
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


void printPath(int i, int start, int dimPath, int *path, int *parent, int *distance){
	int count, temp, j;

	// removes "d(0-0) = 0	path: 0" from print output
	if (i != start){
		temp = distance[i];
		printf("d(%d-%d) = %d\tpath: ", start, i, temp);

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


// Dijkstra's algorithm; taking a graph, its dimension and the starting point
void dijkstra(int **graph, int dim, int start, clock_t *time_dijkstra){
	int i, j, k, minDistance, next = 1, dimPath = 20;
	int distance[dim], parent[dim], path[dimPath];
	bool visited[dim];

	// loop 1: initializing
	for (i = 0; i < dim; i++){
		distance[i] = graph[start][i]; // vector of distances from start to adjescent nodes 
		parent[i] = 0; // no exisiting parents yet
		visited[i] = false; // no nodes visited yet
	}

	// loop 1': initializing path array separately, since dimPath << dim
	for (i = 0; i < dimPath; i++){
		path[i] = INF;
	}

	distance[start] = 0; // set distance of start node to itself to zero
	visited[start] = true; // not necessary, since zero is minDistance anyway; saves a bit of time as it circumvents the second if statement in loop 3 

	// loop 2: iterating over number of nodes
	for (i = 0; i < dim; i++){
		minDistance = INF; // initializing minimum distance in every iteration

		/* loop 3: finding node with lowest distance value in distance vector
		(if node was not already visited); already processed nodes are
		excluded according to their visited status (true) */ 
		for (j = 0; j < dim; j++){
			if (!visited[j]){
				if (distance[j] < minDistance){
					minDistance = distance[j]; // minimal distance value
					next = j; // index of the closest node
				}
			}
		}
		
		/* closest node in current distance list is found and thus given a
		processed flag, which will leave it untouched in further iterations*/
		visited[next] = true;

		/* loop 4: updating the distance vector with distances from next node 
		(graph[next][j]) -> including new connections (replacing INF) and
		shorter indirect paths; by adding the minDistance we keep track of the 
		distance already travelled w.r.t the start node; already processed
		nodes must again be excluded */
		for (j = 0; j < dim; j++){
			if (!visited[j]){
				if (minDistance + graph[next][j] < distance[j]){
					distance[j] = minDistance + graph[next][j];
					parent[j] = next;
				}
			}
		}
	}

	// time for dijkstra algorithm
	*time_dijkstra = clock();

	// printing; only for small values of dim (legibility in terminal, w=160)
	if (dim < 21){
		printf("\n");
		printf("Adjacency matrix:\n-----------------\n");
		printMatrix(graph, dim);
		printf("\n\nPaths:\n------");
		for (i = 0; i < dim; i++){
			printPath(i, start, dimPath, path, parent, distance);
			printf("\n");
		}
	}
	// only print path from start to last node
	else {
		printf("\nPath (start to last):\n---------------------\n");
		printPath(dim - 1, start, dimPath, path, parent, distance);
	}
	printf("\n");

	return;
}


int main(int argc, char *argv[]){
	int i, j, k, corr = 1, temp, dim;
	clock_t start_time, end_time, time_init, time_setup, time_dijkstra;

	// stuff for random numbers; seeding is necessary for true randomness
	int randMin = 0, randMax;
	srand(1); // time(NULL)

	start_time = clock();

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
	filled with the same random values if they are nonzero;
	faster than a single double loop with if (i < j || i > j) */
	for (i = 0; i < dim; i++){
		for (j = 0; j < dim; j++){
			randomGraph[i][j] = INF;
		}
	}
	time_init = clock();
	for (i = 0; i < dim; i++){
		for (j = 0; j < i; j++){ // j < i avoids branch (if statement)
			temp = randMin + rand() / (RAND_MAX / (randMax - randMin + 1) + 1);
			if (temp != 0){
				randomGraph[i][j] = temp;
				randomGraph[j][i] = temp;
			}
		}
	}

	// time for creating the random graph
	time_setup = clock();

	// calling Dijkstra's algorithm
	dijkstra(randomGraph, dim, 0, &time_dijkstra);

	// memory deallocation for (2D) arrays
	for (i = 0; i < dim; i++){
		free(randomGraph[i]);
	}
	free(randomGraph);

	// total time
	end_time = clock();

	// timings
	printf("\nTimings:\n--------\n");
	printf("Graph (fill):\t\t%.3lfs\n", (double)(time_init - start_time) / CLOCKS_PER_SEC);
	printf("Graph (randomize):\t%.3lfs\n", (double)(time_setup - time_init) / CLOCKS_PER_SEC);
	printf("Dijkstra:\t\t%.3lfs\n", (double)(time_dijkstra - time_setup) / CLOCKS_PER_SEC);
	printf("Total time:\t\t%.3lfs\n", (double)(end_time - start_time) / CLOCKS_PER_SEC);
	printf("\n");

	return 0;
}


/*
compiling:
----------
module add openmpi/gcc/9.1.0/3.1.4
gcc random.c -o random.exe


execution:
----------
./random.exe <number of nodes/vertices>
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