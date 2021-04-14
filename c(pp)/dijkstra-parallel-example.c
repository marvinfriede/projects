#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include "mpi.h"

#define INF 999999
#define DIM 5


// printing a matrix; for debugging
void printMatrix(int matrix[DIM][DIM]){
	int i, j;
	for (i = 0; i < DIM; i++){
		for (j = 0; j < DIM; j++){
				printf("%d\t", matrix[i][j]);
		}
		printf("\n"); // linebreak for new row
	}
	return;
}

// printing a vector; just for debugging
void printVector(int *vector){
	int i;
	for (i = 0; i < DIM; i++){
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


// utility function that prints distance between start node and all other nodes
void printPath(int i, int start, int *parent, int *distance){
	int j, count, temp, path[DIM];

	// initialize path array with no connections
	for (j = 0; j < DIM; j++){
		path[j] = INF;
	}

	// removes "d(0-0) = 0	path: 0" from print output
	if (i != start){
		temp = distance[i];
		printf("\nd(%d-%d) = %d\t\tpath: ", start, i, temp);
		j = i;

		// going backwards through parents until start is reached
		count = 0;
		while(j != start) {
			j = parent[j];
			path[count] = j;
			count++;
		}

		// reversing path for nicer printing (start -> ... -> end)
		reverseArray(path, 0, DIM - 1); 

		// printing the path and resetting path array
		for (j = 0; j < DIM; j++){
			temp = path[j];
			if (temp != INF){
				printf("%d -> ", temp);
			}
			path[j] = INF;
		}
		printf("%d", i); // end node
	}
}


// Dijkstra's algorithm; taking a graph, its dimension and the starting point
void dijkstra(int graph[DIM][DIM], int start, int rank, int size){
	int i, j, k, minDistance, next, chunksize, rest, begin, end;
	int *recv_counts = NULL, *recv_displs = NULL, *sub_dist, *sub_parent;
	int global_min[2], local_min[2];
	int full_dist[DIM], full_parent[DIM];
	bool visited[DIM];

	// borders for MPI parallelization of arrays 
	chunksize = DIM / size; // array size for each processor; int division
	begin = rank * chunksize; // start index of array in loop
	end = begin + chunksize - 1; // end index of array in loop

	// if #nodes is not divisible by #procs; remainder to last proc (imbalance)
	rest = DIM % size;
	if (rank == size - 1){
		if (rest){
			chunksize += rest;			
			end += rest;
		}
	}

	// loop 1: init distance vector; distances from start to adjescent nodes 
	sub_dist = (int *) malloc(chunksize * sizeof(int));
	for (i = begin; i <= end; i++){
		sub_dist[i - begin] = graph[start][i]; // could als use i + begin
	}
	/* set distance from start node to itself to zero; since the code works
	with parts of the full array, only node 0 will definitely be in the part
	assigned to rank 0, i.e. start nodes != 0 may be located in sub arrays of 
	other ranks; hence, the code will not work for other start nodes */
	if (rank == 0){
		sub_dist[start] = 0; 
	}

	// loop 1': init parent node array; no exisiting parents yet
	sub_parent = (int *) malloc(chunksize * sizeof(int));
	for (i = 0; i < chunksize; i++){
		sub_parent[i] = 0;
	}

	// loop 1'': init status array; processed nodes are flagged with true 
	for (i = 0; i < DIM; i++){
		visited[i] = false; 
	}
	visited[start] = true; // not necessary, since zero is minDistance anyway; saves a bit of time as it circumvents the second if statement in loop 3 


	// loop 2: main part of algorithm; iterating over number of nodes
	for (i = 0; i < DIM; i++){
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

	// gather chunksizes in array "recv_counts" for root
	MPI_Gather(&chunksize, 1, MPI_INT, recv_counts, 1, MPI_INT, 0, MPI_COMM_WORLD);
	// smarter: with recv_counts[rank] = chunksize

	// build recv_displs array with chunksizes from recv_counts
	if (rank == 0){
		recv_displs[0] = 0;
		for (i = 1; i < size; i++){
			recv_displs[i] = recv_displs[i - 1] + recv_counts[i - 1];
		}
	}

	// concatenate sub arrays
	// sub_dist and chunksize are basically local information of each thread
	// full_dist = destination array
	// recv_counts = array of dim size with chunksizes as elements
	// recv_displs = array of dim size with indexes for concatenation as elements
	MPI_Gatherv(sub_dist, chunksize, MPI_INT, full_dist, recv_counts, recv_displs, MPI_INT, 0, MPI_COMM_WORLD);
	MPI_Gatherv(sub_parent, chunksize, MPI_INT, full_parent, recv_counts, recv_displs, MPI_INT, 0, MPI_COMM_WORLD);

	// MPI part 2: end
	// ------------------------------------------------------------------------


	// printing
	if (rank == 0){
		printf("Adjacency matrix:\n-----------------\n");
		printMatrix(graph);
		printf("\n\n");
		printf("Paths:\n------");
		for (i = 0; i < DIM; i++){
			printPath(i, start, full_parent, full_dist);
		}
		printf("\n");
	}

	free(sub_dist);
	free(recv_counts);
	free(recv_displs);

	return;
}


//nanosleep((const struct timespec[]){{0, 500000000L}}, NULL);

int main(int argc, char *argv[]){
	int i, j, rank, size;
	int exampleGraph[DIM][DIM];
	double start_time, end_time;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	start_time = MPI_Wtime();

	// initializing exampleGraph
	for (i = 0; i < DIM; i++){
		for (j = 0; j < DIM; j++){
			exampleGraph[i][j] = INF;
		}
	}

	// filling example matrix with correct values
	exampleGraph[0][1] = 4;
	exampleGraph[0][2] = 2;
	exampleGraph[1][2] = 3;
	exampleGraph[1][3] = 2;
	exampleGraph[1][4] = 3;
	exampleGraph[2][1] = 1;
	exampleGraph[2][3] = 4;
	exampleGraph[2][4] = 5;

	// calling Dijkstra's algorithm; passing in graph and starting point
	dijkstra(exampleGraph, 0, rank, size);

	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	MPI_Barrier(MPI_COMM_WORLD);
	end_time = MPI_Wtime();

	// timings
	if (rank == 0){
		printf("\nTime: %.2lf s\n", end_time - start_time);
	}

	MPI_Finalize();

	return 0;
}