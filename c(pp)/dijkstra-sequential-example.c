// compiled with: gcc dijkstra-sequential-example.c -o dijkstra-sequential-example.exe

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>

#define INF 9999
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
void printPath(int i, int start, int *path, int *parent, int *distance){
	int count, temp, j;

	// removes "d(0-0) = 0	path: 0" from print output
	if (i != start){
		temp = distance[i];
		printf("\nd(%d-%d) = %d\tpath: ", start, i, temp);
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
void dijkstra(int graph[DIM][DIM], int start){
	int i, j, k, temp, count, minDistance, next;
	int distance[DIM], parent[DIM], path[DIM];
	bool visited[DIM];

	// loop 1: initializing
	for (i = 0; i < DIM; i++){
		distance[i] = graph[start][i]; // vector of distances from start to adjescent nodes 
		parent[i] = 0; // no exisiting parents yet
		visited[i] = false; // no nodes visited yet
		path[i] = INF;
	}

	distance[start] = 0; // set distance of start node to itself to zero
	visited[start] = true; /* not necessary, since zero is minDistance anyway;
	circumvents the second if statement in loop 3 */

	// loop 2: iterating over number of nodes
	for (i = 0; i < DIM; i++){
		minDistance = INF; // initializing minimum distance in every iteration

		/* loop 3: finding node with lowest distance value in distance vector
		(if node was not already visited); already processed nodes are
		excluded according to their visited status (true) */ 
		for (j = 0; j < DIM; j++){
			if (!visited[j]){
				if (distance[j] < minDistance){
					minDistance = distance[j]; // minimal distance value
					next = j; // index of the closest node
				}
			}
		}
		
		/* closest node in current distance list is found and thus given a
		processed flag, which will leave it untouched in further iterations */
		visited[next] = true;

		printf("distance:\t");
		printVector(distance);

		/* loop 4: updating the distance vector with distances from next node 
		(graph[next][j]) -> including new connections (replacing INF) and
		shorter indirect paths; by adding the minDistance we keep track of the 
		distance already travelled w.r.t the start node; already processed
		nodes must again be excluded */
		for (j = 0; j < DIM; j++){
			if (!visited[j]){
				temp = graph[next][j];
				if (minDistance + temp < distance[j]){
					distance[j] = minDistance + temp;
					parent[j] = next;
				}
			}
		}
		printf("distance:\t");
		printVector(distance);
	}

	// printing
	printf("Adjacency matrix:\n-----------------\n");
	printMatrix(graph);
	printf("\n\n");
	printf("Paths:\n------");
	for (i = 0; i < DIM; i++){
		printPath(i, start, path, parent, distance);
	}
	printf("\n");

	return;
}


int main(int argc, char *argv[]){
	int i, j;
	int exampleGraph[DIM][DIM];

	// ínitializing exampleGraph
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
	dijkstra(exampleGraph, 0);

	return 0;
}


