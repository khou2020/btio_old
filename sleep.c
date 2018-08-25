#include <unistd.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char *argv[]){
    int rank, t;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    if (rank == 0){
        if (argc > 1){
            t = atoi(argv[1]);
        }
        else{
            t = 0;
        }
    }
    MPI_Bcast(&t, 1, MPI_INT, 0, MPI_COMM_WORLD);
    sleep(t);
    MPI_Finalize();
    return 0;
}