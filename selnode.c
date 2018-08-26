#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int cmpfunc (const void * a, const void * b) {
   return ( *(int*)a - *(int*)b );
}

int main(int argc, char *argv[]){
    int n, i;
    int a, b, l, s;
    int id[1024];
    char *token;

    if (argc < 3){
        return 0;
    }

    n = atoi(argv[1]);

    token = strtok(argv[2], ",");
    s = 0;
    while(token != NULL){
        l = sscanf(token, "%d-%d", &a, &b);
        if (l == 1){
            id[s] = a;
            s++;
        }
        else if (l == 2){
            for(i = a; i <= b; i++){
                id[s] = i;
                s++;
            }
        }
        else{
            printf("Error parsing token: %s\n", token);
            break;
        }
        token = strtok (NULL, ",");
    }

    if (n > s){
        printf("Error: n > s\n");
        n = s;
    }

    qsort(id, s, sizeof(int), cmpfunc);

    for(i = 0; i < n; i++){
        if (i > 0){
            printf(",");
        }
        printf("%d", id[i]);
    }

    return 0;
}