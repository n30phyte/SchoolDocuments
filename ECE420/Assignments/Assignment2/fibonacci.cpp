#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int fib_tasks(int n) {
    if (n < 2)
        return n;
    else {
        int i, j;
#pragma omp task shared(j) firstprivate(n)
        i = fib_tasks(n - 1);
#pragma omp task shared(i) firstprivate(n)
        j = fib_tasks(n - 2);

#pragma omp taskwait
        return i + j;
    }
}

int fib_sections(int n) {
    if (n < 2)
        return n;
    else {
        int i, j;

#pragma omp parallel sections
        {
#pragma omp section
            {
                i = fib_sections(n - 1);
            }
#pragma omp section
            {
                j = fib_sections(n - 2);
            }
        }

        return i + j;
    }
}

int main(int argc, char *argv[]) {
    int result;
    printf("Doing sequential fibonacci:\n");
    result = fib_tasks(5);
    printf("Result is %d\n", result);
    return 0;
}
