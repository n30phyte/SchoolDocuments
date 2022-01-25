#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int getMax(int *arr, int size) {
    int mx = -1;
    for (int i = 0; i < size; ++i)
        if (arr[i] > mx) mx = arr[i];
    return mx;
}

void radixSort(int *arr, int *tmp, int size) {
    int mx = getMax(arr, size);
    for (int exp = 1; mx / exp > 0; exp *= 10) {
        int bucket[10] = {0};
        for (int i = 0; i < size; ++i)
            ++bucket[(arr[i] / exp) % 10];
        for (int i = 1; i < 10; ++i)
            bucket[i] += bucket[i - 1];
        for (int i = size - 1; i >= 0; --i)
            tmp[--bucket[(arr[i] / exp) % 10]] = arr[i];
        for (int i = 0; i < size; ++i)
            arr[i] = tmp[i];
    }
}

int main(int argc, char *argv[]) {
    int number_of_elements;
    int *a, *b;
    if (argc <= 1 || !argc) {
        exit(0);
    }
    sscanf(argv[1], "%d", &number_of_elements);
    if (number_of_elements < 10 || number_of_elements > 10000000) {
        exit(0);
    }
    a = malloc(number_of_elements * sizeof(int));
    b = malloc(number_of_elements * sizeof(int));
    srand(time(NULL));
    for (int i = 0; i < number_of_elements; i++)
        a[i] = rand();

    fprintf(stderr, "%s\n", "# Start Radixsort");
    radixSort(a, b, number_of_elements);
    fprintf(stderr, "%s\n", "# End Radixsort");

#ifdef DEBUG
    printf("The sorted array is : \n");
    for (int i = 0; i < number_of_elements; i++)
       printf("%d \n", a[i]);
#endif
}
