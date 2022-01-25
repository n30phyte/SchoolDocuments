#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iostream>

/**
 * Sorts the array with the flipflop sort algorithm
 *
 * @param input_array: Pointer to the array to be sorted.
 * @param n: size of the array to be sorted.
 */
void flipFlopSort(uint_fast32_t* input_array, int n)
{
    // Base case
    if (n == 2) {
        if (input_array[0] > input_array[1]) {
            std::swap(input_array[0], input_array[1]);
        }
    } else {
        int sort_size = std::ceil((float)(2 * n) / 3);

        flipFlopSort(input_array, sort_size);
        flipFlopSort(input_array + n - sort_size, sort_size);
        flipFlopSort(input_array, sort_size);
    }
}

int main()
{
    int n;
    uint_fast32_t array[200];

    std::cin >> n;

    if (n == 1) {
        // Edge case. if array only has 1 item, it is sorted by definition.
        std::cin >> array[0];
        std::cout << array[0];
    } else {

        for (auto i = 0; i < n; i++) {
            std::cin >> array[i];
        }

        flipFlopSort(array, n);

        for (auto i = 0; i < n; i++) {
            std::cout << array[i];
            if (i != n - 1) {
                std::cout << " ";
            }
        }
    }

    std::cout << std::endl;
}