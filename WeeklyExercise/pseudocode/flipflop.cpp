#include <cstdint>
#include <iostream>
#include <algorithm>

void flipFlopSort(uint_fast32_t* array, int n)
{
    if (n == 2) {
        if (array[0] > array[1]) {
            std::swap(array[0], array[1]);
        }
    } else {
        flipFlopSort(array, (2 * n) / 3);
        flipFlopSort(array + (n / 3), (2 * n) / 3);
        flipFlopSort(array, (2 * n) / 3);
    }
}

int main()
{
    int n;
    uint_fast32_t array[200];

    std::cin >> n;

    for (auto i = 0; i < n; i++) {
        std::cin >> array[i];
    }
    std::cout << "here" << std::endl;
    flipFlopSort(array, n);
}