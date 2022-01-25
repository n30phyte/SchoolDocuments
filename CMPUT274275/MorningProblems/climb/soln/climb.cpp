#include <iostream>
#include <vector>
using namespace std;

int main()
{
    // I've heard that climbing is a good workout for your legs.
    // But I haven't moved from my chair in years, so I might not be the best person to ask.
    // Good luck with the problem! The term is almost over! :)

    // Read in the input
    int n;
    std::cin >> n;

    vector<int> numbers(n);
    vector<int> outputs;

    for (auto i = 0; i < n; i++) {
        std::cin >> numbers[i];
    }

    for (auto i = 0; i < n - 1; i++) {
        if (numbers[i] >= numbers[i + 1]) {
            outputs.push_back(0);
        } else {
            auto counter = 0;
            while ((numbers[i + counter] < numbers[i + counter + 1]) && (i + counter + 1 < n)) {
                counter++;
            }
            outputs.push_back(counter);
        }
    }

    for (auto i : outputs) {
        std::cout << i << " ";
    }
    std::cout << 0 << endl;
    return 0;
}