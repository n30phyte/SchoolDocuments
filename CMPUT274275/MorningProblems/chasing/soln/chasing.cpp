#include <iostream>
#include <unordered_set>

int main()
{
    uint_fast32_t S = 0;
    uint_fast16_t A = 0;

    std::cin >> S;
    std::cin >> A;

    int_fast64_t values[A];

    for (auto i = 0; i < A; i++) {
        int_fast64_t input = 0;
        std::cin >> input;
        values[i] = input;
    }

    bool cycle = false;

    std::unordered_set<uint_fast32_t> previous_addresses;

    auto address = 0;
    while (!cycle) {
        auto content = values[address];
        if (content >= 0) {
            // It's an address
            // Check if in list
            if (previous_addresses.find(content) != previous_addresses.end()) {
                cycle = true;
            } else {
                previous_addresses.emplace(content);
                address = content - S;
            }
        } else {
            std::cout << content << std::endl;
            return 0;
        }
    }

    std::cout << "There was a cycle" << std::endl;

    return 0;
}
