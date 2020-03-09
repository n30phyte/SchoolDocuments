#include <iostream>
#include <queue>
#include <string>
#include <unordered_set>

int main() {
  int64_t S = 0;
  uint16_t A = 0;
  uint16_t N = 0;

  std::cin >> S;
  std::cin >> A;
  std::cin >> N;

  int64_t values[A] = {0};
  for (uint_fast16_t i = 0; i < N; i++) {
    std::string input;
    std::cin >> input;
    if (input == "S") {
      int_fast64_t ptr;
      int_fast64_t value;
      std::cin >> ptr >> value;
      values[ptr - S] = value;
    } else if (input == "P") {
      int_fast64_t ptr;
      int_fast64_t value;
      std::cin >> ptr >> value;
      values[ptr - S] = value;

    } else if (input == "C") {
      bool cycle = false;

      std::unordered_set<uint_fast32_t> previous_addresses;

      int64_t address;
      std::cin >> address;

      while (!cycle) {
        if (address - S >= A || address < S) {
          std::cout << "Out of bounds" << std::endl;
          break;
        }
        auto content = values[address - S];
        if (content >= S) {
          // It's an address
          // Check if in list
          if (previous_addresses.find(content) != previous_addresses.end()) {
            cycle = true;
            std::cout << "There was a cycle" << std::endl;
            break;

          } else {
            previous_addresses.emplace(content);
            address = content;
          }
        } else {
          if (content < 0) {
            std::cout << content << std::endl;
          } else {
            std::cout << "Out of bounds" << std::endl;
          }
          break;
        }
      }
    }
  }

  return 0;
}
