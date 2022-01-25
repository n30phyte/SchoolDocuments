#include <cstdint>
#include <iostream>
#include <vector>

using namespace std;

int main() {
  // Declare your variables
  int32_t friends;
  vector<vector<int32_t>> numbers;
  cin >> friends;

  numbers.reserve(friends);

  for (int32_t i = 0; i < friends; i++) {
    int32_t count;
    cin >> count;
    for (int32_t j = 0; j < count; j++) {
      int32_t number;
      cin >> number;
      numbers[i].emplace_back(number);
    }
  }
  unsigned int friend_number;
  cin >> friend_number;

  for (auto item : numbers[friend_number]) {
    cout << item << " ";
  }
  return 0;
}
