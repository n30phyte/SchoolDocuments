#include <algorithm>
#include <iostream>
#include <unordered_set>
#include <vector>

using namespace std;

int main() {
  // Read in the input
  unsigned short n = 0;
  cin >> n;

  vector<int> first;
  vector<int> second;
  unordered_multiset<int> third;
  unordered_set<int> sums;

  for (auto i = 0; i < n; i++) {
    int number;
    cin >> number;
    first.emplace_back(number);
  }
  for (auto j = 0; j < n; j++) {
    int number;
    cin >> number;
    second.emplace_back(number);
  }
  for (auto k = 0; k < n; k++) {
    int number;
    cin >> number;
    third.emplace(number);
  }

  auto initial = third.size();

  for (auto a : first) {
    for (auto b : second) {
      third.erase(a + b);
    }
  }
  auto final = third.size();
  cout << initial - final << endl;

  return 0;
}