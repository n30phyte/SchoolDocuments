#include <iostream>
#include <numeric>
#include <vector>

using namespace std;

int main() {
  unsigned int n, q;

  cin >> n >> q;
  vector<unsigned int> calls;

  unsigned int num_call;
  cin >> num_call;

  calls.emplace_back(num_call);

  for (auto i = 1; i < n; i++) {
    cin >> num_call;
    calls.emplace_back(num_call + calls[i - 1]);
  }

  for (auto i = 0; i < q; i++) {
    unsigned int start_time;
    unsigned int end_time;

    cin >> start_time >> end_time;
    if (start_time == 1) {
      cout << calls[end_time - 1] << endl;
    } else {
      cout << calls[end_time - 1] - calls[start_time - 2] << endl;
    }
  }

  return 0;
}