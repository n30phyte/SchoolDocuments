#include <iostream>
#include <queue>  // may or may not be useful

using namespace std;

int main() {
  unsigned long n;
  int q;
  cin >> n >> q;

  queue<unsigned long> students;

  for (unsigned long i = 0; i < n; i++) {
    students.emplace(i);
  }

  unsigned long last_removed = -1;

  while (students.size() > 1) {
    for (auto i = 0; i < q; i++) {
      if (students.front() == last_removed) {
        students.pop();
      } else {
        students.emplace(students.front());
        students.pop();
      }
    }
    last_removed = students.front();
  }

  cout << students.front << endl;

  return 0;
}
