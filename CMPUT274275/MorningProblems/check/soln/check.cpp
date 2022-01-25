#include <iostream>

using namespace std;

int main()
{
    // Read the input
    int d1, d2, d3, d4;
    cin >> d1 >> d2 >> d3 >> d4;
    bool correct;

    // Put your code here!
    int sum = d1 + (d2 * 2) + (d3 * 3) + (d4 * 4);
    correct = (sum % 5 == 0);

    // Output the solution
    if (correct) {
        cout << "yes" << endl;
    } else {
        cout << "no" << endl;
    }
    return 0;
}
