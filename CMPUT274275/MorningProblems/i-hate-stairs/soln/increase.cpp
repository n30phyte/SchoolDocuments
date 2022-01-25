#include <iostream>

using namespace std;

int main()
{
    int n;
    int array[1000];
    int answer = 0;

    cin >> n;
    for (int i = 0; i < n; ++i) {
        cin >> array[i];
    }

    // now compute the answer and put it into the "answer" variable
    // good luck :) you've got this!
    // p.s. don't forget your semi-colons

    for (auto j = 0; j < n - 1; j++) {
        auto difference = array[j+1] - array[j];
        if (difference > 0) {
            answer += difference;
        }
    }

    // output the result
    cout << answer << endl;

    return 0;
}
