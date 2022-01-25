#include <iostream>

using namespace std;

int main()
{
    // here is a template to help get you started
    // declare all the necessary variables
    int x1, y1, x2, y2, x3, y3;

    // read in the input
    cin >> x1 >> y1;
    cin >> x2 >> y2;
    cin >> x3 >> y3;

    int answerx, answery;

    // now solve the problem
    // don't forget a semi-colon at the end of each line :)

    if (x1 == x2) {
        answerx = x3;
    } else if (x1 == x3) {
        answerx = x2;
    } else if (x2 == x3) {
        answerx = x1;
    }

    if (y1 == y2) {
        answery = y3;
    } else if (y1 == y3) {
        answery = y2;
    } else if (y2 == y3) {
        answery = y1;
    }

    // print the output
    cout << answerx << ' ' << answery << endl;

    return 0;
}
