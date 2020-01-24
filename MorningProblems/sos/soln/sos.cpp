#include <iostream>
#include <cmath> // for sqrt

using namespace std;

// Compute the greatest integer d such that d*d <= n
// (i.e. the floor of the square root).
//
// You may use this function or ignore it, it's up to you.
unsigned long integer_sqrt(unsigned long x)
{
    unsigned long d = sqrt(x);

    // should iterate at most once, probably none
    while ((d + 1) * (d + 1) <= x) {
        ++d;
    }

    // probably does not iterate even once
    while (d * d > x) {
        --d;
    }

    // now at this point we know (d+1)*(d+1) > x yet d*d <= x

    return d;
}

int main()
{
    unsigned long number;
    cin >> number;

    unsigned long a_max = integer_sqrt(number / 2);
    bool isSOS = false;

    for (long a = a_max; a >= 0; a--) {
        unsigned long b = integer_sqrt(number - pow(a, 2));
        if ((pow(a, 2) + pow(b, 2)) == number) {
            isSOS = true;
            break;
        }
    }

    if (isSOS) {
        cout << "sum of squares" << endl;
    } else {
        cout << "not sum of squares" << endl;
    }

    return 0;
}
