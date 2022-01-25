#include <iostream>

using namespace std;

int main()
{
    unsigned int n; // number of chair
    unsigned int k; // how many friends
    unsigned int s; // Shift value

    cin >> n >> k >> s;
    long prizes[n];
    for (auto i = 0; i < n; i++) {
        int prize;
        cin >> prize;
        prizes[i] = prize;
    }

    auto total = 0;

    for (auto i = 0; i < k; i++) {
        int knight_number = 0;
        cin >> knight_number;
        auto shifted_number = knight_number + s;

        if (shifted_number >= n) {
            shifted_number = shifted_number - n;
        }
        total += prizes[shifted_number];
    }

    cout << total << endl;

    return 0;
}
