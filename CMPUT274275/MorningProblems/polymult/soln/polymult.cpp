#include <cstring>
#include <iostream>

using namespace std;

int main()
{
    int m = 0;
    int n = 0;

    cin >> m >> n;

    m++;
    n++;

    int poly1[m];
    int poly2[n];

    int polyout[n + m - 1];

    memset(polyout, 0, sizeof(polyout));

    for (auto i = 0; i < m; i++) {
        auto temp = 0;
        cin >> temp;
        poly1[i] = temp;
    }

    for (auto i = 0; i < n; i++) {
        auto temp = 0;
        cin >> temp;
        poly2[i] = temp;
    }

    for (auto i = 0; i < m; i++) {
        auto a = poly1[i];
        for (auto j = 0; j < n; j++) {
            auto b = poly2[j];
            polyout[i + j] += (a * b);
        }
    }

    for (auto val : polyout) {
        cout << val << " ";
    }

    return 0;
}
