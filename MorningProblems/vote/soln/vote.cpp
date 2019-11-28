#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

int main()
{
    int n = 0;
    int vote[1001];

    // keep reading until we see 0
    while (cin >> vote[n] && vote[n] != 0) {
        ++n;
    }
    sort(vote, vote + n);

    vector<int> candidate_votes;
    candidate_votes.reserve(vote[n - 1]);

    int maxVotes = 0;
    int candidateMax = 0;
    bool isTie = false;

    for (auto i = 1; i <= vote[n - 1]; i++) {
        auto currentCount = count(vote, vote + n, i);
        if (currentCount > maxVotes) {
            maxVotes = currentCount;
            candidateMax = i;
            isTie = false;
        } else if (currentCount == maxVotes) {
            isTie = true;
        }
    }

    if (isTie) {
        cout << "tie" << endl;
    } else {
        cout << candidateMax << endl;
    }

    return 0;
}
