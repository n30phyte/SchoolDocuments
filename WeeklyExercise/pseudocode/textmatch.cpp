#include <cmath>
#include <cstring>
#include <iostream>

/**
 * Computes step sizes for the textMatch algorithm to use.
 *
 * @param steps: pointer to where the output should be stored.
 * @param subtr: the char array of the substring to be searched for.
 */
void compute_steps(int32_t* steps, const char* substr)
{
    steps[0] = -1;
    auto k = -1;

    for (auto i = 1; i < strlen(substr); i++) {
        while (k >= 0 && substr[k + 1] != substr[i]) {
            k = steps[k];
        }
        if (substr[k + 1] == substr[i]) {
            k++;
        }
        steps[i] = k;
    }
}

/**
 * Text match algorithm, provided in the assignment.
 *
 * @param s: The substring to be searched for.
 * @param t: the text to search through.
 */
void textMatch(const char* s, const char* t)
{
    int32_t step[strlen(s)];

    // Computes stetps in a seperate function.
    compute_steps(step, s);

    int32_t m = -1;

    // Goes through t to search for substring
    for (auto i = 0; i < strlen(t); i++) {
        while (m >= 0 && s[m + 1] != t[i]) {
            m = step[m];
        }

        // If the current index of the text is in the substring;
        if (s[m + 1] == t[i]) {
            m++;
        }

        // If substring matched, print out index of where it started
        if (m == (strlen(s) - 1)) {
            std::cout << i + 1 - strlen(s) << " ";
            m = step[m];
        }
    }

    std::cout << std::endl;
}

int main()
{
    char s[100002];
    char t[100002];

    std::cin >> s >> t;

    textMatch(s, t);

    return 0;
}