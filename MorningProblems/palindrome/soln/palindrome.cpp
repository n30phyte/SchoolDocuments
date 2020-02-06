#include <algorithm>
#include <cstring>
#include <iostream>
#include <unordered_map>
using namespace std;

int longest_palindrome(char* text, int start, int end)
{
    static unordered_map<string, int> cache;

    // oops?
    if (start > end) {
        return 0;
    }

    // Size 1
    if (start == end) {
        return 1;
    }

    string search_text(text + start, text + end);

    if (cache.find(search_text) == cache.end()) {

        // possible palindrome (start == end)
        if (text[start] == text[end]) {
            // repeat
            cache[search_text] = longest_palindrome(text, start + 1, end - 1) + 2;
        }
        cache[search_text] = max(longest_palindrome(text, start, end - 1), longest_palindrome(text, start + 1, end));
    }

    return cache[text];
}

int main()
{
    // input contains the input string
    // n is the length of the input string
    char input[200001];

    // read in line and determine out how long it is
    cin >> input;
    int n = strlen(input);

    cout << longest_palindrome(input, 0, n - 1) << endl;
    return 0;
}
