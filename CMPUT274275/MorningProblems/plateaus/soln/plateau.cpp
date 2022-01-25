#include <iostream>
#include <algorithm> // for max()

using namespace std;

int main()
{
    int input_length = 0;

    int current_longest = 0;

    cin >> input_length;

    int current_count = 0;
    int current_number = 0;
    for (int i = 0; i < input_length; i++) {
        int temp_number = 0;
        cin >> temp_number;
        if (temp_number == current_number) {
            current_count++;
        } else {
            if (current_count > current_longest) {
                current_longest = current_count;
            }
            current_count = 1;
            current_number = temp_number;
        }
    }
    // print the output

    cout << current_longest << endl;
    return 0;
}
