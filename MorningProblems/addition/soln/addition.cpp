#include <iostream>
#include <vector>

using namespace std;

int main()
{

    int count = 0;
    // Read the input
    cin >> count;
    // Solve the problem

    // Declare your variables
    std::vector<int> number1(count);
    std::vector<int> number2(count);
    std::vector<int> output(count + 1);

    for (auto i = 0; i < count; i++) {
        cin >> number1[i];
    }
    for (auto i = 0; i < count; i++) {
        cin >> number2[i];
    }

    int carry = 0;
    for (auto i = count - 1; i >= 0; i--) {
        output[i + 1] = number1[i] + number2[i] + carry;
        if (output[i + 1] >= 10) {
            carry = 1;
            output[i + 1] = output[i + 1] - 10;
        } else {
            carry = 0;
        }
    }
    if (carry != 0) {
        output[0] = carry;
    }
    if (output[0] == 0) {
        output.erase(output.begin());
    }

    for (auto i = 0; i < output.size(); i++) {
        cout << output[i];
        if (i != output.size() - 1) {
            cout << " ";
        }
    }
    cout << endl;

    return 0;
}
