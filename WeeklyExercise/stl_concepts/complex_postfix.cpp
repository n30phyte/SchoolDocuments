#include <iostream>
#include <utility>
#include <stack>

int main() {

    std::stack<std::pair<long long, long long>> OperationStack;
    std::string input;

    bool done = false;

    while(!done) {
        std::cin >> input;

        if (input == "V") {
            long long real;
            long long complex;

            std::cin >> real;
            std::cin >> complex;

            OperationStack.push(std::make_pair(real, complex));
        } else if (input == "B") {

            auto right = OperationStack.top();
            OperationStack.pop();

            auto left = OperationStack.top();
            OperationStack.pop();

            std::cin >> input;

            if (input == "+") {
                OperationStack.push(std::make_pair(left.first + right.first, left.second + right.second));
            } else if (input == "-") {
                OperationStack.push(std::make_pair(left.first - right.first, left.second - right.second));
            } else if (input == "*") {
                OperationStack.push(std::make_pair((left.first * right.first) - (left.second * right.second),
                                                   (left.first * right.second) + (left.second * right.first)));
            }
        } else if (input == "U") {
            auto target = OperationStack.top();
            OperationStack.pop();

            std::cin >> input;

            if (input == "-") {
                OperationStack.push(std::make_pair(-target.first, -target.second));
            } else if (input == "c") {
                OperationStack.push(std::make_pair(target.first, -target.second));
            }
        } else if (input == "S") {
            auto output = OperationStack.top();
            std::cout << output.first << " " << output.second << std::endl;
            done = true;
        } else {
            std::cout << "Error: Incorrect input." << std::endl;
            std::cin.sync();
        }
    }
    return 0;
}
