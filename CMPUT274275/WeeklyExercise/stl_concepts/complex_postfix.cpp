/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 275, Winter 2020
 *  Weekly Exercise 6
 ********************************/

#include <iostream>
#include <stack>

#include "complex.hpp"

int main() {
    // Stack to store following operations
    std::stack<Complex> OperationStack;
    std::string input;

    bool done = false;

    while (!done) {
        std::cin >> input;

        if (input == "V") {
            long long real;
            long long imaginary;

            std::cin >> real;
            std::cin >> imaginary;

            OperationStack.push(Complex(real, imaginary));
        } else if (input == "B") {
            auto right = OperationStack.top();
            OperationStack.pop();

            auto left = OperationStack.top();
            OperationStack.pop();

            std::cin >> input;

            if (input == "+") {
                OperationStack.push(left + right);
            } else if (input == "-") {
                OperationStack.push(left - right);
            } else if (input == "*") {
                OperationStack.push(left * right);
            }
        } else if (input == "U") {
            auto target = OperationStack.top();
            OperationStack.pop();

            std::cin >> input;

            if (input == "-") {
                OperationStack.push(-target);
            } else if (input == "c") {
                OperationStack.push(target.Conjugate());
            }

        } else if (input == "S") {
            std::cout << OperationStack.top() << std::endl;
            done = true;

        } else {
            std::cout << "Error: Incorrect input." << std::endl;
            std::cin.sync();
        }
    }

    return 0;
}
