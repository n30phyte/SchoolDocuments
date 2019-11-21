#include <cmath> // for sqrt
#include <iostream>

int main()
{
    // read in the integer
    // NOTE: look *carefully* at the maximum value of the number
    // in the problem description
    unsigned int number = 0;
    std::cin >> number;
    // determine if the integer is prime
    // good luck and remember that composite numbers are for chumps :)
    if (number == 2) {
        std::cout << "prime" << std::endl;
        return 0;
    }
    if ((number % 2) == 0) {
        // even number
        std::cout << "not prime" << std::endl;
        return 0;
    } else {
        unsigned int root = std::sqrt(number);
        if (root * root == number) {
            std::cout << "not prime" << std::endl;
            return 0;
        } else {
            for (auto i = 3; i < root; i++) {
                if ((number % i) == 0) {
                    std::cout << "not prime" << std::endl;
                    return 0;
                }
            }
            std::cout << "prime" << std::endl;
            return 0;
        }
    }
    // print the answer
}
