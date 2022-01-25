/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 274, Fall 2019
 *  Weekly Exercise 7
 ********************************/

#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <vector>

/**
 * Find greatest common divisor of two numbers
 *
 * Algorithm from slides in class.
 *
 * @param a: First value.
 * @param b: Second value.
 * @return Greatest Common Divisor
 */
unsigned int gcd(unsigned int a, unsigned int b)
{
    while (b > 0) {
        a %= b;
        std::swap(a, b);
    }
    return a;
}

/**
 * Coprime count estimation.
 *
 * Estimate the number of positive integers that are coprime to n. Brute force algorithm by checking if gcd(n, x) == 1,
 * where 2^14 <= x < 2^14.
 *
 * @param n: The number to calculate amount of comprimes.
 * @return: The number of coprimes.
 */
unsigned int coprimeCount(unsigned int n)
{
    unsigned int counter = 0;
    for (unsigned int i = (1 << 14); i < (1 << 15); i++) {
        if (gcd(n, i) == 1) {
            counter++;
        }
    }

    return counter;
}

/**
 * Euler's Totient function
 *
 * Count the number of positive integers that are coprime to n.
 *
 * @param n: The number to calculate amount of comprimes.
 * @return: The number of coprimes.
 */
unsigned int phi(unsigned int n)
{
    std::vector<unsigned int> primes;

    unsigned int output = n;
    unsigned int n_prime = n;

    // Optimization trial division case for when the prime is 2
    if ((n_prime % 2) == 0) {
        primes.emplace_back(2);
        while ((n_prime % 2) == 0) {
            n_prime /= 2;
        }
    }

    // Trial division for primes [3, sqrt(n))
    auto i = 3;

    while (i < std::sqrt(n) && i != 1) {
        if ((n_prime % i) == 0) {
            primes.emplace_back(i);
            while ((n_prime % i) == 0) {
                n_prime /= i;
            }
        }
        i += 2;
    }

    // Check if number itself is prime
    if (n_prime != 1) {
        primes.emplace_back(n_prime);
    }

    // Do the pi operation.
    for (auto prime : primes) {
        output /= prime;
        output *= (prime - 1);
    }
    return output;
}

/**
 * Main function of the program
 *
 * Gets user input and calls appropriate functions.
 */
int main()
{
    unsigned int input;

    std::cin >> input;

    unsigned int totient = phi(input);

    std::cout << std::setw(11) << std::left << "n"
              << " = " << input << std::endl;
    std::cout << std::setw(11) << std::left << "phi(n)"
              << " = " << totient << std::endl;
    std::cout << std::setw(11) << std::left << "phi(n)/n"
              << " = " << std::fixed << std::setprecision(5) << (float)totient / (float)input << std::endl;
    if (input >= (1 << 15)) {
        std::cout << std::setw(11) << std::left << "15-bit test"
                  << " = " << std::fixed << std::setprecision(5) << (float)coprimeCount(input) / (float)(1 << 14)
                  << std::endl;
    }
}
