#include <iomanip>
#include <iostream>
#include <vector>

unsigned int gcd(unsigned int a, unsigned int b)
{
}

unsigned int phi(unsigned int n)
{
    std::vector<unsigned int> primes;
    primes.reserve(n / 4);
    unsigned int output = n;
    unsigned int n_prime = n;
    for (auto i = 2; (i * i) < n; i += 1) {
        if ((i % 2) != 0 || i == 2) {
            if ((n_prime % i) == 0) {
                primes.emplace_back(i);
                while ((n_prime % i) == 0) {
                    n_prime /= i;
                }
            }
        }
    }

    if (n_prime != 1) {
        primes.emplace_back(n_prime);
    }

    for (auto prime : primes) {
        output /= prime;
        output *= (prime - 1);
    }
    return output;
}

int main()
{
    unsigned int input;
    unsigned int totient;

    std::cin >> input;

    totient = phi(input);

    std::cout << std::setw(11) << std::left << "n"
              << " = " << input << std::endl;
    std::cout << std::setw(11) << std::left << "phi(n)"
              << " = " << totient << std::endl;
    std::cout << std::setw(11) << std::left << "phi(n)/n"
              << " = " << std::fixed << std::setprecision(5)
              << (float)totient / (float)input << std::endl;
    if (input >= (1 << 15)) {
        std::cout << std::setw(11) << std::left << "15-bit test"
                  << " = " << std::fixed << std::setprecision(5) << std::endl;
    }
}
