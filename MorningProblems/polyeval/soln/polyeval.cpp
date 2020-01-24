#include <iostream>
#include <vector>
#include <cmath>
using namespace std;
unsigned long mulmod(unsigned long value1, unsigned long value2, unsigned long modulus)
{
    unsigned long answer = 0;

    // Take the modulus of `value2' to prevent overflow. This is justified
    // through modulus rules.
    value2 %= modulus;

    while (value1 > 0) {

        // If the current bit is a one
        if (value1 & 0b1) {
            answer = (answer + value2) % modulus;
        }

        // Multiply 'value2' by two with bit shift, then take the modulus.
        value2 = (value2 << 1) % modulus;

        // Divide 'value1' by two by bit shift
        value1 >>= 1;
    }

    // The last step is to take the modulus of the answer.
    answer %= modulus;

    return answer;
}

/**
 * 32 bit Modular Exponentiation.
 *
 * Calculates (base^exponent) % modulus. Procedure is described in the class notes.
 *
 * @param base the base of exponentiation.
 * @param exponent the exponent of exponentiation.
 * @param modulus the modulus of the operation.
 * @return answer the result of fast expoenetiation.
 */
unsigned long powmod(unsigned long base, unsigned long exponent, unsigned long modulus)
{
    unsigned long answer = 1;
    unsigned long pow_x = base % modulus;

    while (exponent > 0) {
        if (exponent & 0b1) {
            answer = mulmod(answer, pow_x, modulus);
        }

        // Essentially divide the exponent by two using bitwise operators.
        exponent >>= 1;
        pow_x = mulmod(pow_x, pow_x, modulus);
    }

    return answer;
}
int main()
{
    unsigned long d;
    unsigned long x;
    unsigned long m;

    cin >> d >> x >> m;

    x = x % m;

    auto output = 0;
    for (unsigned long i = 0; i <= d; i++) {
        unsigned long input;
        cin >> input;
        output += (input % m) * powmod(x, i, m) % m;
    }

    cout << output % m << endl;
    return 0;
}
