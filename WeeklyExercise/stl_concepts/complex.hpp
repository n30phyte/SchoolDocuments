/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 275, Winter 2020
 *  Weekly Exercise 6
 ********************************/
#pragma once

#include <iostream>
#include <utility>

class Complex {
 public:
  /**
   * Constructor for Complex type.
   *
   * Complex(a, b) construct a number in the form a + ib
   *
   * @param real: Real part of the number
   * @param imaginary: Imaginary part of the number
   */
  Complex(long long real, long long imaginary);

  /**
   * Copy constructor for Complex type. Copies an existing Complex number to a new instance
   *
   * @param num: Existing complex number
   */
  Complex(const Complex& num);

  /**
   * Get the real part of (a in a + ib) a complex number.
   *
   * @return real part of number
   */
  long long Real() const;

  /**
   * Get the imaginary part (b in a + ib) of a complex number.
   *
   * @return long long value
   */
  long long Imaginary() const;

  /**
   * Returns the conjugate pair of an existing complex number
   *
   * @return a new Complex instance
   */
  Complex Conjugate();

  /**
   * Addition operator
   *
   * @param left Left hand side of the operation
   * @param right Right hand side of the operation
   * @return New instance of Complex
   */
  friend Complex operator+(const Complex& left, const Complex& right);
  /**
   * Subtraction operator
   *
   * @param left Left hand side of the operation
   * @param right Right hand side of the operation
   * @return New instance of Complex
   */
  friend Complex operator-(const Complex& left, const Complex& right);
  /**
   * Multiplication operator
   *
   * @param left Left hand side of the operation
   * @param right Right hand side of the operation
   * @return New instance of Complex
   */
  friend Complex operator*(const Complex& left, const Complex& right);

  /**
   * Negation operator
   *
   * @param left The number to be negated
   * @return Negated number
   */
  friend Complex operator-(const Complex& left);

  /**
   * Operator for easier cout
   *
   * @param out Output stream
   * @param complex Complex number to print
   * @return stream with number pushed in
   */
  friend std::ostream& operator<<(std::ostream& out, const Complex& complex);

 private:
  std::pair<long long, long long> data;
};
