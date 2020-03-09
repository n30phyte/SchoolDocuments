/*********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 275, Winter 2020
 *  Weekly Exercise 6
 ********************************/

#include "complex.hpp"


Complex::Complex(long long real, long long imaginary) { data = std::make_pair(real, imaginary); }

Complex::Complex(const Complex& num) : Complex(num.Real(), num.Imaginary()){};

long long Complex::Real() const { return data.first; }

long long Complex::Imaginary() const { return data.second; }


Complex Complex::Conjugate() { return Complex(this->Real(), -this->Imaginary()); }


Complex operator+(const Complex& left, const Complex& right) {
  return Complex(left.Real() + right.Real(), left.Imaginary() + right.Imaginary());
}

Complex operator-(const Complex& left, const Complex& right) {
  return Complex(left.Real() - right.Real(), left.Imaginary() - right.Imaginary());
}
Complex operator*(const Complex& left, const Complex& right) {
  auto real = (left.Real() * right.Real()) - (left.Imaginary() * right.Imaginary());
  auto imaginary = (left.Real() * right.Imaginary()) + (left.Imaginary() * right.Real());
  return Complex(real, imaginary);
}

Complex operator-(const Complex& left) { return Complex(-left.Real(), -left.Imaginary()); }

std::ostream& operator<<(std::ostream& out, const Complex& complex) {
  out << complex.Real() << " " << complex.Imaginary();
  return out;
}
