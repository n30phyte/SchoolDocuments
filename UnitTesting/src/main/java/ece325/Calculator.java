package ece325;

/**
 * Assignment 4 Part 2: Unit Testing <br />
 * The calculator to run the test cases
 */
// TODO: Assignment 4 Part 2 -- Create the Calculator here
public class Calculator {

    public Double add(double a, double b) {
        return a + b;
    }

    public Double subtract(double a, double b) {
        return a - b;
    }

    public Double multiply(double a, double b) {
        return a * b;
    }

    public Double divide(double a, double b) {
        return a / b;
    }

    public Double squareRoot(double a) {
        return Math.sqrt(a);
    }

    public Double[] getRoots(double a, double b, double c) {
        double discriminant = Math.pow(b, 2) - 4 * a * c;

        if (discriminant < 0) {
            return new Double[]{Double.NaN, Double.NaN};
        } else if (discriminant == 0) {
            return new Double[]{-b / (2 * a)};
        } else {
            return new Double[]{(-b + squareRoot(discriminant)) / (2 * a), (-b - squareRoot(discriminant)) / (2 * a)};

        }
    }


}