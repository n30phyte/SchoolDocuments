package ece325.TestCases;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import ece325.*;

/**
 * JUnit test class for solving square roots
 */
public class SquareRootTests {


    private Calculator calc;
    private double epsilon = 0.0000001;

    @Before
    public void setUp() throws Exception {
        calc = new Calculator();
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testRandomPositiveSquareRoot() {
        // Ensure no zeroes by shifting lowest number right a little bit
        double a = (Math.random() + epsilon) * 2000000;

        assertEquals(a, calc.squareRoot(a) * calc.squareRoot(a), epsilon);
    }

    @Test
    public void testRandomNegativeSquareRoot() {
        // Ensure no zeroes by shifting lowest number right a little bit
        double a = (Math.random() + epsilon) * -2000000;

        assertEquals(Double.NaN, calc.squareRoot(a), epsilon);
    }

    @Test
    public void testSquareRootofZero() {
        assertEquals(0.0, calc.squareRoot(0), epsilon);
    }

    @Test
    public void testSquareRootofOne() {
        assertEquals(1.0, calc.squareRoot(1), epsilon);
    }

}
