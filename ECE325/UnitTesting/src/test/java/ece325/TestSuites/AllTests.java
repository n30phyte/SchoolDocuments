package ece325.TestSuites;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import ece325.TestCases.*;
 
@RunWith(Suite.class)
@Suite.SuiteClasses({
    CreationTests.class,
    AdditionTests.class,
    SubtractionTests.class,
    MultiplicationTests.class,
    DivisionTests.class,
    SolveQuadraticTests.class,
    SquareRootTests.class,
})

public class AllTests {
    // the class remains completely empty, 
    // being used only as a holder for the above annotations
}
