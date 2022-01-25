import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class HwEngineerTest {
    HwEngineer engineer;

    final String NAME = "Joestar";
    final int SALARY = 300;

    @BeforeAll
    void Setup() {
        engineer = new HwEngineer(NAME, SALARY);
    }

    @Test
    void raiseSalary() {
        assertEquals(1.18 * engineer.getBaseSalary(), engineer.RaiseSalary());
    }

}
