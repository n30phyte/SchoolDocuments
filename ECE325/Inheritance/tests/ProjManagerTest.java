import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class ProjManagerTest {
    ProjManager manager;
    Date deadline;
    final String NAME = "James";
    final String PROJECT_NAME = "Raytracer";
    final int SALARY = 6000;

    @BeforeAll
    void Setup() {
        Calendar cal = Calendar.getInstance();

        cal.add(Calendar.DAY_OF_YEAR, 1);

        deadline = cal.getTime();
        
        manager = new ProjManager(NAME, SALARY, PROJECT_NAME, deadline);
    }

    @Test
    void printInfo() {
        assertEquals(String.format("%s %s %s %s", NAME, PROJECT_NAME, SALARY * 1.24, deadline), manager.PrintInfo());
    }
}