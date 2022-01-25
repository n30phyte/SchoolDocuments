import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import static org.junit.jupiter.api.Assertions.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class CustomerTest {
    Customer customer;

    final String NAME = "Jojo";
    final float PRICE = 5000;

    @BeforeAll
    void Setup() {
        customer = new Customer(NAME, PRICE);
    }

    @Test
    void printInfo() {
        System.out.println(customer.PrintInfo());
        assertEquals(String.format("%s %s", NAME, PRICE), customer.PrintInfo());
    }
}
