import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class fnv {
    /**
     * Hash a byte array b with large integers as factors
     *
     * @param b {@code byte[]} Byte array to be hashed
     * @return {@code BigInteger} Hashed result.
     */
    private static BigInteger fnv(byte[] b) {

        var hash = new BigInteger("14695981039346656037");
        var hashFactor = new BigInteger("1099511628211");

        for (var i : b) {
            // hash = (hash * factor) xor i
            hash = (hash.multiply(hashFactor)).xor(BigInteger.valueOf(i));
        }

        return hash;
    }

    public static void main(String[] args) {
        // Part 1 tests
        final String NAME = "Joestar";
        final int PRICE = 300;
        var customer = new Customer(NAME, PRICE);
        System.out.println(customer.PrintInfo());

        ProjManager manager;
        Date deadline;
        final String PROJECT_NAME = "Raytracer";
        final int SALARY = 6000;

        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_YEAR, 1);
        deadline = cal.getTime();

        manager = new ProjManager(NAME, SALARY, PROJECT_NAME, deadline);

        System.out.println(manager.PrintInfo());

        // fnv tests
        var out = fnv(new byte[]{0});
        System.out.println(out);

        out = fnv("H".getBytes());
        System.out.println(out);

        out = fnv("Hi".getBytes());
        System.out.println(out);
    }
}
