/**
 * Assignment 5: Interfaces <br />
 * Part 1: The {@code Coffee} class
 */
public class Coffee implements Comparable<Coffee> {
    private int strength; // The strength of the coffee

    @Override
    public int compareTo(Coffee coffee) {
        return Integer.compare(this.strength, coffee.strength);
    }
}
