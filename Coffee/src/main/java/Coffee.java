/**
 * Assignment 5: Interfaces <br />
 * Part 1: The {@code Coffee} class
 */
public class Coffee implements Comparable<Coffee> {
    private final int strength; // The strength of the coffee

    public Coffee(int coffeeStrength) {
        this.strength = coffeeStrength;
    }

    @Override
    public int compareTo(Coffee coffee) {
        return Integer.compare(this.strength, coffee.strength);
    }

    public int getStrength() {
        return strength;
    }
}
