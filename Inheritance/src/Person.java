/**
 * Base person class for everything
 */
public class Person {
    private final String name;

    /**
     * Default constructor for all subclasses
     * @param name {String}  Name of the person
     */
    public Person(String name) {
        this.name = name;
    }

    /**
     * Return name of the person
     * @return {@code String}}
     */
    public String getName() {
        return name;
    }
}
