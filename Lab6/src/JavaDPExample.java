import java.util.HashMap;

/**
 * Lab 6: Anonymous Inner Classes and Reflection <br />
 * The {@code Animal} interface
 */
interface Animal {
    /**
     * An animal speaks
     *
     * @return {@code String} animal speaks
     */
    String speak();
}

/**
 * Lab 6: Anonymous Inner Classes and Reflection <br />
 * The {@code Lion} class
 */
class Lion implements Animal {
    /**
     * The lion speaks
     *
     * @return {@code String} lion speaks
     */
    public String speak() {
        return "ROAR";
    }
}

/**
 * Lab 6: Anonymous Inner Classes and Reflection <br />
 * The {@code Mouse} class
 */
class Mouse implements Animal {
    /**
     * The mouse speaks
     *
     * @return {@code String} mouse speaks
     */
    public String speak() {
        return "SQUEAK";
    }
}

/**
 * Lab 6: Anonymous Inner Classes and Reflection <br />
 * The {@code Bison} class
 */
class Bison implements Animal {
    /**
     * The bison speaks
     *
     * @return {@code String} bison speaks
     */
    public String speak() {
        return "BELLOW";
    }
}


/**
 * Dog class for Deliverable 2
 */
class Dog implements Animal {

    /**
     * Dog speaking
     *
     * @return Dog sound
     */
    public String speak() {
        return "woof";
    }
}


/**
 * Lab 6: Anonymous Inner Classes and Reflection <br />
 * The {@code AnimalType} class
 */
class AnimalType {

    private static final HashMap<String, String> animalTypes = new HashMap<>();

    /**
     * Create and return an animal
     *
     * @param criteria {@code String} how is the animal like
     * @return {@code Animal} the animal
     */
    public static Animal getAnimal(String criteria) {
        try {
            var className = animalTypes.get(criteria);
            var output = Class.forName(className).getDeclaredConstructor().newInstance();
            return (Animal) output;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Add a new animal and criteria into the registry
     *
     * @param criteria Criteria for the animal
     * @param className Name of the animal class being inserted
     */
    public static void addAnimal(String criteria, String className) {
        animalTypes.put(criteria, className);
    }

    /**
     * Remove an animal from the registry
     *
     * @param criteria Criteria of the animal to remove
     */
    public static void removeAnimal(String criteria) {
        animalTypes.remove(criteria);
    }
}

/**
 * Lab 6: Anonymous Inner Classes and Reflection <br />
 * The {@code JavaDPExample} class
 */
public class JavaDPExample {
    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {

        AnimalType.addAnimal("small", Mouse.class.getName());
        AnimalType.addAnimal("big", Bison.class.getName());
        AnimalType.addAnimal("lazy", Lion.class.getName());

        Animal small = AnimalType.getAnimal("small");
        System.out.println(small.getClass().getName() + " speaks: " + small.speak());
        Animal big = AnimalType.getAnimal("big");
        System.out.println(big.getClass().getName() + " speaks: " + big.speak());
        Animal lazy = AnimalType.getAnimal("lazy");
        System.out.println(lazy.getClass().getName() + " speaks: " + lazy.speak());

        AnimalType.addAnimal("loyal", Dog.class.getName());

        Animal loyal = AnimalType.getAnimal("loyal");
        System.out.println(loyal.getClass().getName() + " speaks: " + loyal.speak());

        AnimalType.removeAnimal("small");

        try {
            small = AnimalType.getAnimal("small");
            System.out.println(small.getClass().getName() + " speaks: " + small.speak());
        } catch (Exception e) {
            System.out.println("Unknown animal...");
        }
    }
}
