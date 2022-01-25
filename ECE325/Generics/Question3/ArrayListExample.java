import java.util.ArrayList;

/**
 * Assignment 7: Type Compatibility and Generics <br />
 * An generic array
 */
public class ArrayListExample {


    /**
     * Difficult Question: <br/>
     * supersize_list -- takes an array list of some kind of geometric shapes
     * and returns an array list of the same type, with the shapes super sized
     */

    private static <T extends GeometricShape<T>> ArrayList<T> supersize_list(ArrayList<T> shapes) {

        var outputArray = new ArrayList<T>();

        for (var shape : shapes) {

            outputArray.add(shape.supersize());
        }

        return outputArray;
    }

    /**
     * describe_all -- takes a list of geometric shapes and invokes the "{@code describe}" method
     * on each of them, then prints out the total number of shapes
     */
    private static <T extends GeometricShape<T>> void describe_all(ArrayList<T> shapes) {
        for (var shape : shapes) {
            shape.describe();
        }

        System.out.format("Total number of shapes: %d%n", shapes.size());
    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        // Make a list of rectangles and add some rectangles.
        ArrayList<Rectangle> rects = new ArrayList<Rectangle>();
        rects.add(new Rectangle(2.0, 3.0));
        rects.add(new Rectangle(5.0, 5.0));
        // Make a list of spheres
        ArrayList<Sphere> spheres = new ArrayList<Sphere>();
        spheres.add(new Sphere(10.0));
        spheres.add(new Sphere(50.0));
        spheres.add(new Sphere(0.0));
        // Super-size them
        System.out.println();
        System.out.println("super-sizing a list of rectangles");
        ArrayList<Rectangle> double_rects = supersize_list(rects);
        describe_all(double_rects);
        System.out.println();
        System.out.println("super-sizing a list of spheres");
        ArrayList<Sphere> double_spheres = supersize_list(spheres);
        describe_all(double_spheres);

    }


}
