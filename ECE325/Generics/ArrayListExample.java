import java.util.ArrayList;

/**
 * Assignment 7: Type Compatibility and Generics <br />
 * An generic array
 */
public class ArrayListExample {

    /**
     * total_area -- takes a list of 2d shapes and calculates the total area of those shapes
     */
    private static <T extends TwoDShape> double total_area(ArrayList<T> shapes) {

        double area = 0.0;

        for (var shape : shapes) {
            area += shape.area();
        }

        return area;
    }


    /**
     * total_perimeter -- takes a list of rectangles and calculates the total perimeter
     */
    private static <T extends Rectangle> double total_perimeter(ArrayList<T> rectangles) {

        double perimeter = 0.0;

        for (var rect : rectangles) {
            perimeter += rect.perimeter();
        }

        return perimeter;
    }

    /**
     * describe_all -- takes a list of geometric shapes and invokes the "{@code describe}" method
     * on each of them, then prints out the total number of shapes
     */
    private static <T extends GeometricShape> void describe_all(ArrayList<T> shapes) {
        for (var shape : shapes) {
            shape.describe();
        }

        System.out.format("Total number of shapes: %d%n", shapes.size());
    }


    /**
     * add_empties -- takes a list of geometric shapes and adds the following objects to it: <br/>
     * {@code new Circle(0.0);          } <br/>
     * {@code new Cone(0.0, 0.0);       } <br/>
     * {@code new Rectangle(0.0, 0.0);  } <br/>
     * {@code new Sphere(0.0);          }
     */

    private static void add_empties(ArrayList<GeometricShape> shapes) {
        shapes.add(new Circle(0.0));
        shapes.add(new Cone(0.0, 0.0));
        shapes.add(new Rectangle(0.0, 0.0));
        shapes.add(new Sphere(0.0));
    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        // Make a list of shapes, add a circle, a cone and some empty shapes, and then describe all of the shapes
        System.out.println("Example with a list of shapes with a circle, a cone, and some empty shapes");
        ArrayList<GeometricShape> shapes = new ArrayList<GeometricShape>();
        shapes.add(new Circle(1.0));
        shapes.add(new Cone(2.0, 3.0));
        add_empties(shapes);
        describe_all(shapes);
        // Make a list of rectangles, add some rectangles, describe them, and calculate the total area and perimeter
        System.out.println();
        System.out.println("Example with a list of rectangles");
        ArrayList<Rectangle> rects = new ArrayList<Rectangle>();
        rects.add(new Rectangle(2.0, 3.0));
        rects.add(new Rectangle(5.0, 5.0));
        describe_all(rects);
        System.out.print("total area of rectangles: ");
        System.out.println(total_area(rects));
        System.out.print("total perimeter of rectangles: ");
        System.out.println(total_perimeter(rects));
        // Make a list of 2d shapes, add a rectangle and a circle, describe them and calculate the total area.
        System.out.println();
        System.out.print("Example with a list of 2d shapes with a circle ");
        System.out.println("and a rectangle");
        ArrayList<TwoDShape> flat_shapes = new ArrayList<TwoDShape>();
        flat_shapes.add(new Rectangle(10.0, 10.0));
        flat_shapes.add(new Circle(2.0));
        describe_all(flat_shapes);
        System.out.print("total area of flat shapes: ");
        System.out.println(total_area(flat_shapes));
        // Make a list of spheres and describe them
        ArrayList<Sphere> spheres = new ArrayList<Sphere>();
        spheres.add(new Sphere(10.0));
        spheres.add(new Sphere(50.0));
        spheres.add(new Sphere(0.0));
        System.out.println();
        System.out.println("Example list of spheres");
        describe_all(spheres);
    }

}
