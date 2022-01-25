/**
 * A class representing an employee
 */
public class Employee extends Person {
    private final double baseSalary;

    /**
     * @param name {@code String} Employee's name
     * @param baseSalary {@code double} Employee's salary
     */
    public Employee(String name, double baseSalary) {
        super(name);
        this.baseSalary = baseSalary;
    }

    /**
     * @return {@code double} Get the base salary of the employee.
     */
    public double getBaseSalary() {
        return baseSalary;
    }
}
