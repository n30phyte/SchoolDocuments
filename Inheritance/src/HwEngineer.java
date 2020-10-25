/**
 * A class representing a Hardware Engineer
 */
public class HwEngineer extends Employee implements SalaryRaisable {

    /**
     * @param name {@code String} Name of the engineer
     * @param baseSalary {@code double} Base salary of the engineer
     */
    public HwEngineer(String name, double baseSalary) {
        super(name, baseSalary);
    }

    /**
     * @return {@code double} Get the raised salary of the engineer
     */
    @Override
    public double RaiseSalary() {
        return this.getBaseSalary() * 1.18;
    }
}
