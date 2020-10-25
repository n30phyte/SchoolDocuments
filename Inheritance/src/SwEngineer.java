import java.util.Objects;

/**
 * Class to represent Software Engineer
 */
public class SwEngineer extends Employee {
    private final String projName;

    /**
     *
     * @param name {@code String} Engineer's name
     * @param baseSalary {@code double} Engineer's starting salary
     * @param projName {@code String} Project that engineer is working on
     */
    public SwEngineer(String name, double baseSalary, String projName) {
        super(name, baseSalary);
        this.projName = projName;
    }

    /**
     * Getter for the name of current project
     * @return {@code String} current project name
     */
    public String getProjName() {
        return projName;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SwEngineer that = (SwEngineer) o;
        return Objects.equals(this.getName(), that.getName())
                && (this.getBaseSalary() == that.getBaseSalary());
    }


    @Override
    public int hashCode() {
        return Objects.hash(this.getName(), this.getBaseSalary(), this.getProjName());
    }
}
