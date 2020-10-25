import java.util.Date;
import java.util.Objects;

/**
 * Class to represent project manager
 */
public class ProjManager extends SwEngineer implements SalaryRaisable, Printable {
    private final Date projDeadline;

    /**
     * Constructor for a project manager
     * @param name {@code String} Project manager's name
     * @param baseSalary {@code double} Project manager's starting salary before multiplier
     * @param projName {@code String} Project name being managed by manager
     * @param projDeadline {@code Date} Deadline for the project
     */
    public ProjManager(String name, double baseSalary, String projName, Date projDeadline) {
        super(name, baseSalary, projName);
        this.projDeadline = projDeadline;
    }

    /**
     * Getter for project manager's project deadline
     * @return {@code Date} Date object for deadline
     */
    public Date getProjDeadline() {
        return projDeadline;
    }

    /**
     * Getter for increased, not base, salary.
     * @return {@code double} Increased salary
     */
    @Override
    public double RaiseSalary() {
        return this.getBaseSalary() * 1.24;
    }

    /**
     * Getter of manager's information as a formatted string
     * @return {@code String} Manager's name, project name, salary and deadline.
     */
    @Override
    public String PrintInfo() {
        return String.format("%s %s %s %s", this.getName(), this.getProjName(), this.RaiseSalary(), this.getProjDeadline());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ProjManager other = (ProjManager) o;
        return Objects.equals(this.getName(), other.getName())
                && (this.RaiseSalary() == other.RaiseSalary());
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.getName(), this.RaiseSalary(), this.getProjName());
    }
}
