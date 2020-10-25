/**
 * A class representing a customer
 */
public class Customer extends Person implements Printable {
    private final double projPrice;

    /**
     * @param name {@code String} Customer name
     * @param projPrice {@code double} Price of the project
     */
    public Customer(String name, double projPrice) {
        super(name);
        this.projPrice = projPrice;
    }

    /**
     * @return {@code String} Formatted values
     */
    @Override
    public String PrintInfo() {
        return String.format("%s %s", this.getName(), this.getProjPrice());
    }

    /**
     * @return {@code double} Price of the customer's project
     */
    public double getProjPrice() {
        return projPrice;
    }
}
