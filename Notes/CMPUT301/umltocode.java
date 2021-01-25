public class Employee {
    private String name

    public Employee(String name) {
        this.name = name;
    }

    public String getName() {
        return this.name;
    }
}

public class Manager extends Employee {
    private ArrayList<Employee> manages;
    public Manager(String name) {
        super(name);
    }
}

public class Artist extends Employee {
    public Artist(String name) {
        super(name);
    }
}

public class Programmer extends Employee {
    public Programmer(String name) {
        super(name);
    }
}
