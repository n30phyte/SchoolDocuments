class Counter {
    private long counter;

    Counter() {
        counter = 0;
    }

    public void operation(String operationName) {
        if (operationName.equals("report")) {
            System.out.format("Number of elements = %s%n", counter);
        } else {
            System.out.println("Wrong operation");
        }
    }

    public void operation(String operationName, long i) {
        if (i < 0) {
            System.out.println("Only support positive integer values");
            return;
        }

        switch (operationName) {
            case "add":
                counter += i;
                System.out.format("%s elements added%n", i);
                break;
            case "remove":
                if (counter - i < 0) {
                    System.out.println("Not enough elements");
                } else {
                    counter -= i;
                    System.out.format("%s elements removed%n", i);
                }
                break;
            default:
                System.out.println("Wrong operation");
                break;
        }
    }


    public void operation(String operationName, double i) {
        System.out.println("Only support integer values");
    }

}

public class Question {
    public static void main(String[] args) {
        Counter c = new Counter();
        c.operation("add", 10);     // print: "10 elements added"
        c.operation("remove", 5);   // print: "5 elements removed"
        c.operation("add", -2);     // print: "Only support positive integer values"
        c.operation("add", 3.7);    // print: "Only support integer values"
        c.operation("plus", 13);    // print: "Wrong operation"
        c.operation("remove", 10);  // print: "Not enough elements"
        c.operation("report");      // print: "Number of elements = 5"
    }
}
