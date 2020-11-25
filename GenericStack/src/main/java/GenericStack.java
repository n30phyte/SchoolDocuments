import java.util.ArrayList;
import java.util.Stack;

/**
 * Lab 4: Generics <br /> The {@code GenericStack} class
 */
public class GenericStack<T> {

    ArrayList<T> stack = new ArrayList<>();

    /**
     * Query the top element
     *
     * @return {@code T} the top element
     */
    public T peek() {
        return stack.get(stack.size() - 1);
    }

    /**
     * Add a new element as top element
     *
     * @param value {@code T} the new element
     */
    public void push(T value) {
        stack.add(value);
    }

    /**
     * Remove the top element
     *
     * @return {@code T} the removed element
     */
    public T pop() {
        var value = this.peek();
        stack.remove(stack.size() - 1);
        return value;
    }

    /**
     * Query the size of the stack
     *
     * @return {@code int} size of the element
     */
    public int size() {
        return stack.size();
    }

    /**
     * Check if the stack is empty of not
     *
     * @return {@code boolean} {@code true} for empty; {@code false} for not
     */
    public boolean isEmpty() {
        return stack.size() == 0;
    }

    /**
     * Simple function to check if a String is a valid number
     * @param str {@code String} String to check
     * @return {@code boolean} {@code true} if it's a number, {@code false} otherwise
     */
    private static boolean isNumeric(String str) {
        try {
            Double.parseDouble(str);
            return true;
        } catch(NumberFormatException ex) {
            return false;
        }
    }

    /**
     * Calculate a postfix expression
     *
     * @param exp {@code String} the postfix expression
     * @return {@code Double} the value of the expression
     */
    public static Double calcPostfixExpression(String exp) {
        var tokenStack = new GenericStack<Double>();
        var tokens = exp.split(" ");

        int idx = 0;

        while (idx < tokens.length) {
            if(isNumeric(tokens[idx])) {
                tokenStack.push(Double.parseDouble(tokens[idx]));
            } else {
                var right = tokenStack.pop();
                var left = tokenStack.pop();
                switch(tokens[idx]) {
                    case "+":
                        tokenStack.push(left + right);
                        break;
                    case "-":
                        tokenStack.push(left - right);
                        break;
                    case "*":
                        tokenStack.push(left * right);
                        break;
                    case "/":
                        tokenStack.push(left / right);
                        break;
                    case "^":
                        tokenStack.push(Math.pow(left, right));
                        break;
                }
            }

            idx++;
        }

        return tokenStack.pop();
    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        String[] expressions = {
            "4 1 +",                    // 1: = 5
            "2 6 -",                    // 2: = -4
            "3 3 *",                    // 3: = 9
            "1 4 /",                    // 4: = 0.25
            "2 3 ^",                    // 5: = 8
            "2 1 + 4 * 8 - 3 ^ 6 -",    // 6: 58
        }; // String[] expressions = { ... };
        for (String s : expressions) {
            System.out.println(s + " = " + calcPostfixExpression(s));
        }
    }

}
