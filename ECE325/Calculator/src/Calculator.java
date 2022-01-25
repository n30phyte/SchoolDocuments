import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Stack;
import java.util.HashMap;
import java.util.regex.Pattern;

import static java.lang.Character.isDigit;


/**
 * ECE 325 - Fall 2020 <br/>
 * Assignment 3: Exception handling <br />
 * Calculator using BNF
 * <p>
 *
 * @author Michael Kwok
 */
public class Calculator {

    /**
     * Enum to identify tokens with the correct type when parsing
     */
    enum TokenType {
        SEMICOLON,
        ADD, SUBTRACT, MULTIPLY, EXPONENT, LEFT_BRACKET, RIGHT_BRACKET, EQUAL,
        VARIABLE_NAME, NUMBER,
        LET
    }

    /**
     * Storage class used like a struct, to store information about the calculator commands.
     */
    static class Token {
        TokenType type;
        Object value;

        Token(TokenType type, Object value) {
            this.type = type;
            this.value = value;
        }

        Token(TokenType type) {
            this.type = type;
            this.value = null;
        }
    }

    /**
     * Execute the expression, and return the correct value
     *
     * @param expr {@code String} The expression string
     * @return {@code int}    The value of the expression
     */
    public int execExpression(String expr) {
        var tokenList = tokenizeString(expr);
        var rpn = shunt(tokenList); // The commands converted to Reverse Polish Notation
        var outputStack = new Stack<Token>(); // Temporary stack to use for output.
        var registers = new HashMap<String, Integer>(); // Variable storage

        // Go through the RPN queue token by token
        while (!rpn.isEmpty()) {

            var currentToken = rpn.pop();

            switch (currentToken.type) {
                case VARIABLE_NAME:
                    var varValue = registers.get(currentToken.value);

                    // Using a variable name in the equation means it was previously defined with let
                    if (varValue != null) {
                        outputStack.push(new Token(TokenType.NUMBER, varValue));
                    } else {
                        throw new Error("runtime error: '" + currentToken.value + "' undefined");
                    }
                    break;
                case NUMBER:
                    outputStack.push(currentToken);
                    break;
                case LET:
                    var nextToken = outputStack.peek();
                    if (nextToken.type == TokenType.NUMBER || nextToken.type == TokenType.VARIABLE_NAME) {
                        Integer numericValue;
                        if (nextToken.type == TokenType.NUMBER) {
                            numericValue = (Integer) outputStack.pop().value;
                        } else {
                            var varName = (String) outputStack.pop().value;
                            numericValue = registers.get(varName);
                        }
                        // Replace token with assigned value of variable.
                        registers.put((String) currentToken.value, numericValue);
                        outputStack.push(new Token(TokenType.NUMBER, numericValue));
                    }
                    break;
                // Mathematical operations
                case ADD:
                    var aAdd = (Integer) outputStack.pop().value;
                    var bAdd = (Integer) outputStack.pop().value;
                    outputStack.push(new Token(TokenType.NUMBER, bAdd + aAdd));
                    break;
                case SUBTRACT:
                    var aSub = (Integer) outputStack.pop().value;
                    var bSub = (Integer) outputStack.pop().value;
                    outputStack.push(new Token(TokenType.NUMBER, bSub - aSub));
                    break;
                case MULTIPLY:
                    var aMult = (Integer) outputStack.pop().value;
                    var bMult = (Integer) outputStack.pop().value;
                    outputStack.push(new Token(TokenType.NUMBER, bMult * aMult));
                    break;
                case EXPONENT:
                    var aExp = (Integer) outputStack.pop().value;
                    var bExp = (Integer) outputStack.pop().value;
                    outputStack.push(new Token(TokenType.NUMBER, (int) Math.pow(bExp, aExp)));
                    break;
            }
        }

        // If after popping everything in the RPN queue there are more than 1 value, missing operator
        if (outputStack.size() != 1) {
            throw new Error("syntax error: operator expected");
        }

        // Get final value
        if (outputStack.peek().type == TokenType.VARIABLE_NAME) {
            return registers.get(outputStack.pop().value);
        } else if (outputStack.peek().type == TokenType.NUMBER) {
            return (int) outputStack.pop().value;
        }

        throw new Error("runtime error: unknown");
    }

    /**
     * A small helper function to compare operator precedence.
     *
     * @param left The token on the left to compare
     * @param right The token on the right to compare
     * @return <0 for left < right, 0 for left = right, >0 for left > right
     */
    private int comparePrecedent(TokenType left, TokenType right) {
        int leftVal = 0;
        int rightVal = 0;

        if (left == TokenType.ADD || left == TokenType.SUBTRACT) {
            leftVal = 1;
        } else if (left == TokenType.MULTIPLY || left == TokenType.EXPONENT) {
            leftVal = 2;
        }

        if (right == TokenType.ADD || right == TokenType.SUBTRACT) {
            rightVal = 1;
        } else if (right == TokenType.MULTIPLY || right == TokenType.EXPONENT) {
            rightVal = 2;
        }

        return leftVal - rightVal;
    }

    /**
     * Use the Shunting-yard algorithm to rearrange Token list to RPN, making operations easier
     *
     * @param tokenList Tokenized list of strings
     * @return RPN arranged from infix expressions
     */
    private ArrayDeque<Token> shunt(ArrayList<Token> tokenList) {
        var operatorStack = new Stack<Token>();
        var outputQueue = new ArrayDeque<Token>();

        var tokenIdx = 0;

        while (tokenIdx < tokenList.size()) {
            var currentToken = tokenList.get(tokenIdx);

            switch (currentToken.type) {
                case VARIABLE_NAME:
                case NUMBER:
                    outputQueue.add(currentToken);
                    tokenIdx++;
                    break;
                case LET:
                    var nextToken = tokenList.get(++tokenIdx);
                    if (nextToken.type != TokenType.VARIABLE_NAME) {
                        throw new Error("syntax error: variable name expected");
                    } else {
                        currentToken.value = nextToken.value;
                    }
                    nextToken = tokenList.get(++tokenIdx);
                    if (nextToken.type != TokenType.EQUAL) {
                        throw new Error("syntax error: '=' expected");
                    }
                    tokenIdx++;
                    operatorStack.push(currentToken);
                    break;
                case ADD:
                case SUBTRACT:
                case EXPONENT:
                case MULTIPLY:
                    while (!operatorStack.empty()// There is operator on top of stack
                            && (comparePrecedent(currentToken.type, operatorStack.peek().type) < 1)
                            && (operatorStack.peek().type != TokenType.LEFT_BRACKET)) {
                        outputQueue.add(operatorStack.pop());
                    }
                    operatorStack.push(currentToken);
                    tokenIdx++;
                    break;
                case LEFT_BRACKET:
                    operatorStack.push(currentToken);
                    tokenIdx++;
                    break;
                case RIGHT_BRACKET:
                    while (operatorStack.peek().type != TokenType.LEFT_BRACKET) {
                        outputQueue.add(operatorStack.pop());
                    }
                    if (operatorStack.peek().type == TokenType.LEFT_BRACKET) {
                        operatorStack.pop();
                    } else {
                        throw new Error("syntax error: '(' expected");
                    }
                    tokenIdx++;
                    break;
                case SEMICOLON:
                    while (!operatorStack.empty()) {
                        outputQueue.add(operatorStack.pop());
                    }
                    tokenIdx++;
                    break;
                default:
                    throw new Error("syntax error: unknown");
            }
        }

        return outputQueue;
    }

    /**
     * Tokenize an input string into a list of tokens
     *
     * @param input {@code String} String to be processed
     * @return {@code ArrayList<Token>}
     */
    private ArrayList<Token> tokenizeString(String input) {
        var split_string = input.split("\\s+");
        var tokenList = new ArrayList<Token>();

        var bracketDepth = 0;

        for (var word : split_string) {
            int current_idx = 0;
            while (current_idx < word.length()) {
                switch (word.charAt(current_idx)) {
                    case '(':
                        tokenList.add(new Token(TokenType.LEFT_BRACKET));
                        current_idx++;
                        bracketDepth++;
                        break;
                    case ')':
                        tokenList.add(new Token(TokenType.RIGHT_BRACKET));
                        current_idx++;
                        bracketDepth--;
                        break;
                    case '=':
                        tokenList.add(new Token(TokenType.EQUAL));
                        current_idx++;
                        break;
                    case ';':
                        tokenList.add(new Token(TokenType.SEMICOLON));
                        current_idx = word.length() + 1;
                        break;
                    case '+':
                        tokenList.add(new Token(TokenType.ADD));
                        current_idx++;
                        break;
                    case '-':
                        tokenList.add(new Token(TokenType.SUBTRACT));
                        current_idx++;
                        break;
                    case '^':
                        tokenList.add(new Token(TokenType.EXPONENT));
                        current_idx++;
                        break;
                    case '*':
                        tokenList.add(new Token(TokenType.MULTIPLY));
                        current_idx++;
                        break;
                    default:
                        int word_idx = 0;
                        var tempWord = word.substring(current_idx);

                        var variableNamePattern = Pattern.compile("^([a-zA-Z]+)\\W*");
                        var matcher = variableNamePattern.matcher(tempWord);

                        if (isDigit(tempWord.charAt(0))) {
                            while (word_idx < tempWord.length() && isDigit(tempWord.charAt(word_idx))) word_idx++;
                            tokenList.add(new Token(TokenType.NUMBER, Integer.parseInt(tempWord.substring(0, word_idx))));
                            current_idx += word_idx;
                        } else if (tempWord.matches("^let$")) {
                            tokenList.add(new Token(TokenType.LET, null));
                            current_idx += 3;
                        } else if (matcher.find()) {
                            tokenList.add(new Token(TokenType.VARIABLE_NAME, matcher.group(1)));
                            current_idx += matcher.group(1).length();
                        }
                        break;
                }
            }
        }

        if (bracketDepth > 0) {
            throw new Error("syntax error: ')' expected");
        } else if (bracketDepth < 0) {
            throw new Error("syntax error: '(' expected");
        }

        if(tokenList.get(tokenList.size()-1).type != TokenType.SEMICOLON) {
            throw new Error("syntax error: missing semicolon");
        }

        return tokenList;
    }

    /**
     * Main entry
     *
     * @param args {@code String[]} Command line arguments
     */
    public static void main(String[] args) {
        Calculator calc = new Calculator();
        // Part 1
        String[] inputs = {
                "let x = 1;",                                                                           // 1, returns 1
                "(let x=1)+x;",                                                                     // 2, returns 2
                "(let a = 2) + 3 * a - 5;",                                                             // 3, returns 3
                "(let x = (let y = (let z = 1))) + x + y + z;",                                         // 4, returns 4
                "1 + (let x = 1) + (let y = 2) + (1 + x) * (1 + y) - (let x = y) - (let y = 1) - x;",   // 5, returns 5
                "1 + (let a = (let b = 1) + b) + a + 1;",                                               // 6, returns 6
                "(let a = (let a = (let a = (let a = 2) + a) + a) + a) - 9;",                           // 7, returns 7
                "(let x = 2) ^ (let y = 3);",                                                           // 8, returns 8
                "(let y = 3) ^ (let x = 2);"                                                            // 9, returns 9
        };
        for (int i = 0; i < inputs.length; i++)
            System.out.printf("%d -- %-90s %d%n", i + 1, inputs[i], calc.execExpression(inputs[i]));

        // Part 2
        inputs = new String[]{
                "1 + (2 * 3;",                  // 1, syntax error: ')' expected
                "(let x 5) + x;",               // 2, syntax error: '=' expected
                "(let x = 5) (let y = 6);",     // 3, syntax error: operator expected
                "(let x = 5 let y = 6);",       // 4, syntax error: ')' expected
                "(ler x = 5) ^ (let y = 6);",   // 5, runtime error: 'ler' undefined
                "(let x = 5) + y;"              // 6, runtime error: 'y' undefined
        };
        for (int i = 0; i < inputs.length; i++)
            try {
                System.out.printf("%d -- %-30s %d%n", i + 1, inputs[i], calc.execExpression(inputs[i]));
            } catch (Error e) {
                System.out.println(e.getMessage());
            }
    }

}
