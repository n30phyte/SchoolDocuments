package ece325;

import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;

/**
 * ECE 325 - Fall 2020
 * Assignment 4 Part 1: Static Code Analysis <br />
 * The buggy {@code CodingHorror} source code
 */
public class CodingHorror {

    public static void main(String[] args) {
        InputStreamReader isr = new InputStreamReader(System.in, Charset.defaultCharset());
        BufferedReader br = new BufferedReader(isr);
        String input = null;
        try {
            input = br.readLine();                  // e.g., peel
        } catch (IOException ioex) {
            System.err.println(ioex.getMessage());
        }

        // Did not check if br.readLine() failed. Possible NullPointerException
        if (input == null) {
            // Set empty String
            input = "";
        }
        // Replace does not mutate input, returns a new value instead.
        input = input.replace('e', 'o');

        // Originally used ==. Comparison of objects does not compare values.
        if (input.equals("pool")) {
            System.out.println("User entered peel.");
        } else {
            System.out.println("User entered something else.");
        }

    }
}
