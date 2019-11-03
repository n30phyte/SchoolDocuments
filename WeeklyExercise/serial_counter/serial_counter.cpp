/**********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 274, Fall 2019
 *  Weekly Exercise 6
 ********************************/
#include <Arduino.h>

// To reduce magic numbers
const int LED_COUNT = 5;

const int LED_PINS[LED_COUNT] = { 9, 10, 11, 12, 13 };

/*
 * Updates the LEDs to output the first 5 bits of the total integer
 *
 * @param total: The total integer to output
 */
void updateLEDs(uint8_t total)
{
    for (auto i = 0; i < LED_COUNT; i++) {
        digitalWrite(LED_PINS[i], total & (1 << i));
    }
}

/*
 * Converts characters into integer values
 *
 * @param digit: The input character
 * @return: the decimal value of the hex character if valid, -1 if invalid.
 */
int8_t getHexValue(char digit)
{
    if (digit >= '0' && digit <= '9') {
        return digit - '0';
    }

    // If it's an uppercase letter, this will turn it into lowercase.
    digit = digit | 0b100000;

    if (digit >= 'a' && digit <= 'f') {
        return (digit - 'a') + 10;
    }
    // Fallthrough
    return -1;
}

/*
 * Setup function.
 *
 * Calls Arduino initialization code (init())
 * Sets up pins to be on output mode for all the LED pins then turns off all LEDs.
 */
void setup()
{
    init();

    Serial.begin(9600);

    // Set LED pin modes
    for (auto i = 0; i < LED_COUNT; i++) {
        pinMode(LED_PINS[i], OUTPUT);
    }

    // Reset output
    updateLEDs(0);
    Serial.flush();
}

/*
 * Main function of the program.
 */
int main()
{
    setup();
    Serial.println("Ready for input.");

    uint8_t total = 0;

    // Busy loop
    while (true) {
        // Only do work when things available in Serial
        if (Serial.available() > 0) {

            char input = Serial.read();

            // Newline and whitespace check.
            if (input == '\r' || input == '\n' || input == ' ') {
                total = 0;
            } else {
                auto value = getHexValue(input);
                if (value != -1) {
                    total += value;
                }
            }

            Serial.print("Total: ");
            Serial.println(total);

            updateLEDs(total);
        }
    }
    return 0;
}