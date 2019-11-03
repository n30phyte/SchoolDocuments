/**********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 274, Fall 2019
 *  Weekly Exercise 5
 ********************************/

#include <Arduino.h>

// To reduce magic numbers
const unsigned int LED_COUNT = 5;

const unsigned int STORE_LOAD_PIN = 5;

const unsigned int DECREMENT_PIN = 6;
const unsigned int INCREMENT_PIN = 7;

const unsigned int LED_PINS[LED_COUNT] = {9, 10, 11, 12, 13};

// Output should be all LED off at the start
char LED_output[LED_COUNT] = {LOW, LOW, LOW, LOW, LOW};

/*
 * Subroutine to output the stored state of the LEDs
 */
const void output_pins() {
  for (int i = 0; i < LED_COUNT; i++) {
    digitalWrite(LED_PINS[i], LED_output[i]);
  }
}

/*
 * Increments the stored array by one
 */
void increment() {
  unsigned int index = 0;
  while ((LED_output[index] != LOW) && (index < LED_COUNT)) {
    LED_output[index] = LOW;
    index++;
  }
  if (LED_output[index] == LOW) {
    LED_output[index] = HIGH;
  }
}

/*
 * Decrements the stored array by one
 */
void decrement() {
  unsigned int index = 0;
  while ((LED_output[index] != HIGH) && (index < LED_COUNT)) {
    LED_output[index] = HIGH;
    index++;
  }
  if (LED_output[index] == HIGH) {
    LED_output[index] = LOW;
  }
}

/*
 * Copies an array to another, while filling up the original with LOWs
 *
 * @param input: the array where the LED output is stored
 */
void store(char* input) {
  for (int i = 0; i < LED_COUNT; i++) {
    *(input + i) = *(LED_output + i);
    *(LED_output + i) = LOW;
  }
}

/*
 * Replaces the output array with the array taken in the parameter
 *
 * @param input: the array where the LED output was stored
 */
void load(const char* input) {
  for (int i = 0; i < LED_COUNT; i++) {
    *(LED_output + i) = *(input + i);
  }
}

/*
 * Setup function.
 *
 * Calls Arduino initialization code (init())
 * Sets up pins to be pullup mode for inputs and output mode for all the LED
 * pins. Then turns off all LEDs
 */
void setup() {
  init();

  // Serial.begin(9600);

  // Set pin modes
  pinMode(STORE_LOAD_PIN, INPUT_PULLUP);

  pinMode(DECREMENT_PIN, INPUT_PULLUP);
  pinMode(INCREMENT_PIN, INPUT_PULLUP);

  // Set LED pin modes
  for (int i = 0; i < LED_COUNT; i++) {
    pinMode(LED_PINS[i], OUTPUT);
  }

  // Reset output
  output_pins();
}

/*
 * Main function of the program.
 */
int main() {
  setup();

  // Assume all buttons are off at the start.
  int decrement_old = HIGH;
  int increment_old = HIGH;
  int store_old = HIGH;

  // Flag to remember if any data is stored.
  bool is_stored = false;

  // Stored data
  char memory[LED_COUNT] = {LOW, LOW, LOW, LOW, LOW};

  while (true) {
    // Read inputs
    int decrement_new = digitalRead(DECREMENT_PIN);
    int increment_new = digitalRead(INCREMENT_PIN);
    int store_new = digitalRead(STORE_LOAD_PIN);

    // Only fire when new signal and old signal different.
    if (decrement_new != decrement_old || increment_new != increment_old ||
        store_new != store_old) {
      // Increment, decrement load or store.
      if (increment_new == LOW) {
        increment();
      } else if (decrement_new == LOW) {
        decrement();
      } else if (store_new == LOW) {
        if (is_stored) {
          load(memory);
          is_stored = false;
        } else {
          store(memory);
          is_stored = true;
        }
      }

      // Store current button states as old
      decrement_old = decrement_new;
      increment_old = increment_new;
      store_old = store_new;
    }

    // Output to LEDs
    output_pins();
  }

  return 0;
}