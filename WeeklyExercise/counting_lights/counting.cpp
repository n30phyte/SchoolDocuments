#include <Arduino.h>

const unsigned int LED_COUNT = 5;

const unsigned int STORE_LOAD_PIN = 5;

const unsigned int DECREMENT_PIN = 6;
const unsigned int INCREMENT_PIN = 7;

const unsigned int LED_PINS[LED_COUNT] = {9, 10, 11, 12, 13};

const void output_pins(const unsigned int* pin_array,
                       const int array_length,
                       const int value) {
  for (auto i = 0; i < array_length; i++) {
    int output_signal = LOW;
    if ((value & (1 << i)) != 0) {
      output_signal = HIGH;
    }
    digitalWrite(LED_PINS[i], output_signal);
  }
}

void setup() {
  init();

  Serial.begin(9600);

  // Set pin modes
  pinMode(STORE_LOAD_PIN, INPUT_PULLUP);

  pinMode(DECREMENT_PIN, INPUT_PULLUP);
  pinMode(INCREMENT_PIN, INPUT_PULLUP);

  for (auto i = 0; i < LED_COUNT; i++) {
    pinMode(LED_PINS[i], OUTPUT);
  }

  // Reset output
  output_pins(LED_PINS, LED_COUNT, 0);
}

int main() {
  setup();

  auto decrement_old = digitalRead(DECREMENT_PIN);
  auto increment_old = digitalRead(INCREMENT_PIN);

  auto store_old = digitalRead(STORE_LOAD_PIN);

  unsigned char counter = 0;
  unsigned char memory = 0;

  bool stored = false;

  while (true) {
    // Read inputs
    auto decrement_new = digitalRead(DECREMENT_PIN);
    auto increment_new = digitalRead(INCREMENT_PIN);

    auto store_new = digitalRead(STORE_LOAD_PIN);

    // Only fire when new signal and old signal different.
    if (decrement_new != decrement_old || increment_new != increment_old) {
      // Increment or decrement
      if (increment_new == LOW) {
        counter++;
      } else if (decrement_new == LOW) {
        --counter;
      }

      decrement_old = decrement_new;
      increment_old = increment_new;

    } else if (store_new != store_old) {
      if (store_new == LOW) {
        if (stored) {
          counter = memory;
          stored = false;
          Serial.println("Restoring");
        } else {
          Serial.println("Storing");
          memory = counter;
          counter = 0;
          stored = true;
        }
      }
      store_old = store_new;
    }

    // If counter overflows backwards, fix it
    if (counter == 32) {
      counter = 0;
    } else if (counter == 255) {
      counter = 31;
    }
    output_pins(LED_PINS, LED_COUNT, counter);
  }
}