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
    auto output_signal = (value & (1 << i)) ? HIGH : LOW;

    Serial.println(value & (1 << i));

    digitalWrite(LED_PINS[i], output_signal);
  }
}

void setup() {
  init();

  Serial.begin(9600);

  // Set pin modes
  pinMode(STORE_LOAD_PIN, INPUT);

  pinMode(DECREMENT_PIN, INPUT);
  pinMode(INCREMENT_PIN, INPUT);

  for (auto i = 0; i < LED_COUNT; i++) {
    pinMode(INCREMENT_PIN, OUTPUT);
  }

  // Reset output
  output_pins(LED_PINS, LED_COUNT, 0);
}

int main() {
  setup();

  unsigned char counter = 0;

  auto decrement_old = digitalRead(DECREMENT_PIN);
  auto increment_old = digitalRead(INCREMENT_PIN);

  auto store_old = digitalRead(STORE_LOAD_PIN);

  unsigned char memory = 0;
  bool stored = false;

  while (true) {
    // Increment/Decrement
    auto decrement_new = digitalRead(DECREMENT_PIN);
    auto increment_new = digitalRead(INCREMENT_PIN);

    // Only fire when new signal and old signal different.
    if (decrement_new != decrement_old || increment_new != increment_old) {
      // Increment or decrement
      if (increment_new == HIGH) {
        counter++;
      } else if (decrement_new == HIGH) {
        --counter;
      }
      Serial.println(counter);
      decrement_old = decrement_new;
      increment_old = increment_new;
    }

    // If counter overflows backwards, fix it
    if (counter == (1 << LED_COUNT)) {
      counter = (1 << LED_COUNT) - 1;
      Serial.println(counter);
    }

    // Store/Load
    auto store_new = digitalRead(STORE_LOAD_PIN);

    if (store_new != store_old) {
      if (store_new == HIGH) {
        if (stored) {
          counter = memory;
          stored = false;
        } else {
          memory = counter;
          counter = 0;
          stored = true;
        }
      }
    }

    output_pins(LED_PINS, LED_COUNT, counter);
  }

  return 0;
}