/*
        Demonstrating cursor movement over the map of Edmonton. You will improve over
  what we do in the next weekly exercise.
*/

#define SD_CS 10
#define JOY_VERT A9 // should connect A9 to pin VRx
#define JOY_HORIZ A8 // should connect A8 to pin VRy
#define JOY_SEL 53

#include <Arduino.h>

// core graphics library (written by Adafruit)
#include <Adafruit_GFX.h>

// Hardware-specific graphics library for MCU Friend 3.5" TFT LCD shield
#include <MCUFRIEND_kbv.h>

// LCD and SD card will communicate using the Serial Peripheral Interface (SPI)
// e.g., SPI is used to display images stored on the SD card
#include <SPI.h>

// needed for reading/writing to SD card
#include <SD.h>

#include "lcd_image.h"

MCUFRIEND_kbv tft;

#define DISPLAY_WIDTH 480
#define DISPLAY_HEIGHT 320
#define YEG_SIZE 2048

lcd_image_t yegImage = { "yeg-big.lcd", YEG_SIZE, YEG_SIZE };

#define JOY_CENTER 512
#define JOY_DEADZONE 64

#define CURSOR_SIZE 9

const int yegMiddleX = YEG_SIZE / 2 - (DISPLAY_WIDTH - 60) / 2;
const int yegMiddleY = YEG_SIZE / 2 - DISPLAY_HEIGHT / 2;

const int max_x = DISPLAY_WIDTH - (CURSOR_SIZE / 2) - 1 - 60;
const int max_y = DISPLAY_HEIGHT - (CURSOR_SIZE / 2) - 1;

// the cursor position on the display
int cursorX, cursorY;

// forward declaration for redrawing the cursor
void redrawCursor(uint16_t colour);

void setup()
{
    init();

    Serial.begin(9600);

    pinMode(JOY_SEL, INPUT_PULLUP);

    //    tft.reset();             // hardware reset
    uint16_t ID = tft.readID(); // read ID from display
    Serial.print("ID = 0x");
    Serial.println(ID, HEX);
    if (ID == 0xD3D3)
        ID = 0x9481; // write-only shield

    // must come before SD.begin() ...
    tft.begin(ID); // LCD gets ready to work

    Serial.print("Initializing SD card...");
    if (!SD.begin(SD_CS)) {
        Serial.println("failed! Is it inserted properly?");
        while (true) {
        }
    }
    Serial.println("OK!");

    tft.setRotation(1);

    tft.fillScreen(TFT_BLACK);

    // draws the centre of the Edmonton map, leaving the rightmost 60 columns black

    lcd_image_draw(&yegImage, &tft, yegMiddleX, yegMiddleY, 0, 0, DISPLAY_WIDTH - 60, DISPLAY_HEIGHT);

    // initial cursor position is the middle of the screen
    cursorX = (DISPLAY_WIDTH - 60) / 2;
    cursorY = DISPLAY_HEIGHT / 2;

    redrawCursor(TFT_RED);
}

void redrawCursor(uint16_t colour)
{
    tft.fillRect(cursorX - CURSOR_SIZE / 2, cursorY - CURSOR_SIZE / 2, CURSOR_SIZE, CURSOR_SIZE, colour);
}

inline float calculate_acceleration(int joystick_value)
{
    // constant for the curve for the joystick's acceleration function.
    const float curve_constant = 1.2;

    float normalized = 0.0;

    if (joystick_value < 0) {
        normalized = (float)(joystick_value + JOY_DEADZONE) / JOY_CENTER;
    } else {
        normalized = (float)(joystick_value - JOY_DEADZONE) / JOY_CENTER;
    }

    return 8 * sin(curve_constant * normalized);
}

void processJoystick()
{
    // xVal is multiplied by -1 because joystick axis inverted.
    int xVal = -(analogRead(JOY_HORIZ) - JOY_CENTER);
    int yVal = analogRead(JOY_VERT) - JOY_CENTER;
    int buttonVal = digitalRead(JOY_SEL);

    bool joystick_moved = false;

    int cursorY_new = cursorY;
    int cursorX_new = cursorX;

    // check if joystick y past deadzone
    if (yVal < -JOY_DEADZONE || yVal > JOY_DEADZONE) {
        cursorY_new += calculate_acceleration(yVal);
        joystick_moved = true;
    }

    // check if joystick x past deadzone
    if (xVal < -JOY_DEADZONE || xVal > JOY_DEADZONE) {
        cursorX_new += calculate_acceleration(xVal);
        joystick_moved = true;
    }

    // only update if joystick moved
    if (joystick_moved) {
        // Calculate where in the image file the cursor was.
        int patch_origin_x = yegMiddleX + (cursorX - (CURSOR_SIZE / 2));
        int patch_origin_y = yegMiddleY + (cursorY - (CURSOR_SIZE / 2));

        // Redraw over old cursor
        lcd_image_draw(&yegImage, &tft, patch_origin_x, patch_origin_y, cursorX - (CURSOR_SIZE / 2),
            cursorY - (CURSOR_SIZE / 2), CURSOR_SIZE, CURSOR_SIZE);

        // set new cursor position
        cursorX = constrain(cursorX_new, CURSOR_SIZE / 2, max_x);
        cursorY = constrain(cursorY_new, CURSOR_SIZE / 2, max_y);

        // Draw new cursor
        redrawCursor(TFT_RED);
    }

    delay(10);
}

int main()
{
    setup();

    while (true) {
        processJoystick();
    }

    Serial.end();
    return 0;
}
