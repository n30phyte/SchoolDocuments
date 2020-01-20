/**********************************
 *  Name: Michael Kwok
 *  ID: 1548454
 *  CMPUT 274, Fall 2019
 *  Weekly Exercise 5
 ********************************/

#include <Arduino.h>
#include <Adafruit_GFX.h>
#include <MCUFRIEND_kbv.h>
#include <SD.h>
#include <TouchScreen.h>

#define SD_CS 10

// physical dimensions of the tft display (# of pixels)
#define DISPLAY_WIDTH 480
#define DISPLAY_HEIGHT 320

// touch screen pins, obtained from the documentaion
#define YP A3 // must be an analog pin, use "An" notation!
#define XM A2 // must be an analog pin, use "An" notation!
#define YM 9 // can be a digital pin
#define XP 8 // can be a digital pin

// dimensions of the part allocated to the map display
#define MAP_DISP_WIDTH (DISPLAY_WIDTH - 60)
#define MAP_DISP_HEIGHT DISPLAY_HEIGHT

#define REST_START_BLOCK 4000000
#define NUM_RESTAURANTS 1066

// calibration data for the touch screen, obtained from documentation
// the minimum/maximum possible readings from the touch point
#define TS_MINX 100
#define TS_MINY 120
#define TS_MAXX 940
#define TS_MAXY 920

// thresholds to determine if there was a touch
#define MINPRESSURE 10
#define MAXPRESSURE 1000

MCUFRIEND_kbv tft;

// a multimeter reading says there are 300 ohms of resistance across the plate,
// so initialize with this to get more accurate readings
TouchScreen ts = TouchScreen(XP, YP, XM, YM, 300);

// different than SD
Sd2Card card;

int slow_run_count = 0;
int fast_run_count = 0;

int slow_run_latest = 0;
int fast_run_latest = 0;

int slow_run_avg = 0;
int fast_run_avg = 0;

struct restaurant {
    int32_t lat;
    int32_t lon;
    uint8_t rating; // from 0 to 10
    char name[55];
};

/**
 * Function used to print char array vertically at specified location
 *
 * @param input_string: The null terminated char array to be printed.
 * @param x_loc: The x location on the monitor
 * @param y_loc: The y location on the monitor
 */
void print_vertical(char* input_string, int16_t x_loc, int16_t y_loc)
{
    auto i = 0;
    tft.setCursor(x_loc, y_loc);

    while (input_string[i] != '\0') {
        tft.println(input_string[i]);
        i++;
        tft.setCursor(x_loc, y_loc + (20 * i));
    }
}

void setup()
{
    init();
    Serial.begin(9600);

    // tft display initialization
    uint16_t ID = tft.readID();
    tft.begin(ID);

    tft.fillScreen(TFT_BLACK);
    tft.setRotation(1);

    // SD card initialization for raw reads
    Serial.print("Initializing SPI communication for raw reads...");
    if (!card.init(SPI_HALF_SPEED, SD_CS)) {
        Serial.println("failed! Is the card inserted properly?");
        while (true) {
        }
    } else {
        Serial.println("OK!");
    }

    tft.setTextSize(2);

    // Prints the buttons on the side of the screen
    print_vertical("SLOW", DISPLAY_WIDTH - 35, 40);
    print_vertical("FAST", DISPLAY_WIDTH - 35, 200);

    // Draw the button boundaries
    tft.drawRect(DISPLAY_WIDTH - 60, 0, 60, 160, TFT_RED);
    tft.drawRect(DISPLAY_WIDTH - 60, 160, 60, 160, TFT_RED);
}

/**
 * The getRestaurant function provided in class
 *
 * @param restIndex: The index of the restaurant being requested
 * @param restPtr: The pointer to the memory location where the requested restaurant should be placed.
 */

void getRestaurant(int restIndex, restaurant* restPtr)
{
    uint32_t blockNum = REST_START_BLOCK + restIndex / 8;
    restaurant restBlock[8];

    while (!card.readBlock(blockNum, (uint8_t*)restBlock)) {
        Serial.println("Read block failed, trying again.");
    }

    *restPtr = restBlock[restIndex % 8];
}

/**
 * Optimized version of getRestaurant
 *
 * Uses static variables instead of globals
 *
 * @param restIndex: The index of the restaurant being requested
 * @param restPtr: The pointer to the memory location where the requested restaurant should be placed.
 */

void getRestaurantFast(int restIndex, restaurant* restPtr)
{
    uint32_t blockNum = REST_START_BLOCK + restIndex / 8;

    static restaurant restBlock[8];
    static uint32_t previousBlockNum = 0;

    if (blockNum != previousBlockNum) {
        while (!card.readBlock(blockNum, (uint8_t*)restBlock)) {
            Serial.println("Read block failed, trying again.");
        }
        previousBlockNum = blockNum;
    }

    *restPtr = restBlock[restIndex % 8];
}

/**
 * Subroutine to print out the run values.
 */
void print_display()
{
    tft.setCursor(0, 0);
    tft.setTextColor(TFT_WHITE, TFT_BLACK);

    tft.println("RECENT SLOW RUN:");
    if (slow_run_latest != 0) {
        tft.print(slow_run_latest);
        tft.println(" ms");
    } else {
        tft.println("Not yet run");
    }
    tft.println();

    tft.println("SLOW RUN AVG:");
    if (slow_run_avg != 0) {
        tft.print(slow_run_avg);
        tft.println(" ms");
    } else {
        tft.println("Not yet run");
    }
    tft.println();

    tft.println("RECENT FAST RUN:");
    if (fast_run_latest != 0) {
        tft.print(fast_run_latest);
        tft.println(" ms");
    } else {
        tft.println("Not yet run");
    }
    tft.println();

    tft.println("FAST RUN AVG:");
    if (fast_run_avg != 0) {
        tft.print(fast_run_avg);
        tft.println(" ms");
    } else {
        tft.println("Not yet run");
    }
}

/**
 * Function to check for where the touchscreen is being touched
 *
 * This code is based on the provided code in class.
 *
 * @param x_point: The x coordinate of the touch
 * @param y_point: The y coordinate of the touch
 */
void poll_touchscreen(int16_t& x_point, int16_t& y_point)
{
    TSPoint point = ts.getPoint();
    pinMode(YP, OUTPUT);
    pinMode(XM, OUTPUT);

    x_point = -1;
    y_point = -1;

    if (point.z > MINPRESSURE && point.z < MAXPRESSURE) {
        x_point = map(point.y, TS_MINX, TS_MAXX, DISPLAY_WIDTH - 1, 0);
        y_point = map(point.x, TS_MINY, TS_MAXY, DISPLAY_HEIGHT - 1, 0);
    }
}

int main()
{
    setup();

    // print out the left side of the screen.
    print_display();

    // temporary storage for the restaurant
    restaurant rest;

    // temporary storage for the x and y touch points
    int16_t x_point;
    int16_t y_point;

    while (true) {
        poll_touchscreen(x_point, y_point);

        // check if within 60 pixels of the right edge
        if (x_point > (DISPLAY_WIDTH - 60) && x_point < DISPLAY_WIDTH) {
            if (y_point < 160) { // Check if in the top half
                // Slow
                Serial.println("Running slow algorithm...");
                int start_time = millis();
                for (int i = 0; i < NUM_RESTAURANTS; ++i) {
                    getRestaurant(i, &rest);
                }
                slow_run_latest = millis() - start_time;
                slow_run_avg = ((slow_run_avg * slow_run_count) + slow_run_latest) / ++slow_run_count;

                Serial.print("Slow run completed in (ms): ");
                Serial.println(slow_run_latest);
            } else if (y_point > 160) { // Check if in the bottom half
                // Fast
                Serial.println("Running fast algorithm...");
                int start_time = millis();
                for (int i = 0; i < NUM_RESTAURANTS; ++i) {
                    getRestaurantFast(i, &rest);
                }
                fast_run_latest = millis() - start_time;
                fast_run_avg = ((fast_run_avg * fast_run_count) + fast_run_latest) / ++fast_run_count;

                Serial.print("Fast run completed in (ms): ");
                Serial.println(fast_run_latest);
            }
            print_display();
        }
    }

    Serial.end();

    return 0;
}