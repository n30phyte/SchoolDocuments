/**
 * ECE 315 Wi21 Lab 1 Part 1
 * Seven Segment Display Decoder
 *
 * Gets input from Digilent KYPD PMOD and outputs the read value to Digilent
 * SSD PMOD
 *
 * Created on: 5 February 2021 by Shyama M. Gandhi
 *
 * Modified on: 11 Februrary 2021 by Michael Kwok
 */

#include "FreeRTOS.h"
#include "pmodkypd.h"
#include "queue.h"
#include "sleep.h"
#include "task.h"
#include "xgpio.h"
#include "xil_cache.h"
#include "xil_exception.h"
#include "xil_printf.h"
#include "xparameters.h"
#include "xscugic.h"

// Parameter definitions
#define SSD_DEVICE_ID XPAR_AXI_GPIO_PMOD_SSD_DEVICE_ID
#define KYPD_DEVICE_ID XPAR_AXI_GPIO_PMOD_KEYPAD_DEVICE_ID
#define SSD_DELAY 10

// Variables for GPIO access
XGpio SSDInst, KYPDInst;

PmodKYPD myDevice;

// Handle to running task
static TaskHandle_t xTxTask;

// keytable is determined as follows (indices shown in Keypad position below)
// 12 13 14 15
// 8  9  10 11
// 4  5  6  7
// 0  1  2  3
#define DEFAULT_KEYTABLE "0FED789C456B123A"

void DemoInitialize() {
  KYPD_begin(&myDevice, XPAR_AXI_GPIO_PMOD_KEYPAD_BASEADDR);
  KYPD_loadKeyTable(&myDevice, (u8 *)DEFAULT_KEYTABLE);
}

u32 SSD_decode(u8 key_value, u8 cathode) {
  switch (key_value) {
    case '0':
      return 0b00111111 | (cathode << 7);
    case '1':
      return 0b00000110 | (cathode << 7);
    case '2':
      return 0b01011011 | (cathode << 7);
    case '3':
      return 0b01001111 | (cathode << 7);
    case '4':
      return 0b01100110 | (cathode << 7);
    case '5':
      return 0b01101101 | (cathode << 7);
    case '6':
      return 0b01111101 | (cathode << 7);
    case '7':
      return 0b00000111 | (cathode << 7);
    case '8':
      return 0b01111111 | (cathode << 7);
    case '9':
      return 0b01101111 | (cathode << 7);
    case 'A':
      return 0b01110111 | (cathode << 7);
    case 'B':
      return 0b01111100 | (cathode << 7);
    case 'C':
      return 0b00111001 | (cathode << 7);
    case 'D':
      return 0b01011110 | (cathode << 7);
    case 'E':
      return 0b01111001 | (cathode << 7);
    case 'F':
      return 0b01110001 | (cathode << 7);
    default:
      return 0b00000000 | (cathode << 7);
  }
}

static void prvTxTask(void *pvParameters) {
  for (;;) {
    u16 keystate;
    XStatus status, last_status = KYPD_NO_KEY;
    u8 key, last_key = 'x';
    u32 ssd_value = 0;

    // Initial value of last_key cannot be contained in loaded KEYTABLE string
    Xil_Out32(myDevice.GPIO_addr, 0xF);

    xil_printf("Pmod KYPD demo started. Press any key on the Keypad.\r\n");
    while (1) {
      // Read keypad state
      keystate = KYPD_getKeyStates(&myDevice);

      // Read keypad input
      status = KYPD_getKeyPressed(&myDevice, keystate, &key);

      // Print detected single key input
      if (status == KYPD_SINGLE_KEY &&
          (status != last_status || key != last_key)) {
        xil_printf("Key Pressed: %c\r\n", (char)key);
        last_key = key;

      } else if (status == KYPD_MULTI_KEY && status != last_status)
        xil_printf("Error: Multiple keys pressed\r\n");

      // Save previous status
      last_status = status;

      // Clear first SSD
      XGpio_DiscreteWrite(&SSDInst, 1, 0b10000000);
      ssd_value = SSD_decode(key, 0);

      // Write first SSD value
      XGpio_DiscreteWrite(&SSDInst, 1, ssd_value);

      vTaskDelay(pdMS_TO_TICKS(SSD_DELAY));

      // Clear second SSD value
      XGpio_DiscreteWrite(&SSDInst, 1, 0b00000000);
      ssd_value = SSD_decode(key, 1);

      // Write second SSD value
      XGpio_DiscreteWrite(&SSDInst, 1, ssd_value);

      vTaskDelay(pdMS_TO_TICKS(SSD_DELAY));
    }
  }
}

int main(void) {
  int status;

  // Initialize SSD
  status = XGpio_Initialize(&SSDInst, SSD_DEVICE_ID);

  // Check initialization result
  if (status != XST_SUCCESS) {
    xil_printf("GPIO Initialization for SSD unsuccessful.\r\n");
    return XST_FAILURE;
  }

  // Set SSD pin data direction
  XGpio_SetDataDirection(&SSDInst, 1, 0);

  xil_printf("Initialization Complete, System Ready!\n");

  // Create new task from prvTxTask function, with minimal stack size, no
  // parameters at idle priority. Save task handle to xTxTask
  xTaskCreate(prvTxTask, (const char *)"Tx", configMINIMAL_STACK_SIZE, NULL,
              tskIDLE_PRIORITY, &xTxTask);

  DemoInitialize();

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}
