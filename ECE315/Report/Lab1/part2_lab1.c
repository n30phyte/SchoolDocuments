/**
 * ECE 315 Wi21 Lab 1 Part 2
 * Single digit calculator
 *
 * Gets input from Digilent KYPD PMOD and outputs the calculated values to
 * Digilent SSD PMOD.
 *
 * Negative results will not show the sign. Division by zero will be -1.
 *
 * Created on: 5 February 2021 by Shyama M. Gandhi
 *
 * Modified on: 11 Februrary 2021 by Michael Kwok
 */

#include "FreeRTOS.h"
#include "math.h"
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
#define BTNS_DEVICE_ID XPAR_AXI_GPIO_BUTTONS_DEVICE_ID
#define SSD_DEVICE_ID XPAR_AXI_GPIO_PMOD_SSD_DEVICE_ID

// Button Variable
XGpio BTNInst, SSDInst;

PmodKYPD myDevice;

static TaskHandle_t xTxTask;
static TaskHandle_t xRxTask;
static QueueHandle_t xQueue = NULL;

#define DEFAULT_KEYTABLE "0FED789C456B123A"

void DemoInitialize() {
  KYPD_begin(&myDevice, XPAR_AXI_GPIO_PMOD_KEYPAD_BASEADDR);
  KYPD_loadKeyTable(&myDevice, (u8 *)DEFAULT_KEYTABLE);
}

u32 SSD_decode(u8 key_value, u8 cathode) {
  switch (key_value) {
    case 0:
      return 0b00111111 | (cathode << 7);
    case 1:
      return 0b00000110 | (cathode << 7);
    case 2:
      return 0b01011011 | (cathode << 7);
    case 3:
      return 0b01001111 | (cathode << 7);
    case 4:
      return 0b01100110 | (cathode << 7);
    case 5:
      return 0b01101101 | (cathode << 7);
    case 6:
      return 0b01111101 | (cathode << 7);
    case 7:
      return 0b00000111 | (cathode << 7);
    case 8:
      return 0b01111111 | (cathode << 7);
    case 9:
      return 0b01101111 | (cathode << 7);
    default:
      return 0b00000000 | (cathode << 7);
  }
}

static void prvTxTask(void *pvParameters) {
  UBaseType_t uxPriority;

  for (;;) {
    u16 keystate;
    XStatus status, last_status = KYPD_NO_KEY;
    u8 key, last_key = 'x', store_key;
    u32 key_stroke_on_SSD = 0;

    // Initial value of last_key cannot be contained in loaded KEYTABLE string
    Xil_Out32(myDevice.GPIO_addr, 0xF);

    xil_printf("PMOD KYPD demo started. Press any key on the Keypad.\r\n");

    uxPriority = uxTaskPriorityGet(NULL);

    while (1) {
      keystate = KYPD_getKeyStates(&myDevice);

      // Determine which single key is pressed, if any
      status = KYPD_getKeyPressed(&myDevice, keystate, &key);

      if (uxQueueMessagesWaiting(xQueue) == 2) {
        // Drop priority
        vTaskPrioritySet(xTxTask, uxPriority - 2);
      }

      // Print key detect if a new key is pressed or if status has changed
      if (status == KYPD_SINGLE_KEY &&
          (status != last_status || key != last_key)) {
        xil_printf("Key Pressed: %c\r\n", (char)key);
        last_key = key;

        if (((char)key >= 'A' && (char)key <= 'D') ||
            (char)key == 'F') {  // Ignored keys
          xil_printf("Key invalid: %c \n", (char)key);

        } else if ((char)key != 'E') {  // Normal keys
          store_key = key;

        } else if ((char)key == 'E') {  // Store key
          xil_printf("Storing the operand %d to Queue...\n", (char)store_key);
          key_stroke_on_SSD = SSD_decode((int)store_key - '0', 0);
          XGpio_DiscreteWrite(&SSDInst, 1, key_stroke_on_SSD);

          xQueueSendToBack(xQueue, (void *)&store_key, 0);

        } else if (status == KYPD_MULTI_KEY && status != last_status) {
          // Multiple keys pressed, error.
          xil_printf("Error: Multiple keys pressed\r\n");
        }

        last_status = status;
        usleep(1000);
      }
    }
  }
}

static void prvRxTask(void *pvParameters) {
  UBaseType_t uxPriority;
  uxPriority = uxTaskPriorityGet(NULL);

  for (;;) {
    u8 read_queue_value;
    u32 store_operands[2];
    int result = 0;

    unsigned int btn_value;

    xQueueReceive(xQueue, &read_queue_value, 0);
    store_operands[0] = read_queue_value - '0';
    xQueueReceive(xQueue, &read_queue_value, 0);
    store_operands[1] = read_queue_value - '0';

    btn_value = XGpio_DiscreteRead(&BTNInst, 1);

    switch (btn_value) {
      case 1:
        xil_printf("Add\r\n");
        result = store_operands[0] + store_operands[1];
        break;
      case 2:
        xil_printf("Subtract\r\n");
        result = store_operands[0] - store_operands[1];
        break;
      case 4:
        xil_printf("Multiply\r\n");
        result = store_operands[0] * store_operands[1];
        break;
      case 8:
        xil_printf("Divide\r\n");
        if (store_operands[1] == 0) {
          result = -1;
        } else {
          result = store_operands[0] / store_operands[1];
        }
        break;
    }

    xil_printf("Arithmetic operation result = %d\n\n", result);

    // SSD selection
    u8 cathode_signal = 0;

    if (result < 0) {
      xil_printf("Result is less than zero!!!\n");
    } else if (result == 0) {
      XGpio_DiscreteWrite(&SSDInst, 1, 0b00111111);
      vTaskDelay(pdMS_TO_TICKS(1000));
    }

    vTaskDelay(pdMS_TO_TICKS(1500));

    while (result > 0) {
      u32 ssd_value;
      u8 mod = result % 10;

      ssd_value = SSD_decode(mod, cathode_signal);

      XGpio_DiscreteWrite(&SSDInst, 1, ssd_value);

      result = result / 10;

      if (cathode_signal == 1) {
        cathode_signal = 0;
      } else {
        cathode_signal = cathode_signal + 1;
      }
      vTaskDelay(pdMS_TO_TICKS(1000));
    }


    // Clear both the segments after the result is displayed.
    XGpio_DiscreteWrite(&SSDInst, 1, 0b00000000);
    XGpio_DiscreteWrite(&SSDInst, 1, 0b10000000);

    // Reset priority of other task to be higher.
    vTaskPrioritySet(xTxTask, (uxPriority + 1));
  }
}

int main(void) {
  // Initialization status variable.
  int status;

  // Initialize Push Buttons
  status = XGpio_Initialize(&BTNInst, BTNS_DEVICE_ID);
  if (status != XST_SUCCESS) {
    xil_printf("GPIO Initialization for BUTTONS unsuccessful.\r\n");
    return XST_FAILURE;
  }

  // Initialize SSD
  status = XGpio_Initialize(&SSDInst, SSD_DEVICE_ID);
  if (status != XST_SUCCESS) {
    xil_printf("GPIO Initialization for SSD unsuccessful.\r\n");
    return XST_FAILURE;
  }

  // Set GPIO directions
  XGpio_SetDataDirection(&BTNInst, 1, 0b1111);
  XGpio_SetDataDirection(&SSDInst, 1, 0b0);

  xil_printf("Initialization Complete, System Ready!\n");

  // Create Tx task with a higher priority to ensure Rx stays blocked with a
  // lower priority.
  xTaskCreate(prvTxTask, (const char *)"Tx", configMINIMAL_STACK_SIZE, NULL,
              tskIDLE_PRIORITY + 2, &xTxTask);

  xTaskCreate(prvRxTask, (const char *)"Rx", configMINIMAL_STACK_SIZE, NULL,
              tskIDLE_PRIORITY + 1, &xRxTask);

  // Create a new queue to store 2 unsigned ints
  xQueue = xQueueCreate(2, sizeof(unsigned int));
  configASSERT(xQueue);

  DemoInitialize();

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}
