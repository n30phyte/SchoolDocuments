/**
 * ECE 315 Wi21 Lab 1 Part 3
 * Simple Calculator
 *
 * Gets input from Digilent KYPD PMOD and outputs the calculated value through
 * serial printing.
 *
 * Addition, subtraction, multiplication and factorial are all supported.
 * Overflow is detected for addition and multiplication.
 *
 * Created on: 6 February 2021 by Shyama M. Gandhi
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

// KYPD PMOD device handle.
PmodKYPD myDevice;

static TaskHandle_t xTxTask;
static TaskHandle_t xRxTask;
static QueueHandle_t xQueue = NULL;

#define DEFAULT_KEYTABLE "0FED789C456B123A"

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

void DemoInitialize() {
  KYPD_begin(&myDevice, XPAR_AXI_GPIO_PMOD_KEYPAD_BASEADDR);
  KYPD_loadKeyTable(&myDevice, (u8 *)DEFAULT_KEYTABLE);
}

typedef enum Operation { ADD, SUBTRACT, MULTIPLY, FACTORIAL } Operation;

static void prvTxTask(void *pvParameters) {
  UBaseType_t uxPriority;

  for (;;) {
    u16 keystate;
    XStatus status, last_status = KYPD_NO_KEY;
    u8 key, last_key = 'x';
    u32 factor = 0, current_value = 0;

    Xil_Out32(myDevice.GPIO_addr, 0xF);
    xil_printf("PMOD KYPD demo started. Press any key on the Keypad.\r\n");

    uxPriority = uxTaskPriorityGet(NULL);

    while (1) {
      // Capture state of each key
      keystate = KYPD_getKeyStates(&myDevice);

      // Determine which single key is pressed, if any
      status = KYPD_getKeyPressed(&myDevice, keystate, &key);

      // When queue is full, drop task priority to let calculation task run.
      if (uxQueueMessagesWaiting(xQueue) == 3) {
        vTaskPrioritySet(xTxTask, uxPriority - 1);
      }

      // Print key detect if a new key is pressed or if status has changed
      if (status == KYPD_SINGLE_KEY &&
          (status != last_status || key != last_key)) {
        xil_printf("Key Pressed: %c\r\n", (char)key);
        last_key = key;

        // whenever 'F' is pressed, the aggregated number will be registered as
        // an operand
        if ((char)key == 'F') {
          current_value = current_value * 10 + factor;

          xQueueSendToBack(xQueue, (void *)&current_value, 0);

          current_value = 0;
        }
        // case when we consider input key strokes from '0' to '9' (only these
        // are the valid key inputs for arithmetic operation)
        else if ((char)key == 'E') {
          current_value = current_value * 10 + factor;
        }
        // if user presses 'E' key, consider the last input key pressed as the
        // operand's new digit
        else if ((int)key >= 48 && (int)key <= 57) {
          factor = (int)key - 48;
        } else if ((uxQueueMessagesWaiting(xQueue) == 2) &&
                   ((char)key == 'A' || (char)key == 'B' || (char)key == 'C' ||
                    (char)key == 'D')) {
          // Store the operation into the queue
          u8 operation = key - 'A';

          xQueueSendToBack(xQueue, (void *)&operation, 0);

          current_value = 0;
        }
      } else if (status == KYPD_MULTI_KEY && status != last_status) {
        xil_printf("Error: Multiple keys pressed\r\n");
      }
      last_status = status;
      usleep(1000);
    }
  }
}

u32 factorial(int n1) {
  u32 factorial_answer = 1;

  for (int i = 1; i <= n1; i++) {
    factorial_answer *= i;
  }

  return factorial_answer;
}

static void prvRxTask(void *pvParameters) {
  UBaseType_t uxPriority;
  uxPriority = uxTaskPriorityGet(NULL);

  for (;;) {
    u32 read_queue_value;
    u32 store_operands[2];
    int result = 0;

    u32 fact_operand;

    xQueueReceive(xQueue, &read_queue_value, 0);
    store_operands[0] = read_queue_value;
    xQueueReceive(xQueue, &read_queue_value, 0);
    store_operands[1] = read_queue_value;

    xQueueReceive(xQueue, &read_queue_value, 0);

    Operation op = (Operation)read_queue_value;

    switch (op) {
      case ADD:
        result = store_operands[0] + store_operands[1];

        if ((store_operands[0] > 0 && store_operands[1] > 0 && result < 0) ||
            (store_operands[0] < 0 && store_operands[1] < 0 && result > 0)) {
          xil_printf("Overflowed!");
        } else {
            xil_printf("Result: %d + %d = %d\r\n", store_operands[0],
                       store_operands[1], result);
        }

        break;
      case SUBTRACT:
        result = store_operands[0] - store_operands[1];
        xil_printf("Result: %d - %d = %d\r\n", store_operands[0],
                   store_operands[1], result);
        break;
      case MULTIPLY:
        result = store_operands[0] * store_operands[1];

        if (store_operands[0] != 0 &&
            (result / store_operands[0] != store_operands[1])) {
          xil_printf("Overflowed!");
        } else {
            xil_printf("Result: %d x %d = %d\r\n", store_operands[0],
            		store_operands[1], result);
        }

        break;
      case FACTORIAL:
        fact_operand = MIN(store_operands[0], store_operands[1]);
        result = factorial(fact_operand);
        xil_printf("Result: %d! = %d\r\n", fact_operand, result);
        break;
      default:
        result = 0;
        break;
    }

    xQueueReset(xQueue);
    vTaskPrioritySet(xTxTask, (uxPriority + 1));
  }
}

int main(void) {
  xil_printf("System Ready!\n");

  xTaskCreate(prvTxTask, (const char *)"Tx", configMINIMAL_STACK_SIZE, NULL,
              tskIDLE_PRIORITY + 2, &xTxTask);

  xTaskCreate(prvRxTask, (const char *)"Rx", configMINIMAL_STACK_SIZE, NULL,
              tskIDLE_PRIORITY + 1, &xRxTask);

  xQueue = xQueueCreate(3, sizeof(unsigned int));
  configASSERT(xQueue);

  DemoInitialize();

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}
