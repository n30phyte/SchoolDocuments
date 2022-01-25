/*
 * ECE - 315 : WINTER 2021
 * LAB 2: Implementation of UART in polled mode - PART 1
 * Created by: Shyama M. Gandhi
 */

/***************************** Include Files *********************************/
#include <stdlib.h>

#include "xil_exception.h"
#include "xil_printf.h"
#include "xparameters.h"
#include "xplatform_info.h"
#include "xscugic.h"
#include "xuartps.h"
/* FreeRTOS includes. */
#include "FreeRTOS.h"
#include "queue.h"
#include "task.h"
/************************** Constant Definitions *****************************/

// UART definitions from xparameters.h
#define UART_DEVICE_ID XPAR_XUARTPS_0_DEVICE_ID
#define UART_BASEADDR XPAR_XUARTPS_0_BASEADDR

#define CHAR_ESC 0x23 /* '#' character is used as termination sequence */
#define CHAR_CARRIAGE_RETURN \
  0x0D /* '\r' character is used in the termination sequence */

/**************************** Type Definitions *******************************/

/***************** Macros (Inline Functions) Definitions *********************/
static void TaskMsgReceiver(void *pvParameters);
static TaskHandle_t xTask_msgreceive;
static void TaskMsgProcessor(void *pvParameters);
static TaskHandle_t xTask_msgprocess;
static void TaskMsgTransmitter(void *pvParameters);
static TaskHandle_t xTask_msgtransmit;

static QueueHandle_t xQueue_12 = NULL;  // queue between task1 and task2
static QueueHandle_t xQueue_23 = NULL;  // queue between task2 and task3
/************************** Function Prototypes ******************************/

int Intialize_UART(u16 DeviceId);  // Initialization function for UART

/************************** Variable Definitions *****************************/

XUartPs Uart_PS;        /* Instance of the UART Device */
XUartPs_Config *Config; /* The instance of the UART-PS Config */

int main(void) {
  int Status;

  xTaskCreate(TaskMsgReceiver,    /* The function that implements the task. */
              (const char *)"T1", /* Text name for the task, provided to assist
                                     debugging only. */
              configMINIMAL_STACK_SIZE, /* The stack allocated to the task. */
              NULL, /* The task parameter is not used, so set to NULL. */
              tskIDLE_PRIORITY, /* The task runs at the idle priority. */
              &xTask_msgreceive);

  xTaskCreate(TaskMsgProcessor, (const char *)"T2", configMINIMAL_STACK_SIZE,
              NULL, tskIDLE_PRIORITY, &xTask_msgprocess);

  xTaskCreate(TaskMsgTransmitter, (const char *)"T3", configMINIMAL_STACK_SIZE,
              NULL, tskIDLE_PRIORITY, &xTask_msgtransmit);

  // Initialization function for UART
  Status = Intialize_UART(UART_DEVICE_ID);
  if (Status != XST_SUCCESS) {
    xil_printf("UART Polled Mode Initialization Failed\r\n");
  }

  xQueue_12 = xQueueCreate(20, sizeof(u8));

  xQueue_23 = xQueueCreate(30, sizeof(u8));

  /* Check the queue was created. */
  configASSERT(xQueue_12);
  configASSERT(xQueue_23);

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}

int Intialize_UART(u16 DeviceId) {
  int Status;

  /*
   * Initialize the UART driver so that it's ready to use.
   * Look up the configuration in the config table, then initialize it.
   */
  Config = XUartPs_LookupConfig(DeviceId);
  if (NULL == Config) {
    return XST_FAILURE;
  }

  Status = XUartPs_CfgInitialize(&Uart_PS, Config, Config->BaseAddress);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  /* Use NORMAL UART mode. */
  XUartPs_SetOperMode(&Uart_PS, XUARTPS_OPER_MODE_NORMAL);

  return XST_SUCCESS;
}

static void TaskMsgReceiver(void *pvParameters) {
  TickType_t xLastWakeTime;
  const TickType_t xFrequency = pdMS_TO_TICKS(20);
  for (;;) {
    u32 Running;
    u8 RecvChar;

    while (TRUE) {
        vTaskDelayUntil( &xLastWakeTime, xFrequency );
        if(uxQueueMessagesWaiting(xQueue_12) < 20){
            RecvChar = XUartPs_RecvByte(UART_BASEADDR);
            xQueueSend(xQueue_12, (void *)&RecvChar, portMAX_DELAY);
        }
    }
  }
}

static void TaskMsgProcessor(void *pvParameters) {
  u8 *read = malloc(1000 * sizeof(u8));
  int i = 0;

  for (;;) {
    u8 received_char;
    if (xQueueReceive(xQueue_12, (void *) &received_char, portMAX_DELAY) == pdPASS) {
      if (i < 1000) {
        read[i++] = received_char;

        if (i >= 4 && read[i - 3] == '\r' && read[i - 2] == '#' &&
            read[i - 1] == '\r') {
          for (int j = 0; j < i; j++) {
            u8 value = read[j];
            if (isalpha(value)) {
              value ^= 0x20;
            }
            xQueueSend(xQueue_23, (void *)&value, portMAX_DELAY);
          }
          i = 0;
        }
      } else {
        xil_printf("Maximum message length exceeded. Message ignored.\r\n");
        i = 0;
      }
    }
  }
  free(read);
}

static void TaskMsgTransmitter(void *pvParameters) {
  for (;;) {
    u8 write_to_console;

    while (uxQueueMessagesWaiting(xQueue_23) != 0) {
      xQueueReceive(xQueue_23, (void *)&write_to_console, 0);
      XUartPs_Send(&Uart_PS, &write_to_console, 1);
    }
  }
}
