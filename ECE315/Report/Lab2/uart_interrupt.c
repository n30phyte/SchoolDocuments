/*
 * uart_driver.h
 *
 *  Created on: Feb 28, 2021
 *      Author: Shyama M. Gandhi
 *
 *     This is the main file that uses the Xilinx the "uart_driver.h" that you
 * will modify in this lab. Receive side: implemented in interrupt mode Transmit
 * side: implemented in polling mode Hints about the places where you will find
 * info for creating four drivers functions and using them:
 *     1. xuartps_hw.h
 *     2. xuartps.h
 *     3. Use the lecture slides to get more idea about the UART functionality
 *     4. Introduction presentation
 *
 *     There are two tasks in this file. One task will block on the receive
 * queue and will pass any characters if present in the transmit queue. Another
 * task transfers bytes from the transmit queue into the TxFIFO and hence you
 * see the echoed characters back on the console based on what you typed.
 *
 *     Study the Interrupt Mask function XUartPs_SetInterruptMask() carefully.
 *     This functions has details that can be used to enable different types of
 * interrupts in Tx and Rx direction.
 *
 */

#include "stdio.h"
#include "xil_printf.h"
#include "xil_types.h"
#include "xparameters.h"
#include "xtime_l.h"

// this is the uart driver file where students will add the implementation as
// mentioned in the lab manual
#include "uart_driver.h"

TaskHandle_t task_receiveuarthandle = NULL;
TaskHandle_t task_transmituarthandle = NULL;

// Function declaration for UART interrupt setup
extern int SetupInterruptSystem(INTC *IntcInstancePtr, XUartPs *UartInstancePtr,
                                u16 UartIntrId);
// Initialization function for UART
extern int Initialize_UART();
extern void Transmit_Byte(u8 data);

extern QueueHandle_t xQueueRx;
extern QueueHandle_t xQueueTx;

// functions to be implemented by students
extern BaseType_t MyIsReceiveData();
extern u8 MyReceiveByte();
extern BaseType_t MyIsTransmitFull();
extern void MySendByte(u8 Data);
extern void ResetStats();
extern void SendStats();

int main() {
  int Status;
  xTaskCreate(Task_UART_buffer_receive, "uart_receive_task", 1024, (void *)0,
              tskIDLE_PRIORITY, &task_receiveuarthandle);
  // Set up queues
  xQueueRx = xQueueCreate(SIZE_OF_QUEUE, sizeof(u8));
  xQueueTx = xQueueCreate(SIZE_OF_QUEUE, sizeof(u8));

  Status = Initialize_UART();
  if (Status != XST_SUCCESS) {
    xil_printf("UART Initialization failed\n");
  }

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}

void Task_UART_buffer_receive(void *p) {
  int Status;

  Status = SetupInterruptSystem(&InterruptController, &UART, UART_INT_IRQ_ID);
  if (Status != XST_SUCCESS) {
    xil_printf("UART PS interrupt failed\n");
  }

  u8 ringbuf[3];

  while (1) {
    while (!MyIsReceiveData())
      ;
    u8 data;
    data = MyReceiveByte();

    while (MyIsTransmitFull())
      ;
    MySendByte(data);

    ringbuf[2] = ringbuf[1];
    ringbuf[1] = ringbuf[0];
    ringbuf[0] = data;

    if (ringbuf[0] == '\r' && ringbuf[2] == '\r') {
      if (ringbuf[1] == '%') {
        ResetStats();
      } else if (ringbuf[1] == '#') {
        SendStats();
      }
    }
  }
}
