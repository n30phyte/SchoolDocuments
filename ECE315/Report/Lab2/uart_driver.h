/*
 * uart_driver.h
 *
 *  Created on: Feb 28, 2021
 *      Author: Shyama M. Gandhi
 *
 *      Students are to add the driver code that has their definitions functions
 * in this file and use them to implement the interrupt method for reception as
 * well as transmission side. The current template uses interrupt method for
 * receiving and polling method for transmitting the data on the UART. Use the
 * lab manual instructions to implement the required interrupt method for
 * reception as well as transmission on UART. Please make sure that you use the
 * newly created functions to achieve it.
 *
 *      When using the idea to set threshold in Receive buffer, hint: have a
 * look at the function, XUartPs_SetFifoThreshold(UartInstancePtr,
 * UART_RX_BUFFER_SIZE);
 *
 */

#ifndef SRC_UART_DRIVER_H_
#define SRC_UART_DRIVER_H_

#include "xil_io.h"
#include "xscugic.h"  //interrupt controller header file
#include "xuartps.h"  //UART definitions header file

/* FreeRTOS includes. */
#include "FreeRTOS.h"
#include "queue.h"
#include "semphr.h"
#include "task.h"

// UART Interrupt definitions
#define INTC XScuGic                             // Interrupt controller
#define UART_DEVICE_ID XPAR_XUARTPS_0_DEVICE_ID  // UART Device ID
#define INTC_DEVICE_ID \
  XPAR_SCUGIC_SINGLE_DEVICE_ID               // Interrupt controller device ID
#define UART_INT_IRQ_ID XPAR_XUARTPS_1_INTR  // UART interrupt identifier
#define UART_RX_BUFFER_SIZE 3U               // 5 bytes from host
#define SIZE_OF_QUEUE 100

void Task_UART_buffer_receive(void *p);
void Task_UART_buffer_send(void *p);

// UART interrupt control ISR declaration
void Interrupt_Handler(void *CallBackRef, u32 Event, unsigned int EventData);

XUartPs UART;              // UART Instance
XUartPs_Config *Config;    // Pointer to UART
INTC InterruptController;  // Interrupt controller instance
u32 IntrMask;  // interrupt mask variable to be used to enable different type of
               // interrupt on Rx and Tx in UART

QueueHandle_t xQueueRx;
QueueHandle_t xQueueTx;

int interrupt_counter = 0;
int CountRxIrq = 0;
int CountTxIrq = 0;

int Initialize_UART() {
  int Status;

  Config = XUartPs_LookupConfig(UART_DEVICE_ID);
  if (NULL == Config) {
    return XST_FAILURE;
    xil_printf("UART PS Config failed\n");
  }

  // Initialize UART
  Status = XUartPs_CfgInitialize(&UART, Config, Config->BaseAddress);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
    xil_printf("UART PS init failed\n");
  }
  ResetStats();

  return XST_SUCCESS;
}

// Function for interrupt setup
int SetupInterruptSystem(INTC *IntcInstancePtr, XUartPs *UartInstancePtr,
                         u16 UartIntrId) {
  int Status;
  XScuGic_Config *IntcConfig;  // Config pointer for interrupt controller

  // Lookup the config information for interrupt controller
  IntcConfig = XScuGic_LookupConfig(INTC_DEVICE_ID);
  if (NULL == IntcConfig) {
    return XST_FAILURE;
  }

  // Initialize interrupt controller
  Status = XScuGic_CfgInitialize(IntcInstancePtr, IntcConfig,
                                 IntcConfig->CpuBaseAddress);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  // Connect the interrupt controller interrupt handler
  Xil_ExceptionRegisterHandler(XIL_EXCEPTION_ID_INT,
                               (Xil_ExceptionHandler)XScuGic_InterruptHandler,
                               IntcInstancePtr);

  // Connect the PS UART interrupt handler
  // The interrupt handler whichhandles the interrupts for the UART peripheral
  // is connected to it's unique ID number (82 in this case)
  Status = XScuGic_Connect(IntcInstancePtr, UartIntrId,
                           (Xil_ExceptionHandler)XUartPs_InterruptHandler,
                           (void *)UartInstancePtr);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  // Enable the UART interrupt input on the interrupt controller
  XScuGic_Enable(IntcInstancePtr, UartIntrId);

  // Enable the processor interrupt handling on the ARM processor
  Xil_ExceptionEnable();

  // Setup the UART Interrupt handler function
  XUartPs_SetHandler(UartInstancePtr, (XUartPs_Handler)Interrupt_Handler,
                     UartInstancePtr);

  // Create mask for UART interrupt, Enable the interrupt when the receive
  // buffer has reached a particular threshold
  IntrMask = XUARTPS_IXR_TOUT | XUARTPS_IXR_PARITY | XUARTPS_IXR_FRAMING |
             XUARTPS_IXR_OVER | XUARTPS_IXR_TXEMPTY | XUARTPS_IXR_RXFULL |
             XUARTPS_IXR_RXOVR;

  // Setup the UART interrupt Mask
  XUartPs_SetInterruptMask(UartInstancePtr, IntrMask);

  // Setup the PS UART to Work in Normal Mode
  XUartPs_SetOperMode(UartInstancePtr, XUARTPS_OPER_MODE_NORMAL);

  return XST_SUCCESS;
}

// PS UART Interrupt Subroutine
void Interrupt_Handler(void *CallBackRef, u32 Event, unsigned int EventData) {
  u8 data;
  BaseType_t xHigherPriorityTaskWoken = pdFALSE;
  UBaseType_t uxSavedInterruptStatus;
  
  uxSavedInterruptStatus = taskENTER_CRITICAL_FROM_ISR();
  if (Event == XUARTPS_EVENT_RECV_DATA) {
    while (XUartPs_IsReceiveData(XPAR_XUARTPS_0_BASEADDR)) {
      data = XUartPs_ReadReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET);
      xQueueSendFromISR(xQueueRx, (void *)&data, &xHigherPriorityTaskWoken);
      interrupt_counter++;
    }
    CountRxIrq++;
  } else if (Event == XUARTPS_EVENT_SENT_DATA) {
    while (!XUartPs_IsTransmitFull(XPAR_XUARTPS_0_BASEADDR)) {
      if (xQueueReceiveFromISR(xQueueTx, &data, &xHigherPriorityTaskWoken)) {
        XUartPs_WriteReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET, data);
        interrupt_counter++;
      } else {
        XUartPs_SetInterruptMask(
            &UART, XUartPs_GetInterruptMask(&UART) & ~XUARTPS_IXR_TXEMPTY);
        break;
      }
    }
    CountTxIrq++;
  }
  taskEXIT_CRITICAL_FROM_ISR(uxSavedInterruptStatus);

  portYIELD_FROM_ISR(xHigherPriorityTaskWoken);
}

void ResetStats() {
  interrupt_counter = 0;
  CountTxIrq = 0;
  CountRxIrq = 0;
}

void SendStats() {
  xil_printf("Number of bytes processed: %d\n", interrupt_counter);
  xil_printf("Number of Rx interrupts: %d\n", CountTxIrq);
  xil_printf("Number of Tx interrupts: %d\n", CountRxIrq);
}

BaseType_t MyIsReceiveData() { return uxQueueMessagesWaiting(xQueueRx) != 0; }

u8 MyReceiveByte() {
  u8 data;
  xQueueReceive(xQueueRx, (void *)&data, 0);

  return data;
}

BaseType_t MyIsTransmitFull() {
  return uxQueueMessagesWaiting(xQueueTx) == SIZE_OF_QUEUE;
}

void MySendByte(u8 data) {
  taskENTER_CRITICAL();

  XUartPs_SetInterruptMask(
      &UART, XUartPs_GetInterruptMask(&UART) | XUARTPS_IXR_TXEMPTY);

  if (XUartPs_IsTransmitFull(XPAR_XUARTPS_0_BASEADDR)) {
    xQueueSend(xQueueTx, (void *)&data, 0);
  } else {
    XUartPs_WriteReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET, data);
  }

  taskEXIT_CRITICAL();
}

#endif /* SRC_UART_DRIVER_H_ */
