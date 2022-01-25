/***************************************************************************
 * ECE - 315 : WINTER 2021
 * LAB 3: Implementation of SPI in Zynq-7000
 * Created by: Shyama M. Gandhi
 *
 * This lab uses SPI in polled mode. The hardware diagram has a loop back
 * connection hard coded where SPI0 - MASTER and SPI1 - SLAVE. In this code SPI0
 * MASTER writes to SPI1 slave. The data received by SPI1 is transmitted back to
 * the SPI0 master. The driver function used to achieve this are provided by
 * Xilinx as xspips.h and xspipshw_h. They are present in the provided
 * initialization.h header file.
 *
 * FOUR TASKS IN THIS FILE.
 * There are three commands in the menu (options in the menu).
 * 1. Enable loopback for Uart manager task
 * 2. Enable loopback for  spi0-spi1 connection enable or disable(loopback mode)
 * 3. Calibrate the load generator using the global loop_count variable(STUDENTS
 * WILL CREATE THIS COMMAND OPTION AND WORK ACCORDINGLY) (you also have to write
 * the code to detect this command selection and then work accordingly as
 * mentioned in the handout).
 *
 * User enters the command in following ways:
 * for example, user wants to select 1. command is only detected by using
 * "enter, command, enter" same goes for 2 as well as 3 option of the menu.
 *
 * "ENTER, #, ENTER" IS USED TO END THE TEXT ENTRY OF LOOPBACK UART MANAGER TASK
 * AND IT CHANGES TO THE DISABLED MODE. SAME WAY, "ENTER, #, ENTER" IS ALSO USED
 * TO END THE TEXT ENTRY FOR LOOPBACK OF TaskSpi0Master AND CHANGES IT TO
 * DISBALE MODE.
 *
 * When spi_master_loopback_en=0, the data entered by the user will loop back
 * from sp1 to spi0 and again spi1. Currently, the case spi_master_loopback_en=0
 * will only echo whatever is typed back, via the FIFO2. This is the SPI0-SPI1
 * loopback.
 */

/*****************************  FreeRTOS includes. ****************************/
#include <stdio.h>

#include "FreeRTOS.h"
#include "queue.h"
#include "task.h"

/***************************** Include Files *********************************/

#include <ctype.h>

#include "xil_printf.h"
#include "xparameters.h" /* SDK generated parameters */
#include "xspips.h"      /* SPI device driver */

/********************** User defined Include Files **************************/
#include "initialization.h"

/************************** Constant Definitions *****************************/
#define CHAR_POUND_HASH 0x23 /* '#' character is used as termination sequence \
                              */
#define CHAR_CARRIAGE_RETURN \
  0x0D /* '\r' character is used in the termination sequence */

#define UART_DEVICE_ID_0 XPAR_XUARTPS_0_DEVICE_ID
#define SPI_0_DEVICE_ID XPAR_XSPIPS_0_DEVICE_ID
#define SPI_1_DEVICE_ID XPAR_XSPIPS_1_DEVICE_ID

/***************** Macros (Inline Functions) Definitions *********************/
/************************* Task Function definitions *************************/
static void TaskUartManager(void *pvParameters);
static TaskHandle_t xTask_uart;

static void TaskSpi0Master(void *pvParameters);
static TaskHandle_t xTask_spi0;

static void TaskSpi1Slave(void *pvParameters);
static TaskHandle_t xTask_spi1;

static void TaskCpuLoadGen(void *pvParameters);
static TaskHandle_t xTask_cpuload;

/************************* Queue Function definitions *************************/

static QueueHandle_t xQueue_FIFO1 = NULL;  // queue between task1 and task2
static QueueHandle_t xQueue_FIFO2 = NULL;  // queue between task1 and task2

/************************* Global Variables *********************************/

BaseType_t spi_master_loopback_en =
    0;  // GLOBAL variable to enable/disable loopback for spi_master() task
        // (i.e., task number 2)
u8 OPTIONS_IN_MENU = '3';        // total options in the menu display
u32 TRANSFER_SIZE_IN_BYTES = 1;  // 1 bytes transferred between SPI 0 and SPI 1
                                 // in the provided template everytime
static u64 loop_count =
    10000;  // GLOBAL variable for loop_count to generate a fake CPU load

u32 flag = 0;
u32 current_command_execution_flag = 0;

volatile u32 bytesSent = 0;
static const char CONTROL_CHARACTER =
    0x07;  // Bell character used as control character

int main(void) {
  int Status;

  xTaskCreate(TaskUartManager, (const char *)"TASK1", configMINIMAL_STACK_SIZE,
              NULL, tskIDLE_PRIORITY + 4, &xTask_uart);

  xTaskCreate(TaskSpi0Master, (const char *)"TASK2", configMINIMAL_STACK_SIZE,
              NULL, tskIDLE_PRIORITY + 3, &xTask_spi0);

  xTaskCreate(TaskSpi1Slave, (const char *)"TASK3", configMINIMAL_STACK_SIZE,
              NULL, tskIDLE_PRIORITY + 3, &xTask_spi1);

  xTaskCreate(TaskCpuLoadGen, (const char *)"TASK4", configMINIMAL_STACK_SIZE,
              NULL, tskIDLE_PRIORITY + 2, &xTask_cpuload);

  xQueue_FIFO1 = xQueueCreate(500, sizeof(u8));  // connects task1 -> task2
  xQueue_FIFO2 = xQueueCreate(500, sizeof(u8));  // connects task2 -> task1

  /* Check the xQueue_FIFO1 and xQueue_FIFO2 if they were created. */
  configASSERT(xQueue_FIFO1);
  configASSERT(xQueue_FIFO2);

  // Initialization function for UART
  Status = Intialize_UART(UART_DEVICE_ID_0);
  if (Status != XST_SUCCESS) {
    xil_printf("UART Initialization Failed\r\n");
  }

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}

static void TaskUartManager(void *pvParameters) {
  print_command_menu();

  u8 RecvChar = 0;
  u8 RecvChar_1 = 0;
  u8 task1_receive_from_FIFO2;
  u8 task1_receive_from_FIFO2_spi_data;
  u32 end_sequence_detect_flag = 0;

  u32 valid_command_detect_flag = 0;

  BaseType_t task1_uart_loopback_en = 0;

  u64 newNum = 0;

  while (1) {
    /* Wait until there is data */

    while (XUartPs_IsReceiveData(XPAR_XUARTPS_0_BASEADDR)) {
      RecvChar_1 = RecvChar;
      RecvChar = XUartPs_ReadReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET);

      if (current_command_execution_flag == 2) {
        xQueueSendToBack(xQueue_FIFO1, &RecvChar, 0UL);

        if (spi_master_loopback_en == 1) {
          xQueueReceive(xQueue_FIFO2, &task1_receive_from_FIFO2, portMAX_DELAY);
          while (XUartPs_IsTransmitFull(XPAR_XUARTPS_0_BASEADDR) == TRUE) {
          };
          XUartPs_WriteReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET,
                           task1_receive_from_FIFO2);
        } else {
          xQueueReceive(xQueue_FIFO2, &task1_receive_from_FIFO2_spi_data,
                        portMAX_DELAY);
          while (XUartPs_IsTransmitFull(XPAR_XUARTPS_0_BASEADDR) == TRUE) {
          };
          XUartPs_WriteReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET,
                           task1_receive_from_FIFO2_spi_data);
        }
      } else if (current_command_execution_flag == 3) {
        if (RecvChar == '\r') {
          if (newNum == 0) {
            xil_printf("\nCancelled\r\n");
          } else {
            char *buffer = pvPortMalloc(sizeof(char) * 50);
            loop_count = newNum;
            snprintf(buffer, 50, "\nNew value of loop_count: %llu\r\n",
                     loop_count);
            xil_printf(buffer);
            vPortFree(buffer);
            newNum = 0;
          }
          current_command_execution_flag = 0;
        } else if (isdigit(RecvChar)) {
          newNum *= 10;
          newNum += (RecvChar - '0');
        } else {
          xil_printf("\nNot a number!\r\n");
          xil_printf("\nCancelled\r\n");
          current_command_execution_flag = 0;
        }
      }

      if ((RecvChar == 0x0D) && valid_command_detect_flag == 0) {
        valid_command_detect_flag = valid_command_detect_flag + 1;
      } else if ((isdigit((int)RecvChar) &&
                  ((int)RecvChar <= OPTIONS_IN_MENU)) &&
                 valid_command_detect_flag == 1) {
        valid_command_detect_flag = valid_command_detect_flag + 1;
      } else if (RecvChar == 0x0D) {
        valid_command_detect_flag = valid_command_detect_flag + 1;
      } else {
        valid_command_detect_flag = 0;
      }

      if (valid_command_detect_flag == 3) {
        if (RecvChar_1 == '1') {
          current_command_execution_flag = 1;
          task1_uart_loopback_en = !task1_uart_loopback_en;
          if (task1_uart_loopback_en == 1) {
            xil_printf("\n*** UART Manager Task loopback enabled ***\r\n");
          } else
            xil_printf(
                "\n*** UART Manager Task loopback disabled using command "
                "toggling***\r\n");
        } else if (RecvChar_1 == '2') {
          current_command_execution_flag = 2;
          spi_master_loopback_en = !spi_master_loopback_en;
          if (spi_master_loopback_en == 1) {
            xil_printf(
                "\n*** Task2 loopback enabled : No access to SPI0 interface "
                "***\r\n");
          } else {
            xil_printf(
                "\n*** Task2 loopback disabled using command toggling : "
                "SPI0-SPI1 in effect. Send "
                "the bytes from the console ***\r\n");
          }
        } else if (RecvChar_1 == '3') {
          char *buffer = pvPortMalloc(sizeof(char) * 50);
          snprintf(buffer, 50, "\nCurrent value of loop_count: %llu\r\n",
                   loop_count);
          xil_printf(buffer);
          xil_printf("\nPlease input new value:\r\n");
          vPortFree(buffer);
          current_command_execution_flag = 3;
        }
      }

      if ((current_command_execution_flag == 1 && task1_uart_loopback_en == 1) |
          (current_command_execution_flag == 2 &&
           spi_master_loopback_en == 1)) {
        if (RecvChar == CHAR_CARRIAGE_RETURN && end_sequence_detect_flag == 2) {
          end_sequence_detect_flag += 1;
        } else if (RecvChar == CHAR_POUND_HASH && end_sequence_detect_flag == 1)
          end_sequence_detect_flag += 1;
        else if (RecvChar == CHAR_CARRIAGE_RETURN &&
                 end_sequence_detect_flag == 0)
          end_sequence_detect_flag += 1;
        else
          end_sequence_detect_flag = 0;

        if (current_command_execution_flag == 1 && task1_uart_loopback_en == 1)
          XUartPs_SendByte(XPAR_XUARTPS_0_BASEADDR, RecvChar);
      }

      if (end_sequence_detect_flag == 3) {
        end_sequence_detect_flag = 0;
        if (current_command_execution_flag == 1) {
          task1_uart_loopback_en = 0;
          xil_printf(
              "\n*** UART Manager Task loopback disabled using termination "
              "sequence***\r\n");
        } else if (current_command_execution_flag == 2) {
          spi_master_loopback_en = 0;
          xil_printf(
              "\n*** Task2 loopback disabled using termination sequence : "
              "SPI0-SPI1 connection in "
              "effect. Send the bytes from the console ***\r\n");
        }
      }
    }

    if (bytesSent) {
      xQueueSendToBack(xQueue_FIFO1, &CONTROL_CHARACTER, 0);

      xQueueReceive(xQueue_FIFO2, &task1_receive_from_FIFO2_spi_data,
                    portMAX_DELAY);

      XUartPs_SendByte(XPAR_XUARTPS_0_BASEADDR,
                       task1_receive_from_FIFO2_spi_data);
      bytesSent--;
    }

    vTaskDelay(1);
  }
}
static void TaskSpi0Master(void *pvParameters) {
  u8 task2_receive_from_FIFO1;
  u8 send_SPI_data_via_FIFO2;
  u32 bytecount = 0;
  u8 send_buffer[1];

  Initialize_SPI_0_and_1(SPI_0_DEVICE_ID, SPI_1_DEVICE_ID);

  while (1) {
    if (xQueueReceive(xQueue_FIFO1, &task2_receive_from_FIFO1, 0)) {
      if (spi_master_loopback_en == 1 &&
          current_command_execution_flag ==
              2)  // just send the characters back to task 1. No SPI access!
        xQueueSendToBack(xQueue_FIFO2, &task2_receive_from_FIFO1, 0UL);
      else if (spi_master_loopback_en == 0 &&
               current_command_execution_flag ==
                   2) {  // if global variable spi_master_loopback_en=0, we
                         // enable SPI loopback transfer
        send_buffer[bytecount] = task2_receive_from_FIFO1;
        bytecount++;
        if (bytecount ==
            TRANSFER_SIZE_IN_BYTES) {  // byte is read from the FIFO1 and then
                                       // it is transferred to the SPI
                                       // connection
          SpiMasterWrite(
              send_buffer,
              TRANSFER_SIZE_IN_BYTES);  // the byte read back from the SPI
                                        // master Rx FIFO is then given back to
                                        // the UART Manager task using FIFO2
          taskYIELD();
          send_SPI_data_via_FIFO2 = SpiMasterRead(TRANSFER_SIZE_IN_BYTES);
          xQueueSendToBack(xQueue_FIFO2, &send_SPI_data_via_FIFO2, 0UL);
          bytecount = 0;
        }
      }
    }

    vTaskDelay(1);
  }
}

static void TaskSpi1Slave(void *pvParameters) {
  int count = 0;
  u8 ringBuf[3];
  u8 *outputBuf = pvPortMalloc(200 * sizeof(u8));
  u8 *statBuf = pvPortMalloc(200 * sizeof(u8));
  int sendIdx = 0;

  while (1) {
    if (spi_master_loopback_en == 0 && current_command_execution_flag == 2) {
      SpiSlaveRead(TRANSFER_SIZE_IN_BYTES);  // slave SPI reads the data from
                                             // it's Rx FIFO
      buffer_Rx_Master = RxBuffer_Slave;
      if (*buffer_Rx_Master != CONTROL_CHARACTER) {
        SpiSlaveWrite(buffer_Rx_Master,
                      TRANSFER_SIZE_IN_BYTES);  // slave SPI then writes the
                                                // bytes back on it's Tx FIFO
        count++;

        ringBuf[2] = ringBuf[1];
        ringBuf[1] = ringBuf[0];
        ringBuf[0] = *buffer_Rx_Master;

        if (ringBuf[2] == '\r' && ringBuf[0] == '\r' && ringBuf[1] == '#') {
          vTaskGetRunTimeStats(statBuf);
          bytesSent =
              snprintf(outputBuf, 200,
                       "The number of characters received over SPI: %d.\n%s",
                       count - 3, statBuf);
          count = 0;
          sendIdx = 0;
        }
      } else {
        SpiSlaveWrite(outputBuf + sendIdx, TRANSFER_SIZE_IN_BYTES);
        sendIdx++;
      }
    }
    vTaskDelay(1);
  }
}

// Generate fake load on the CPU controlled by the variable u64 loop_count.
static void TaskCpuLoadGen(void *pvParameters) {
  u32 var = 0x12345678;
  while (1) {
    for (u64 i = 0; i < loop_count; i++) {
      var = var ^ 0xFFFFFFFF;
    }
    vTaskDelay(1);
  }
}
