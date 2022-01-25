/*
 * main.c
 *
 *  Created on: Mar 24, 2021
 *  Author: Shyama Gandhi
 */

#include <stdbool.h>

#include "sleep.h"
#include "stepper.h"
#include "xgpio.h"  //GPIO functions definitions
#include "xil_cache.h"
#include "xparameters.h"  //DEVICE ID, UART BASEADDRESS, GPIO BASE ADDRESS definitions
#include "xuartps.h"  //UART definitions header file

static void _Task_Uart(void *pvParameters);
static TaskHandle_t xUarttask;

static void _Task_Motor(void *pvParameters);
static TaskHandle_t xMotortask;

static void vEmergencyStop(void *pvParameters);
static TaskHandle_t xEmergencyStopTask;

int Initialize_UART();

/************************* Queue Function definitions *************************/
static QueueHandle_t xQueue_FIFO1 = NULL;  // queue between task1 and task2

/************************* Global Variables ***********************************/

// GPIO Button Instance and DEVICE ID
XGpio BTNInst;
#define EMERGENCY_STOP_BUTTON_DEVICE_ID XPAR_PMOD_BUTTONS_DEVICE_ID

// GPIO RGB led Instance and DEVICE ID
XGpio Red_RGBInst;
#define RGB_LED_DEVICE_ID XPAR_PMOD_RGB_DEVICE_ID

// struct for motor parameters
typedef struct {
  long currentposition_in_steps;
  float rotational_speed;
  float rotational_acceleration;
  float rotational_deceleration;
  long targetposition_in_steps;
  int delay_ms;
} decision_parameters;

int parameters_flag = 0;

bool emergencyStopped = false;

//----------------------------------------------------
// MAIN FUNCTION
//----------------------------------------------------
int main(void) {
  int status;
  //------------------------------------------------------
  // INITIALIZE THE PMOD GPIO PERIPHERAL FOR STEPPER MOTOR, STOP BUTTON AND RGB
  // LED(that will flash the red light when emergency stop button is pushed
  // three times).
  //------------------------------------------------------

  // Initialize the PMOD for motor signals (JC PMOD is being used)
  status = XGpio_Initialize(&PModMotorInst, PMOD_MOTOR_DEVICE_ID);
  if (status != XST_SUCCESS) {
    xil_printf("GPIO Initialization for PMOD unsuccessful.\r\n");
    return XST_FAILURE;
  }

  // button for emergency stop activation
  // Initialize the PMOD for getting the button value (btn0 is being used)
  status = XGpio_Initialize(&BTNInst, EMERGENCY_STOP_BUTTON_DEVICE_ID);
  if (status != XST_SUCCESS) {
    xil_printf("GPIO Initialization for BUTTONS unsuccessful.\r\n");
    return XST_FAILURE;
  }

  // RGB Led for flashing the red light when stop button is activated
  // Initialize the PMOD for flashing the RED light on RGB LEDz
  status = XGpio_Initialize(&Red_RGBInst, RGB_LED_DEVICE_ID);
  if (status != XST_SUCCESS) {
    xil_printf("GPIO Initialization for BUTTONS unsuccessful.\r\n");
    return XST_FAILURE;
  }

  // Initialize the UART
  status = Initialize_UART();
  if (status != XST_SUCCESS) {
    xil_printf("UART Initialization failed\n");
  }

  // Set all buttons direction to inputs
  XGpio_SetDataDirection(&BTNInst, 1, 0xFF);
  // Set the RGB LED direction to output
  XGpio_SetDataDirection(&Red_RGBInst, 1, 0x00);

  xil_printf(
      "\nStepper motor Initialization Complete! Operational parameters can be "
      "changed below:\n\n");

  xTaskCreate(_Task_Uart, (const char *)"Uart Task",
              configMINIMAL_STACK_SIZE * 10, NULL, tskIDLE_PRIORITY + 1,
              &xUarttask);

  xTaskCreate(_Task_Motor, (const char *)"Motor Task",
              configMINIMAL_STACK_SIZE * 10, NULL, tskIDLE_PRIORITY + 2,
              &xMotortask);

  xTaskCreate(vEmergencyStop, (const char *)"Emergency Stop",
              configMINIMAL_STACK_SIZE, NULL, tskIDLE_PRIORITY + 3,
              &xEmergencyStopTask);

  // the queue size if set to 25 right now, you can change this size later on
  // based on your requirements.

  xQueue_FIFO1 =
      xQueueCreate(20, sizeof(decision_parameters));  // connects task1 -> task2

  configASSERT(xQueue_FIFO1);

  vTaskStartScheduler();

  while (1)
    ;

  return 0;
}

static void _Task_Uart(void *pvParameters) {
  int message_flag = 0;
  int commandsToSend = 0;
  // this flag when a negative step value for target position is entered.
  int direction_ccw_flag = 0;

  decision_parameters params[20];

  params[commandsToSend].currentposition_in_steps = 0;
  params[commandsToSend].rotational_speed = 500;
  params[commandsToSend].rotational_acceleration = 150;
  params[commandsToSend].rotational_deceleration = 150;
  params[commandsToSend].targetposition_in_steps =
      NO_OF_STEPS_PER_REVOLUTION_FULL_DRIVE;
  params[commandsToSend].delay_ms = 0;

  while (!emergencyStopped) {
    if (message_flag == 0) {
      if (parameters_flag == 0) {
        xil_printf("Current position of the motor = %d steps\n",
                   params[commandsToSend].currentposition_in_steps);
        xil_printf(
            "Press <ENTER> to keep this value, or type a new starting position "
            "and then <ENTER>\n");
      } else if (parameters_flag == 1) {
        printf("Current maximum speed of the motor = %0.1f steps/sec\n",
               params[commandsToSend].rotational_speed);
        xil_printf(
            "Press <ENTER> to keep this value, or type a new maximum speed "
            "number and then <ENTER>\n");
      } else if (parameters_flag == 2) {
        printf(
            "Current maximum acceleration of the motor = %0.1f steps/sec/sec\n",
            params[commandsToSend].rotational_acceleration);
        xil_printf(
            "Press <ENTER> to keep this value, or type a new maximum "
            "acceleration and then <ENTER>\n");
      } else if (parameters_flag == 3) {
        printf(
            "Current maximum deceleration of the motor = %0.1f steps/sec/sec\n",
            params[commandsToSend].rotational_deceleration);
        xil_printf(
            "Press <ENTER> to keep this value, or type a new maximum "
            "deceleration and then <ENTER>\n");
      } else if (parameters_flag == 4) {
        xil_printf("Destination position of the motor = %d steps\n",
                   params[commandsToSend].targetposition_in_steps);
        xil_printf(
            "Press <ENTER> to enter new value, or type a new destination "
            "position and then <ENTER>\n");
      } else if (parameters_flag == 5) {
        xil_printf("Delay at this position = %d ms\n", 0);
        xil_printf(
            "Press <ENTER> to keep this value, or type a new delay and then "
            "<ENTER>\n");
      } else if (parameters_flag == 6) {
        xil_printf(
            "Press <ENTER> to send all destinations, or type a new destination "
            "and then <ENTER>\n");
      }
    }

    char str_value_motor_value[] = "";
    char read_UART_character[100];  // an approximate size is being taken into
                                    // consideration. You will use a larger size
                                    // if you require.
    int invalid_input_flag = 0;

    int keep_default_value_flag = 0;
    int idx = 0;
    while (1) {
      if (XUartPs_IsReceiveData(XPAR_XUARTPS_0_BASEADDR)) {
        read_UART_character[idx] =
            XUartPs_ReadReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET);
        idx++;
        if (read_UART_character[idx - 1] == 0x0D) {
          break;
        }
      }
    }

    if (idx == 1) {
      if (read_UART_character[idx - 1] == 0x0D) {
        keep_default_value_flag = 1;
        invalid_input_flag = 0;
      }
    } else {
      if (parameters_flag < 4) {
        for (int i = 0; i < idx - 1; i++) {
          if (!(read_UART_character[i] >= '0' &&
                read_UART_character[i] <= '9')) {
            invalid_input_flag = 1;
            break;
          } else {
            strncat(str_value_motor_value, &read_UART_character[i], 1);
            invalid_input_flag = 0;
          }
        }
      } else if (parameters_flag == 4 || parameters_flag == 5 ||
                 parameters_flag == 6) {
        int iterate_index = 0;
        if (read_UART_character[0] == '-') {
          direction_ccw_flag = 1;
          iterate_index = 1;
        } else
          iterate_index = 0;

        for (int i = iterate_index; i < idx - 1; i++) {
          if (!(read_UART_character[i] >= '0' &&
                read_UART_character[i] <= '9')) {
            invalid_input_flag = 1;
            break;
          } else {
            strncat(str_value_motor_value, &read_UART_character[i], 1);
            invalid_input_flag = 0;
          }
        }
      }
    }

    if (invalid_input_flag == 1) {
      message_flag = 1;
      xil_printf(
          "There was an invalid input from user except the valid inputs "
          "between 0-9\n");
      xil_printf("Please input the value of this parameter again!\n");
    } else {
      message_flag = 0;
      parameters_flag += 1;
      if (parameters_flag == 1) {
        if (keep_default_value_flag == 1) {
          xil_printf(
              "User chooses to keep the default value of current position = %d "
              "steps\n\n",
              params[commandsToSend].currentposition_in_steps);
        } else {
          params[commandsToSend].currentposition_in_steps =
              atoi(str_value_motor_value);
          xil_printf("User entered the new current position = %d steps\n\n",
                     params[commandsToSend].currentposition_in_steps);
        }
      } else if (parameters_flag == 2) {
        if (keep_default_value_flag == 1) {
          printf(
              "User chooses to keep the default value of rotational speed = "
              "%0.1f steps/sec\n\n",
              params[commandsToSend].rotational_speed);
        } else {
          params[commandsToSend].rotational_speed = atoi(str_value_motor_value);
          printf("User entered the new rotational speed = %0.1f steps/sec\n\n",
                 params[commandsToSend].rotational_speed);
        }
      } else if (parameters_flag == 3) {
        if (keep_default_value_flag == 1) {
          printf(
              "User chooses to keep the default value of rotational "
              "acceleration = %0.1f steps/sec/sec\n\n",
              params[commandsToSend].rotational_acceleration);
        } else {
          params[commandsToSend].rotational_acceleration =
              atoi(str_value_motor_value);
          printf(
              "User entered the new rotational acceleration = %0.1f "
              "steps/sec/sec\n\n",
              params[commandsToSend].rotational_acceleration);
        }
      } else if (parameters_flag == 4) {
        if (keep_default_value_flag == 1) {
          printf(
              "User chooses to keep the default value of rotational "
              "deceleration = %0.1f steps/sec/sec\n\n",
              params[commandsToSend].rotational_deceleration);
        } else {
          params[commandsToSend].rotational_deceleration =
              atoi(str_value_motor_value);
          printf(
              "User entered the new rotational deceleration = %0.1f "
              "steps/sec/sec\n\n",
              params[commandsToSend].rotational_deceleration);
        }
      } else if (parameters_flag == 5) {
        if (keep_default_value_flag == 1) {
          xil_printf(
              "User chooses to keep the default value of destination position "
              "= %d\n\n",
              params[commandsToSend].targetposition_in_steps);
        } else {
          params[commandsToSend].targetposition_in_steps =
              atoi(str_value_motor_value);
          if (direction_ccw_flag == 1) {
            params[commandsToSend].targetposition_in_steps =
                -params[commandsToSend].targetposition_in_steps;
            direction_ccw_flag = 0;
          }
          xil_printf("User entered the new destination position = %d steps\n\n",
                     params[commandsToSend].targetposition_in_steps);
        }
      } else if (parameters_flag == 6) {
        if (keep_default_value_flag == 1) {
          xil_printf("User chooses to keep the default delay of = 0 ms\n\n");
        } else {
          params[commandsToSend].delay_ms = atoi(str_value_motor_value);
          xil_printf("User entered the new delay of = %d ms\n\n",
                     params[commandsToSend].delay_ms);
        }
      } else if (parameters_flag == 7) {
        if (keep_default_value_flag == 1) {
          xil_printf(
              "\n****************************** MENU "
              "******************************\n");
          xil_printf(
              "1. Press m<ENTER> to change the motor parameters again.\n");
          xil_printf("2. Press g<ENTER> to start the movement of the motor.\n");

          char command_1_or_2_values[100];
          int index = 0;
          char command;
          while (1) {
            if (XUartPs_IsReceiveData(XPAR_XUARTPS_0_BASEADDR)) {
              command_1_or_2_values[index] =
                  XUartPs_ReadReg(XPAR_XUARTPS_0_BASEADDR, XUARTPS_FIFO_OFFSET);
              index++;
              if (command_1_or_2_values[index - 1] == 0x0D) {
                if ((index > 2) | (index == 1)) {
                  index = 0;
                } else if (index == 2) {
                  command = command_1_or_2_values[index - 2];
                  if ((command == 'm') | (command == 'g')) {
                    break;
                  } else {
                    index = 0;
                  }
                }
              }
            }
          }

          if (command == 'm') {
            parameters_flag = 0;
          } else if (command == 'g') {
            commandsToSend++;
            for (int i = 0; i < commandsToSend; i++) {
              xQueueSendToBack(xQueue_FIFO1, &params[i], 0UL);
            }
            commandsToSend = 0;
            parameters_flag = 0;
            taskYIELD();
            Stepper_setCurrentPositionInSteps(0);
          }
        } else {
          commandsToSend++;

          params[commandsToSend].currentposition_in_steps = 0;
          params[commandsToSend].rotational_speed = 500;
          params[commandsToSend].rotational_acceleration = 150;
          params[commandsToSend].rotational_deceleration = 150;

          params[commandsToSend].targetposition_in_steps =
              atoi(str_value_motor_value);
          params[commandsToSend].delay_ms = 0;
          if (direction_ccw_flag == 1) {
            params[commandsToSend].targetposition_in_steps =
                -params[commandsToSend].targetposition_in_steps;
            direction_ccw_flag = 0;
          }
          xil_printf("User entered the new destination position = %d steps\n\n",
                     params[commandsToSend].targetposition_in_steps);

          parameters_flag -= 2;
        }
      }
    }
    vTaskDelay(1);
  }
  vTaskDelete(NULL);
}

/*-----------------------------------------------------------*/
static void _Task_Motor(void *pvParameters) {
  decision_parameters read_motor_parameters_from_queue;

  while (!emergencyStopped) {
    if (xQueueReceive(xQueue_FIFO1, &read_motor_parameters_from_queue, 0UL)) {
      Stepper_PMOD_pins_to_output();
      Stepper_Initialize();

      xil_printf("\nStarting the Motor Rotation...\n");

      Stepper_setSpeedInStepsPerSecond(
          read_motor_parameters_from_queue.rotational_speed);
      Stepper_setAccelerationInStepsPerSecondPerSecond(
          read_motor_parameters_from_queue.rotational_acceleration);
      Stepper_setDecelerationInStepsPerSecondPerSecond(
          read_motor_parameters_from_queue.rotational_deceleration);
      Stepper_setCurrentPositionInSteps(
          read_motor_parameters_from_queue.currentposition_in_steps);
      Stepper_SetupMoveInSteps(
          read_motor_parameters_from_queue.targetposition_in_steps);

      while (!Stepper_motionComplete()) {
        Stepper_processMovement();
      }
      if (read_motor_parameters_from_queue.delay_ms > 0) {
        vTaskDelay(pdMS_TO_TICKS(read_motor_parameters_from_queue.delay_ms));
      }
    }
    vTaskDelay(1);
  }

  Stepper_disableMotor();
  vTaskDelete(NULL);
}

static void vEmergencyStop(void *pvParameters) {
  TickType_t xFrequency = pdMS_TO_TICKS(10);  // 100 Hz -> 10 ms
  TickType_t xLastWakeTime = xTaskGetTickCount();

  BaseType_t stopCounter = 0;

  while (!emergencyStopped) {
    vTaskDelayUntil(&xLastWakeTime, xFrequency);
    u32 buttonState = XGpio_DiscreteRead(&BTNInst, 1);
    if (buttonState != 0) {
      stopCounter++;
    } else {
      stopCounter = 0;
    }

    if (stopCounter >= 3) {
      emergencyStopped = true;
      xil_printf("Stopping motor");
      Stepper_SetupStop();
      xLastWakeTime = xTaskGetTickCount();
      xFrequency = pdMS_TO_TICKS(50);  // 20 Hz -> 50 ms
    }
  }

  while (emergencyStopped) {
    XGpio_DiscreteWrite(&Red_RGBInst, 1, 1);
    vTaskDelayUntil(&xLastWakeTime, xFrequency);
    XGpio_DiscreteWrite(&Red_RGBInst, 1, 0);
    vTaskDelayUntil(&xLastWakeTime, xFrequency);
  }
}
