/*
 * initialization.h
 *
 *  Created on	: Mar 14, 2021
 *  Author		: Shyama M. Gandhi
 *  This file has been created using inbuilt driver functions for UART and SPI.
 *  The file also has a print_command_menu() function for displaying the menu on console.
 *  SPI 0 as MASTER, SPI 1 as SLAVE
 *
 */

#ifndef SRC_INITIALIZATION_H_
#define SRC_INITIALIZATION_H_


#include "xspips.h"  /* SPI device driver */
#include "xuartps.h" /* UART device driver */

/******************* Other defined functions for UART and SPI ****************/
extern int Intialize_UART(u16 DeviceId);
extern int Initialize_SPI_0_and_1(u16 SpiDeviceId_0_master, u16 SpiDeviceId_1_slave);

extern void print_command_menu(void);

extern void SpiMasterWrite(u8 *Sendbuffer, int ByteCount);
extern void SpiSlaveRead(int ByteCount);
extern void SpiSlaveWrite(u8 *Sendbuffer, int ByteCount);
extern u8 SpiMasterRead(int ByteCount);


/***************** Macros (Inline Functions) Definitions *********************/

#define SpiPs_RecvByte(BaseAddress) (u8) XSpiPs_In32((BaseAddress) + XSPIPS_RXD_OFFSET)

#define SpiPs_SendByte(BaseAddress, Data) XSpiPs_Out32((BaseAddress) + XSPIPS_TXD_OFFSET, (Data))

/************************** Variable Definitions *****************************/

XUartPs Uart_PS;        /* Instance of the UART Device */
XUartPs_Config *Config; /* The instance of the UART-PS Config */

static XSpiPs Spi_0_Instance_MASTER;
static XSpiPs Spi_1_Instance_SLAVE;

u8 RxBuffer_Slave[1];
u8 *buffer_Rx_Master;
u8 RxBuffer_Master[1];

/*****************************************************************************/


int Intialize_UART(u16 DeviceId) {
  int Status;
  Config = XUartPs_LookupConfig(DeviceId);
  if (NULL == Config) {
    return XST_FAILURE;
  }

  Status = XUartPs_CfgInitialize(&Uart_PS, Config, Config->BaseAddress);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  XUartPs_SetOperMode(&Uart_PS, XUARTPS_OPER_MODE_NORMAL);

  return XST_SUCCESS;
}

void print_command_menu(void) {
  xil_printf("*****************Welcome to Lab_2 of ECE-315*****************\r\n");
  xil_printf("Select from the Command Menu to perform the desired operation.\r\n");
  xil_printf("Just enter the number of the command and press ENTER key from keyboard\r\n\n");
  xil_printf("\n\t1: Toggle Task1 Loop back Mode\r\n");
  xil_printf("\n\t2: Toggle Task2 Loop back Mode\r\n");
  xil_printf("\n\t3: Change loop_count\r\n");
}

int Initialize_SPI_0_and_1(u16 SpiDeviceId_0_master, u16 SpiDeviceId_1_slave) {
  int Status;
  XSpiPs_Config *SpiConfig_0_master;
  XSpiPs_Config *SpiConfig_1_slave;

  /*
   * Initialize the SPI driver for SPI 0 so that it's ready to use
   */
  SpiConfig_0_master = XSpiPs_LookupConfig(SpiDeviceId_0_master);
  if (NULL == SpiConfig_0_master) {
    return XST_FAILURE;
  }

  Status = XSpiPs_CfgInitialize((&Spi_0_Instance_MASTER), SpiConfig_0_master, SpiConfig_0_master->BaseAddress);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  /*
   * Initialize the SPI driver for SPI 1 so that it's ready to use
   */

  SpiConfig_1_slave = XSpiPs_LookupConfig(SpiDeviceId_1_slave);
  if (NULL == SpiConfig_1_slave) {
    return XST_FAILURE;
  }

  Status = XSpiPs_CfgInitialize((&Spi_1_Instance_SLAVE), SpiConfig_1_slave, SpiConfig_1_slave->BaseAddress);
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }


  Status = XSpiPs_SetOptions((&Spi_0_Instance_MASTER), (XSPIPS_CR_CPHA_MASK) | (XSPIPS_MASTER_OPTION) | (XSPIPS_CR_CPOL_MASK));
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  Status = XSpiPs_SetOptions((&Spi_1_Instance_SLAVE), (XSPIPS_CR_CPHA_MASK) | (XSPIPS_CR_CPOL_MASK));
  if (Status != XST_SUCCESS) {
    return XST_FAILURE;
  }

  return XST_SUCCESS;
}

void SpiMasterWrite(u8 *Sendbuffer, int ByteCount) {
  u32 StatusReg;
  int TransCount = 0;

  StatusReg = XSpiPs_ReadReg(Spi_0_Instance_MASTER.Config.BaseAddress, XSPIPS_SR_OFFSET);

  /*
   * Fill the TXFIFO with as many bytes as it will take (or as
   * many as we have to send).
   */

  while ((ByteCount > 0) && (TransCount < XSPIPS_FIFO_DEPTH)) {
    SpiPs_SendByte(Spi_0_Instance_MASTER.Config.BaseAddress, *Sendbuffer);
    Sendbuffer++;
    ++TransCount;
    ByteCount--;
  }

  /*
   * Wait for the transfer to finish by polling Tx fifo status.
   */
  do {
    StatusReg = XSpiPs_ReadReg(Spi_0_Instance_MASTER.Config.BaseAddress, XSPIPS_SR_OFFSET);
  } while ((StatusReg & XSPIPS_IXR_TXOW_MASK) == 0);
}

void SpiSlaveRead(int ByteCount) {
  int Count;
  u32 StatusReg;

  StatusReg = XSpiPs_ReadReg(Spi_1_Instance_SLAVE.Config.BaseAddress, XSPIPS_SR_OFFSET);
  /*
   * Polling the Rx Buffer for Data
   */
  do {
    StatusReg = XSpiPs_ReadReg(Spi_1_Instance_SLAVE.Config.BaseAddress, XSPIPS_SR_OFFSET);
  } while (!(StatusReg & XSPIPS_IXR_RXNEMPTY_MASK));

  /*
   * Reading the Rx Buffer
   */
  for (Count = 0; Count < ByteCount; Count++) {
    RxBuffer_Slave[Count] = SpiPs_RecvByte(Spi_1_Instance_SLAVE.Config.BaseAddress);
  }
}

void SpiSlaveWrite(u8 *Sendbuffer, int ByteCount) {
  int TransCount = 0;

  u32 StatusReg;

  StatusReg = XSpiPs_ReadReg(Spi_1_Instance_SLAVE.Config.BaseAddress, XSPIPS_SR_OFFSET);

  while ((ByteCount > 0) && (TransCount < XSPIPS_FIFO_DEPTH)) {
    SpiPs_SendByte(Spi_1_Instance_SLAVE.Config.BaseAddress, *Sendbuffer);

    Sendbuffer++;
    ++TransCount;
    ByteCount--;
  }

  /*
   * Wait for the transfer to finish by polling Tx fifo status.
   */
  do {
    StatusReg = XSpiPs_ReadReg(Spi_1_Instance_SLAVE.Config.BaseAddress, XSPIPS_SR_OFFSET);
  } while ((StatusReg & XSPIPS_IXR_TXOW_MASK) == 0);
}

u8 SpiMasterRead(int ByteCount) {
  RxBuffer_Master[0] = SpiPs_RecvByte(Spi_0_Instance_MASTER.Config.BaseAddress);

  return RxBuffer_Master[0];
}

#endif /* SRC_INITIALIZATION_H_ */
