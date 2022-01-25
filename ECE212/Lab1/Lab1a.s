/* DO NOT MODIFY THIS --------------------------------------------*/
.text

.global AssemblyProgram

AssemblyProgram:
lea      -40(%a7),%a7 /*Backing up data and address registers */
movem.l %d2-%d7/%a2-%a5,(%a7)
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information                                           **/
/* File Name: Lab1a.s                                            **/
/* Names of Students: Michael Kwok and Hans Jarales              **/
/* Date: 2020-02-05                                              **/
/* General Description:                                          **/
/* Converts alphanumeric characters into Hex                     **/
/******************************************************************/

movea.l	#0x43000000, %a2	/* Address to read from */
movea.l	#0x43100000, %a3	/* Address to store results at */

MainLoop:
move.l	(%a2), %d2			/* Load entry in the current address */
cmpi.l #0x0D, %d2			/* Check if its the enter ASCII */
beq Stop					/* If it is, Stop the program */
cmpi.l #0x30, %d2			/* If it's less than ASCII for 0, go to error */
blt Error
cmpi.l #0x3A, %d2			/* Check if a number */
blt Number					/* If not a number */
or.l #0x20, %d2				/* Set a bit to always be lower case */
cmpi.l #0x67, %d2			/* If larger than z, it's invalid */
bge Error
cmpi.l #0x61, %d2			/* If less than a, invalid */
blt Error
and.l #15, %d2				/* Hex conversion function */
add.l #0x9, %d2
move.l %d2, (%a3)
bra Exit					/* Post loop condition */

Exit:
add.l #0x4, %a2				/* increment address registers */
add.l #0x4, %a3
bra MainLoop

Error:
move.l #0xFFFFFFFF, (%a3)	/* Error code */
bra Exit

Number:
and.l #15, %d2				/* Number conversion function */
move.l %d2, (%a3)			/* Save into memory */
bra Exit

Stop:
/* DO NOT MODIFY THIS --------------------------------------------*/
movem.l (%a7),%d2-%d7/%a2-%a5 /*Restore data and address registers */
lea      40(%a7),%a7 
rts
/*----------------------------------------------------------------*/
