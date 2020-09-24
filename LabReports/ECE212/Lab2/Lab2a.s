/* DO NOT MODIFY THIS --------------------------------------------*/
.text

.global AssemblyProgram

AssemblyProgram:
lea      -40(%a7),%a7 /*Backing up data and address registers */
movem.l %d2-%d7/%a2-%a5,(%a7)
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab2a.s *********************************************/
/* Names of Students: Michael Kwok and Hans Jarales              **/
/* Date: 2020-02-26                                              **/
/* General Description:                                          **/
/*                                                               **/
/******************************************************************/

MOVE.L  0x43000000, %D2 /* Load size of array */ 

MOVEA.L 0x43000004, %A2 /* First array */
MOVEA.L 0x43000008, %A3 /* Second array */
MOVEA.L 0x4300000C, %A4 /* Register Indirect With Offset */
MOVEA.L 0x43000010, %A5 /* Indexed Register Indirect */

/*Part A **********************************************************/
PartA:
MOVE.L (%A2), %D3    /* Load first number using register indirect */
ADD.L (%A3), %D3     /* Add first number using register indirect  */
MOVE.L %D3, (%A4)    /* Save first number using register indirect */

MOVE.L 4(%A2), %D3   /* Load second number using register indirect*/
ADD.L 4(%A3), %D3    /* Add second number using register indirect */
MOVE.L %D3, 4(%A4)   /* Save second number using register indirect*/

MOVE.L 8(%A2), %D3   /* Load third number using register indirect */
ADD.L 8(%A3), %D3    /* Add third number using register indirect  */
MOVE.L %D3, 8(%A4)   /* Save third number using register indirect */

/*Part B **********************************************************/
CLR.L %D7            /* Prepare to use D7 as iteration counter*/

PartB:
CMP.L %D2, %D7           /* Check if done with loop */
BGE PartC

MOVE.L (%A2, %D7*4), %D3 /* Load number with D7 as index and 4 as offset */
ADD.L (%A3, %D7*4), %D3  /* Add number from second array to D3 */
MOVE.L %D3, (%A5, %D7*4) /* Save number into memory */

ADD.L #1, %D7            /* Increment counter */
BRA PartB

/*Part C **********************************************************/
PartC:
MOVEA.L 0x43000014, %A5 /* Postincrement Register */
CLR.L %D7               /* Reset D7 */

PartCLoop:
CMP.L %D2, %D7          /* Check loop condition */
BGE Exit

MOVE.L (%A2)+, %D3      /* Load number and increment address register */
ADD.L (%A3)+, %D3       /* Add from second array and increment register */
MOVE.L %D3, (%A5)+      /* Save into memory */

ADD.L #1, %D7           /* Increment counter */
BRA PartCLoop

/*End of program **************************************************/
Exit:

/* DO NOT MODIFY THIS --------------------------------------------*/
movem.l (%a7),%d2-%d7/%a2-%a5 /*Restore data and address registers */
lea      40(%a7),%a7 
rts
/*----------------------------------------------------------------*/
