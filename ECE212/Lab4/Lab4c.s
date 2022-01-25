/* DO NOT MODIFY THIS --------------------------------------------*/
.text
.global LedSub
.extern cr
.extern iprintf
.extern getchr
.extern TurnOnLed
.extern TurnOffLed
.extern Row
.extern Column
.extern Delay
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab4c.s *********************************************/
/* Names of Students: Michael Kwok and Hans Jarales              **/
/* Date: 06-04-2020                                              **/
/* General Description:                                          **/
/* Subroutine to output numbers and letters to the LED           **/
/******************************************************************/
LedSub:
/*Write your program here******************************************/
lea      -40(%a7),%a7 
movem.l %d2-%d7/%a2-%a5,(%a7)

/* d2 for Row, d3 for column*/

movea.l 44(%a7), %a2    /* Load address of pattern */

clr.l %d2
clr.l %d3

RowLoop:
cmp.l #8, %d2            /* Check if loop is done */
bge End
move.b (%a2, %d2*1), %d4 /* Load row */
ColumnLoop:

move.l %d3, -(%sp)       /* Load column number in to stack */
jsr Column               /* Call column subroutine */
lea 4(%sp), (%sp)        /* Clean up stack */

move.l %d2, -(%sp)       /* Load row number in to stack */
jsr Row                  /* Call row subroutine */
lea 4(%sp), (%sp)        /* Clean up stack */

move.l %d3, %d5          /* Make a copy */
add.l #-7, %d5           /* Invert index for LED array */

btst.b %d5, %d4          /* Check if 0 */
beq TurnOff
jsr TurnOnLed            /* Turn on if 1 */
bra PostLoop

TurnOff:
jsr TurnOffLed           /* Turn off if 0 */
bra PostLoop

PostLoop:
add.l #1, %d3   /* Increment column */
cmp.l #8, %d3   /* Check if end of column */
blt ColumnLoop  /* If not, go back to column loop */
clr.l %d3       /* if yes, reset column */
add.l #1, %d2   /* Increment row */
bra RowLoop     /* return to row loop */

End:
move.l #300, -(%sp)      /* Load row number in to stack */
jsr Delay                /* Call delay subroutine */
lea 4(%sp), (%sp)        /* Clean up stack */

movem.l (%a7), %d2-%d7/%a2-%a5
lea      40(%a7),%a7 


rts 
/*End of Subroutine **************************************************/ 
.data
/*All Strings placed here **************************************************/



/*End of Strings **************************************************/
/******************************************************/
