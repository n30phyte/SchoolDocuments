/* DO NOT MODIFY THIS --------------------------------------------*/
.text
.global convert1
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
/* File Name: Lab4b.s **************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: Apr. 8, 2020                                            **/
/* General Description:                                          **/
/*                                                               **/
/******************************************************************/
convert:
/*Write your program here******************************************/

/* Backup registers */
lea     -40(%sp),         %sp
movem.l %d2-%d5/%a2-%a5, (%sp)

move.l  44(%sp), -(%sp)
jsr     convert1
move.l  (%sp)+, 44(%sp)

/* Restore registers */
movem.l (%sp),      %d2-%d5/%a2-%a5
lea     40(%sp),    %sp

rts 
/*End of Subroutine **************************************************/ 
.data
/*All Strings placed here **************************************************/


/*End of Strings **************************************************/
