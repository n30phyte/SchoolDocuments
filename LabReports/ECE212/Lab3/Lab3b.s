/* DO NOT MODIFY THIS --------------------------------------------*/
.text
.global Stats
.extern iprintf
.extern cr
.extern value
.extern getstring
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab3b.s *********************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: 2020-04-10                                              **/
/* General Description:                                          **/
/* Finds the mean, maximum/minimum, and the divisible numbers    **/
/* from given user input.                                         **/
/******************************************************************/
Stats:
/*Write your program here******************************************/

/* Backup registers */
lea     -40(%sp),         %sp
movem.l %d2-%d7/%a2-%a5, (%sp)

/* A2 = Base address */
/* D4 = Index */

FindMin:
    move.l (%a2)+,      %d2
    move.l 48(%sp),     %d4     /*Initialize counter*/

MinLoop:
    sub.l  #1,     %d4    /* Decrement counter */
    move.l (%a2)+, %d3    /* Load Next number to test */
    cmp.l  #0,     %d4    /* Check if counter done */
    ble    FindMax        /* If counter done, go to FindMax */
    cmp.l  %d3,    %d2    /* Compare stored and new number */
    blt    MinLoop        /* If stored is smaller, repeat checks */
    move.l %d3,    %d2    /* If stored is larger, save new number */
	bra    MinLoop        /* Repeat loop */

FindMax:
    move.l %d2,          (%a3)+    /* Save min number */
    movea.l #0x43000000, %a2       /* Reinitialize counter to data array */
    move.l  (%a2)+,      %d2       /* Load First number */
    move.l  48(%sp),     %d4       /* Reload Counter */

MaxLoop:
    sub.l  #1,     %d4    /* Decrement counter */
    move.l (%a2)+, %d3    /* Load Next number to test */
    cmp.l  #0,     %d4    /* Check if counter done */
    ble    FindMean       /* If counter done, go to FindMax */
    cmp.l  %d3,    %d2    /* Compare stored and new number */
    bgt    MaxLoop        /* If stored is larger, repeat checks */
    move.l %d3,    %d2    /* If stored is smaller, save new number */
	bra    MaxLoop        /* Repeat loop */

FindMean:
    move.l  %d2,    (%a3)+      /* Save max number */
    movea.l #0x43000000, %a2    /* Reinitialize counter to data array */
    move.l  48(%sp),     %d3    /* Reload Counter */
    clr.l   %d2                 /* Clear D5 to use as cum sum */

MeanLoop:
    add.l   (%a2)+,      %d2
    sub.l   #1,          %d3
    bne     MeanLoop
    divu.l  48(%sp),     %d2      /*Divide total sum by amount of integers*/
    move.l  %d2,         (%a3)+   /*Move mean to output array*/

move.l  48(%sp),        %d3     /*Reload Counter*/
movea.l #0x43000000,    %a2     /*Reinitialize counter to data array*/
move.l  #4,             %d5
clr.l   %d6                     /*Divisor counter*/

FindDivisible:
    move.l  (%a2)+,     %d2
    move.l  %d2,        %d4
    move.l  44(%sp),    %d7
    divu.w  %d7,        %d4     /*lower word contains quotient - higher word has remainder*/

remaindercheck:
    and.l 	#0xFFFF0000,	%d4
    cmp.l	#0,			    %d4
    bne		NotDivisible
    move.l  %d2,		    (%a3)+
    add.l 	#1,			    %d6		/*Increment divisor counter*/
    sub.l 	#1, 		    %d3
    bne 	FindDivisible
   	bra 	exit

NotDivisible:
    sub.l   #1,         %d3
    bne     FindDivisible

exit:
	move.l  %d6,        52(%sp)



/* Restore registers */
movem.l (%sp), %d2-%d7/%a2-%a5
lea     40(%sp),          %sp
rts 

/*End of Subroutine **************************************************/ 
.data
/*All Strings placed here **************************************************/



/*End of Strings **************************************************/
