/* DO NOT MODIFY THIS --------------------------------------------*/
.text
.global WelcomePrompt
.extern iprintf
.extern cr
.extern value
.extern getstring
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab3a.s **************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: 2020-03-11                                              **/
/* General Description: Takes inputs for data and analyses       **/
/*                                                               **/
/******************************************************************/
WelcomePrompt:
/*Write your program here******************************************/

/* Backup registers */
lea     -40(%sp),         %sp
movem.l %d2-%d7/%a2-%a5, (%sp)



/* Print Welcome message */
Welcomemsg:
    pea     Welcome     /*Push string to stack*/
    jsr     iprintf     /*Display string */
    adda.l  #4, %sp     /*Pop string location from stack*/
    jsr     cr          /*New Line*/


/* Prompt for number of entries */
Entries:
    pea     EntriesPrompt   /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/
    bra     EntryCheck      /*Check criteria*/

InvalidEntry:
    pea     Invalid         /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/

EntryCheck:
    cmpi.l  #3,  %d0
    blt     InvalidEntry
    cmpi.l  #15, %d0
    bgt     InvalidEntry
    cmpi.l  #0,  %d0
    beq     exit
    move.l  %d0, 48(%sp)    /* Move number of entries onto stack */
    move.l  %d0, %d2        /* Create counter from input */




/* Ask for divisor */
Divisormsg:
    pea     DivisorPrompt   /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/
    bra     DivisorCheck

InvalidDivisor:
    pea     Invalid         /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/

DivisorCheck:
    cmpi.l  #2,  %d0
    blt     InvalidDivisor
    cmpi.l  #5,  %d0
    bgt     InvalidDivisor
    move.l  %d0, 44(%sp)     /* Move divisor number onto stack */




movea.l     #0x43000000, %a2 /* Pointer to array storing data */
Numbermsg:
    pea     NumberPrompt    /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/
    bra     NumCheck

InvalidNum:
    pea     Invalid         /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/

NumCheck:
    cmpi.l  #0, %d0     /* Check if negative number entered */
    blt     InvalidNum  /*Invalid if negative*/
    move.l  %d0, (%a2)+   /*If positive, move to array, increment pointer*/
    sub.l   #1, %d2     /*Decrement counter*/
    cmpi.l  #1, %d2     
    beq     LastNummsg  /*If last number, go to LastNummsg*/
    bra     Numbermsg   /*Otherwise, check next number*/

LastNummsg:
    pea     LastNumPrompt   /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/
    bra     LastNumCheck

InvalidLast:
    pea     Invalid         /*Push string to stack*/
    jsr     iprintf         /*Display string */
    adda.l  #4, %sp         /*Pop string location from stack*/
    jsr     cr              /*New Line*/
    jsr     getstring       /*Get user input*/

LastNumCheck:
    cmpi.l #0, %d0
    blt    InvalidLast
    move.l %d0, (%a2)

exit:
/* Restore registers */
movem.l %d2-%d7/%a2-%a5, (%sp)
lea     40(%sp),          %sp

rts

/*End of Subroutine **************************************************/ 
.data
/*All Strings placed here **************************************************/

Welcome:
.string "Welcome to Wing's Stats Program"

EntriesPrompt:
.string "Please enter the number (3min-15max) of entries followed by 'enter'"

DivisorPrompt:
.string "Please enter the divisor (2min-5max) followed by 'enter'"

NumberPrompt:
.string "Please enter a number (positive only)"

LastNumPrompt:
.string "Please enter the last number (positive only)"

Invalid:
.string "Invalid entry, please enter a proper value."

/*End of Strings **************************************************/

