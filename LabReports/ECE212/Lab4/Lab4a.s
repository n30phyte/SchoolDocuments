/* DO NOT MODIFY THIS --------------------------------------------*/
.text
.global WelcomePrompt
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
/* File Name: Lab4a.s **************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: Apr. 8, 2020                                            **/
/* General Description:                                          **/
/*                                                               **/
/******************************************************************/
WelcomePrompt:
/*Write your program here******************************************/

/* Backup registers */
lea     -40(%sp),         %sp
movem.l %d2-%d5/%a2-%a5, (%sp)

/* Display Welcome message */
pea     Welcome
jsr     iprintf
adda.l  #4, %sp
jsr     cr

/* Prompt for string */
pea     AskForString
jsr     iprintf
adda.l  #4, %sp
jsr     cr
bra GetCharacter

Invalidchr:
    pea     InvalidEntry
    jsr     iprintf
    adda.l  #4, %sp
    jsr     cr
GetCharacter:
    jsr getchr
    
    /*Check if number*/
    cmpi.l  #48, %d0
    blt     Invalidchr
    cmpi.l  #57, %d0
    bgt     CheckLetter

CheckLetter:
    cmpi.l  #65,  %d0 
    blt     Invalidchr
    cmpi.l  #90,  %d0
    bgt     Invalidchr

/* Move ASCII Keystroke to designated location */
move.l  %d0, 44(%sp)


/* Restore registers */
movem.l (%sp),      %d2-%d5/%a2-%a5
lea     40(%sp),    %sp

rts 
/*End of Subroutine **************************************************/ 
.data
/*All Strings placed here **************************************************/

Welcome:
.string "Welcome to Wing's LED Display"

AskForString:
.string "Please enter an UpperCase letter or Number from the keyboard"

InvalidEntry
.string "Invalid entry, please enter proper keystroke from keyboard"

/*End of Strings **************************************************/
/******************************************************/
