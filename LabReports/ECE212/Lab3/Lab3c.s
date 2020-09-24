/* DO NOT MODIFY THIS --------------------------------------------*/
.text
.global Display
.extern iprintf
.extern cr
.extern value
.extern getstring
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab3c.s *********************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: 2020-03-11                                              **/
/* General Description: Print out the statistics calculated in   **/
/*                      other subroutines                        **/
/******************************************************************/
Display:
/*Write your program here******************************************/
lea     -40(%sp),         %sp
movem.l %d2-%d7/%a2-%a5, (%sp)

pea NumEntriesText              /* Push location of string to stack */
jsr iprintf                     /* Print out string */
adda.l #4, %sp                  /* Clean up stack */

move.l 48(%sp), %d2             /* Load number */
move.l %d2, -(%sp)              /* Store to stack */
jsr value                       /* Print number */
adda.l #4, %sp                  /* Clean up stack */
jsr cr                          /* Print newline */

clr.l %d3                       /* Prepare counter */
NumberPrintLoop:                /* Counter print loop *?
    move.l (%a2, %d3*4), -(%sp) /* Load number to stack, using counter as index */
    jsr value                   /* Print number */
    jsr cr                      /* Print newline */
    adda.l #4, %sp              /* Remove number from stack */
    add.l #1, %d3               /* Increment counter */
    cmp.l %d2, %d3              /* Check if done */
    blt NumberPrintLoop         /* repeat if not */

pea MinNumberText               /* Push string to stack */
jsr iprintf                     /* Print out string */
adda.l #4, %sp                  /* pop stack */

move.l (%a3)+, -(%sp)           /* Load smallest number to stack */ 
jsr value                       /* Print out number to stack */
adda.l #4, %sp                  /* Pop stack */
jsr cr                          /* Print newline */

pea MaxNumberText               /* Push string to stack */
jsr iprintf                     /* Print out string */
adda.l #4, %sp                  /* pop stack */

move.l (%a3)+, -(%sp)           /* Load largest number to stack */ 
jsr value                       /* Print out number to stack */
adda.l #4, %sp                  /* Pop stack */
jsr cr                          /* Print newline */

pea MeanNumberText              /* Push string to stack */
jsr iprintf                     /* Print out string */
adda.l #4, %sp                  /* pop stack */

move.l (%a3), -(%sp)            /* Load average number to stack */ 
jsr value                       /* Print out number to stack */
adda.l #4, %sp                  /* Pop stack */
jsr cr                          /* Print newline */

pea DivisbleCountText1          /* Push string to stack */
jsr iprintf                     /* Print out string */
adda.l #4, %sp                  /* pop stack */

move.l 52(%sp), %d2             /* Load number to register */
move.l %d2, -(%sp)              /* Push number to stack */
jsr value                       /* Print out number to stack */
adda.l #4, %sp                  /* pop stack */

pea DivisbleCountText2          /* Push string to stack */
jsr iprintf                     /* Print out string */
adda.l #4, %sp                  /* pop stack */

move.l 44(%sp), %d2             /* Load number to register */
move.l %d2, -(%sp)              /* Push number to stack */
jsr value                       /* Print out number to stack */
adda.l #4, %sp                  /* pop stack */

jsr cr                          /* Print newline */

pea EndText                     /* Push end text to stack */
jsr iprintf                     /* Print text */
adda.l #4, %sp                  /* Pop stack */

jsr cr                          /* Print newline */

movem.l (%sp), %d2-%d7/%a2-%a5  /* Restore registers */
lea     40(%sp),         %sp    /* Reset stack pointer */

rts 

/*End of Subroutine **************************************************/ 
.data
/*All Strings placed here **************************************************/
NumEntriesText:
.string "The number of entries was "

MinNumberText:
.string "Min Number = "

MaxNumberText:
.string "Max Number = "

MeanNumberText:
.string "Mean Number = "

DivisbleCountText1:
.string "There are "

DivisbleCountText2:
.string " number(s) divisble by "

EndText:
.string "Program ended"

/*End of Strings **************************************************/
