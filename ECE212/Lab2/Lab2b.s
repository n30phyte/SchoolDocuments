/* DO NOT MODIFY THIS --------------------------------------------*/
.text

.global AssemblyProgram

AssemblyProgram:
lea      -40(%a7),%a7 /*Backing up data and address registers */
movem.l %d2-%d7/%a2-%a5,(%a7)
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab2.s **********************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: 02/26/2020                                              **/
/* General Description: Calculate area under curve               **/
/*                                                               **/
/******************************************************************/

/*Write your program here******************************************/

move.l  0x43000000, %d2   /* Create Counter                */
movea.l 0x43000004, %a2   /* Pointer to X points           */
movea.l 0x43000008, %a3   /* Pointer to Y points           */
movea.l 0x4300000C, %a4   /* Pointer to storage            */
movea.l 0x43000010, %a5   /* Pointer to final sum location */
clr.l   %d7               /* Initialize d7                 */

sub.l   #1,         %d2   /* off by one */

loop:

      move.l (%a2)+, %d3   /* d3 holds initial X points */
      move.l (%a3)+, %d4   /* d4 holds initial Y points */
        
      move.l (%a2),  %d5   /* d5 holds X endpoint       */
      move.l (%a3),  %d6   /* d6 holds Y endpoint       */
        
      add.l  %d4,    %d6   /* d6 now holds sum of Y points 'ysum'  */
      sub.l  %d3,    %d5   /* d5 now holds width of area 'dx'      */
        
      asr.l  #1,     %d5   /* divide by 2 to reveal amount to multiply ysum */
      asl.l  %d5,    %d6   /* multiply ysum by dx                           */
      
      add.l  %d6,    %d7   /* move area result into d7                      */
      
        
      sub.l #1,      %d2   /* Loops until there are no more data points     */
      bne loop

asr.l  #1,     	     %d7   /* Divide by 2                                   */
move.l %d7,          (%a5) /* move final sum into final sum location        */


/*End of program **************************************************/

/* DO NOT MODIFY THIS --------------------------------------------*/
movem.l (%a7),%d2-%d7/%a2-%a5 /*Restore data and address registers */
lea      40(%a7),%a7
rts
/*----------------------------------------------------------------*/
