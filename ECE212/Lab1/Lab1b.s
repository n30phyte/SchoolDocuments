/* DO NOT MODIFY THIS --------------------------------------------*/
.text

.global AssemblyProgram

AssemblyProgram:
lea      -40(%a7),%a7 /*Backing up data and address registers */
movem.l %d2-%d7/%a2-%a5,(%a7)
/*----------------------------------------------------------------*/

/******************************************************************/
/* General Information ********************************************/
/* File Name: Lab1b.s *********************************************/
/* Names of Students: Hans Jarales and Michael Kwok              **/
/* Date: 02/03/2020                                              **/
/* General Description:                                          **/
/* Convert ASCII char from uppercase to lowercase and vice versa **/
/******************************************************************/

/*Write your program here******************************************/

/* Create pointer to first memory location */
movea.l #0x43000000, %a2

/* Create pointer to first memory location containing converted letters */
movea.l #0x43200000, %a3

loop:
	move.l (%a2), %d2	/* Initialize data register D2 */

	cmpi.l #0xD, %d2	/* If not ASCII 'Enter' (0xD), go to 'exit' */
	beq stop
						/* Begin check if memory location has ASCII letter */
	cmpi.l #0x41, %d2	/* Compare to ASCII hex code of 'A' */
	bge uppermax		/* If greater than or equal to 0x41, go to 'uppermax' */
	blt error			/* if less than 0x41, go to 'error' */

uppermax:
		cmpi.l #0x5A, %d2	/* Compare content of d2 to ASCII hex code of 'Z' */
		ble convert 		/* If less than or equal to 0x5A, go to 'convert' */
		bgt lowermin 		/* If greater than 0x5A, go to 'lowermin' */

lowermin:
		cmpi.l #0x61, %d2	/* Compare content of d2 to ASCII hex code of 'a' */
		bge lowermax	/* If greater than or equal to 0x61, go to 'lowermax' */
		blt error			/* If less than 0x61, go to 'error' */

lowermax:
		cmpi.l #0x7A, %d2	/* Compare content of d2 to ASCII hex code 'z' */
		ble convert			/* If less than 0x7A, go to 'convert' */
		bgt error			/* If greater than 0x7A, go to 'error' */

convert:
		eor.l #0x20, %d2	/* Toggle 5th bit, convert upper/lowercase */
		move.l %d2, (%a3)	/* move converted letter to new location */
		
		bra exit			/* BRAnch always to exit */

error:  /* Place ASCII error code after obtaining non-ASCII letter */
		move.l #0xFFFFFFFF, (%a3)
		
		bra exit			/* BRAnch always to exit  */

exit: 	/* Increment addresses by 4 to check next memory location(s) */
		adda.l #0x4, %a2
		adda.l #0x4, %a3

		bra loop /* Always loop until ENTER 0x0D is found */

stop: /* End of Program */

/*End of program **************************************************/

/* DO NOT MODIFY THIS --------------------------------------------*/
movem.l (%a7),%d2-%d7/%a2-%a5 /*Restore data and address registers */
lea      40(%a7),%a7
rts
/*----------------------------------------------------------------*/
