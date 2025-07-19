;----------------------------------------------------------------------
; PRN2FILE.ASM - A resident program which redirects printer output.
; SYNTAX: PRN2FILE d:path:filename.ext [/Pn] [/Bn] [/U]
;  1)  Run PRN2FILE with the desired filename to activate it.
;  2)  Run it again with no filename to turn off redirection.
;  3)  Run it with a differant filename to change destination file.
;  4)  Use /P to designate the printer number (defaults to 1)
;  5)  Use /B to enter buffer size in K bytes (defaults to 4)
;  6)  Use /U to uninstall the program
;----------------------------------------------------------------------
CSEG		SEGMENT
		ASSUME	CS:CSEG,DS:NOTHING

		ORG	100H	;Beginning for .COM programs
START:		JMP INITIALIZE	;Initialization code is at end

;----------------------------------------------------------------------
; Data area used by this program
;----------------------------------------------------------------------
COPYRIGHT	DB	"PRN2FILE 1.0 (c) 1987 Ziff Communications Co.$",1AH
PROGRAMMER	DB	"Tom Kihlken"
REDIRECT_MESS	DB	"LPT"
PRN_NUM		DB	"1 Redirected to: $"
BAD_FILENAME	DB	"Invalid filename.$"
BAD_PARAM	DB	"Usage: PRN2FILE [path][filename][/Pn][/Bnn][/U]$"
BAD_ALLOC	DB	"Memory Allocation Error.$"
BAD_UNINSTALL	DB	"Cannot Uninstall.$"
PRN_TXT		DB	"PRN$"
CRLF		DB	13,10,"$"
ERR_MESSAGE	DB	13,10,"*Buffer Overflow*",13,10
MESS_LENGTH	EQU	$ - OFFSET ERR_MESSAGE
OLDINT08	DD	?	;Old timer tick interrupt vector
OLDINT17	DD	?	;Old printer output vector
OLDINT21	DD	?	;Old dos function interrupt vector
OLDINT28	DD	?	;Old dos waiting interrupt vector
DOS_FLAG	DD	?	;Dos busy flag
SWITCH		DB	0	;On/off switch for redirecting printer
TIMEOUT		DW	0	;Holds timeout counter to flush buffer
INSTALLED_SEG	DW	0	;Segment location of installed copy
WRITE_FLAG	DB	0	;Indicates buffer should be written
PRINTER_NUM	DW	0	;Default to first parallel printer
BUFF_POINTER	DW  	0	;Pointer to next space in buffer
BUFF_SIZE	DW	4	;Size of buffer
BUFF_SEGMENT	DW	0	;Segment address of buffer
TIME_TO_WRITE	EQU	400H	;Flush buffer when this full

;-----------------------------------------------------------------------
; Interrupt 17 routine. (BIOS printer output)
; If output is to the selected printer and switch is on then redirect
; the character into a file.
;-----------------------------------------------------------------------
NEWINT17	PROC	FAR
		ASSUME	DS:NOTHING, ES:NOTHING

		CMP	DX,CS:PRINTER_NUM ;Is this the selected printer?
		JNE	IGNORE		;If not, let bios handle it
		CMP	CS:SWITCH,1	;Is redirection turned on?
		JE	REDIRECT_IT	;If on, take jump
IGNORE:
		JMP	CS:OLDINT17	;Jump to the bios routine
REDIRECT_IT:
		STI			;Get interrupts back on
		MOV	CS:TIMEOUT,91	;Reset timeout counter
		PUSH	SI		;Si will be used for a pointer
		CMP	AH,1		;Initializing the printer?
		JE	WRITE_BUFF	;If yes, then flush the buffer
		OR	AH,AH		;Printing a character?
		JNZ	PRINT_RET	;If not, take jump to return
		MOV	SI,CS:BUFF_POINTER	;Get pointer to the buffer
		CMP	SI,CS:BUFF_SIZE	;Is buffer filled up yet?
		JE	PRINT_RET	;If full just return.

		PUSH	DS		;Save the data segment
		MOV	DS,CS:BUFF_SEGMENT	;Load DS with the buffer seg
		MOV	DS:[SI],AL	;Store the character in buffer
		POP	DS		;Restore data segment
		INC	SI		;And point to next position
		MOV	CS:BUFF_POINTER,SI	;Save the new pointer

		CMP	SI,TIME_TO_WRITE ;Is buffer filling up yet?
		JL	PRINT_RET	 ;If not, just return
WRITE_BUFF:
		MOV	CS:WRITE_FLAG,1	;Signal buffer needs emptying
		PUSH	DS
 		PUSH	BX
		LDS	BX,CS:DOS_FLAG	;Get location of dos flag
		CMP	BYTE PTR [BX],0	;Is dos busy flag set?
		POP	BX
		POP	DS
		JNE	PRINT_RET	;If busy, do nothing
	 	CALL	WRITE_TO_FILE	;This empties the buffer
PRINT_RET:
		POP	SI
		MOV	AH,10010000B	;Return printer status good
		IRET			;Return from interrupt
NEWINT17	ENDP

;----------------------------------------------------------------------
; New interrupt 08h (timer tick) decrement the timeout counter. Set
; the flush flag when counter reaches zero.
;----------------------------------------------------------------------
NEWINT08	PROC	FAR
		ASSUME	DS:NOTHING, ES:NOTHING

		PUSHF			;Simulate an interrupt
		CALL	CS:OLDINT08	;Do normal timer routine
		DEC	CS:TIMEOUT	;Count down the flush time count
		JNZ	STILL_TIME	;Count until it gets to zero
		CMP	CS:BUFF_POINTER,0 ;Anything in buffer?
		JE	STILL_TIME	;If not, just continue
		MOV	CS:WRITE_FLAG,1	;Set flush trigger
STILL_TIME:
		IRET			;Return from timer interrupt

NEWINT08	ENDP

;----------------------------------------------------------------------
; Interrupt 21 routine.  (DOS function calls) intercept function 40h
; when it writes to the printer.  Also check to see if WRITE_FLAG is
; set to one.  If it is then flush the buffer.
;----------------------------------------------------------------------
NEWINT21	PROC	FAR
		ASSUME	DS:NOTHING, ES:NOTHING

		PUSHF			;Save the callers flags
		CMP	CS:WRITE_FLAG,1	;Buffer need to be written?
		JNE	DONT_WRITE	;If not, then just return
		PUSH	DS
 		PUSH	BX
		LDS	BX,CS:DOS_FLAG	;Get location of DOS flag
		CMP	BYTE PTR [BX],0	;Is DOS busy flag set?
		POP	BX
		POP	DS
		JNE	DONT_WRITE	;If busy, do nothing
		CALL	WRITE_TO_FILE	;Empty the buffer now
DONT_WRITE:
		OR	AH,AH		;Doing function zero?
		JNE	NOT_ZERO
		MOV	AX,4C00H	;If yes, change it to 4Ch
NOT_ZERO:
		CMP	AH,40H		;Writing to a device?
		JNE	NOT_PRINTER	;If not, just continue
		CMP	BX,4		;Writing to the printer handle?
		JNE	NOT_PRINTER	;If not, just continue
		CMP	CS:SWITCH,1	;Is redirection on?
		JE	PRINT_IT	;If yes, then redirect it
NOT_PRINTER:
		POPF			;Recover flags from stack
		CLI
		JMP	CS:OLDINT21	;Do the DOS function

; Emulate print string function by involking INT 17h

PRINT_IT:
		STI			;Reenable interrupts
		CLD			;String moves forward

		PUSH	CX		;Save these registers
		PUSH	DX
		PUSH	SI

		MOV	SI,DX		;Get pointer to string
		MOV	DX,PRINTER_NUM	;Selected printer ID in DX
		JCXZ	END_LOOP	;Skip loop if count is zero
PRINT_LOOP:
		LODSB			;Load next character from string
		MOV	AH,00		;Print character function
		INT	17H		;BIOS print
		LOOP	PRINT_LOOP	;Loop through whole string
END_LOOP:
		POP	SI
		POP	DX
		POP	CX

		MOV	AX,CX		;All bytes were output
		POPF			;Restore the callers flags

		CLC			;Return success status
		STI			;Reenable interrupts
		RET	2		;Return with current flags

NEWINT21	ENDP

;----------------------------------------------------------------------
; This copies the buffer contents to a file. It should only be called
; when dos is in a reentrant condition.  All registers are preserved
;----------------------------------------------------------------------
WRITE_TO_FILE	PROC	NEAR
		ASSUME	DS:NOTHING, ES:NOTHING

		PUSH	AX		;Save registers we need to use
		PUSH	BX
		PUSH	CX
		PUSH	DX
		PUSH	DS
		PUSH	ES

		PUSH	CS
		POP	DS		;Set DS to code segment
		ASSUME	DS:CSEG		;Tell assembler DS is CSEG
		MOV	WRITE_FLAG,0	;Clear write request flag
	     	MOV	AX,3524H	;Get dos critical error vector
		CALL	DOS_FUNCTION	;Do the dos function
		PUSH	BX		;Save old vector on stack
		PUSH	ES

; Replace the dos severe error interrupt with our own routine.

		MOV	DX,OFFSET NEWINT24
		MOV	AX,2524H	;Setup to change int 24h vector
		CALL	DOS_FUNCTION	;Do the dos function

; First try to open the file.  If dos returns with the carry flag set,
; the file didn't exist and we must create it.  Once the file is opened,
; advance the file pointer to the end of file to append.

		CMP	BUFF_POINTER,0	;Anything in the buffer?
		JE	REP_VECTOR	;If not, no nothing
		MOV	DX,OFFSET FILENAME ;Point to filename
		MOV	AX,3D02H	;Dos function to open file
		CALL	DOS_FUNCTION	;Do the dos function
		JC	FILE_NOT_FOUND	;Set if file doesn't exist.
		MOV	BX,AX		;Keep handle in BX also
		XOR	CX,CX		;Move dos file pointer to the
		XOR	DX,DX		;End of the file. this lets us
		MOV	AX,4202H	;Append this to an existing file
		CALL	DOS_FUNCTION	;Do the dos function
		JC	CLOSE_FILE   	;On any error, take jump
		JMP	SHORT WRITE_FILE
FILE_NOT_FOUND:
		CMP	AX,2		;Was it file not found error?
		JNE	REP_VECTOR	;If not, just quit
		MOV	CX,0020H	;Attribute for new file
		MOV	AH,3CH		;Create file for writing
		CALL	DOS_FUNCTION	;Do the dos function
		JC	CLOSE_FILE  	;On any error, take jump

		MOV	BX,AX		;Save handle in BX also
WRITE_FILE:     MOV	DX,0		;Point to buffer
		MOV	CX,BUFF_POINTER	;Number of chars in buffer
		MOV	AH,40H		;Dos write to a device function
		PUSH	DS
		MOV	DS,BUFF_SEGMENT	;Point to buffer segment
		CALL	DOS_FUNCTION	;Do the dos function
		POP	DS
		JC	CLOSE_FILE  	;On any error, take jump
		CMP	CX,AX		;Was everything written
		JNE	CLOSE_FILE	;If not, it was an error
		CMP	CX,BUFF_SIZE	;Was buffer full?
		JNE	CLOSE_FILE	;If not everything is OK

		MOV	DX,OFFSET ERR_MESSAGE ;Insert the error message
		MOV	CX,MESS_LENGTH
		MOV	AH,40H		;Dos write to file function
		CALL	DOS_FUNCTION	;Do the dos function
CLOSE_FILE:
		MOV	AH,3EH		;Dos function to close the file
		CALL	DOS_FUNCTION	;Do the dos function
REP_VECTOR:
		MOV     BUFF_POINTER,0	;Indicate buffer is empty
		POP	DS		;Recover int 24h vector from stack
		POP	DX
		MOV	AX,2524H	;Restore critical error vector
		CALL	DOS_FUNCTION	;Do the dos function
		ASSUME	DS:NOTHING
		POP	ES		;Restore all registers
		POP 	DS
		POP	DX
		POP	CX
		POP	BX
		POP	AX
		RET			;Finished with writing to disk

WRITE_TO_FILE	ENDP

;----------------------------------------------------------------------
; This routine emulates an INT 21 by calling the dos interrupt address
;----------------------------------------------------------------------
DOS_FUNCTION	PROC	NEAR
		ASSUME	DS:NOTHING, ES:NOTHING

		PUSHF			;Save the processor flags
		CLI			;Clear interrupt enable bit
		CALL	CS:OLDINT21	;Execute the interupt procedure
		STI			;Enable further interrupts
		RET			;And return to calling routine

DOS_FUNCTION	ENDP

;----------------------------------------------------------------------
; New interrupt 24h (critical dos error).  This interrupt is only in
; effect when writing to the disk.  It is required to suppress the
; 'Abort, Retry, Ignore' message.  All fatal disk errors are ignored.
;----------------------------------------------------------------------
NEWINT24	PROC	FAR
		ASSUME	DS:NOTHING, ES:NOTHING

		STI			;Turn interrupts back on
 		XOR	AL,AL		;Tells dos to ignore the error
		MOV	CS:SWITCH,AL	;Turn off logging of output
		IRET			;And return to dos

NEWINT24	ENDP

;----------------------------------------------------------------------
; New interrupt 28h (DOS idle).  Check to see if write_flag is set to
; one. If it is, then flush the buffer
;----------------------------------------------------------------------
NEWINT28	PROC	FAR
		ASSUME	DS:NOTHING, ES:NOTHING

		STI
		CMP	CS:WRITE_FLAG,0	;Buffer need to be written?
		JE	DO_NOTHING	;If not, just continue
		CALL	WRITE_TO_FILE	;Empty the buffer
DO_NOTHING:
		JMP	CS:OLDINT28	;Continue with old interrupt

NEWINT28	ENDP

;----------------------------------------------------------------------
; Here is the code used to initialize prn2file.com.  First determine
; if prn2file is already installed.  If it is, just copy new parameters
; into the resident programs data area, otherwise save old vectors
; and replace with new ones.  The output buffer will later overlay
; this code to conserve memory.
;----------------------------------------------------------------------
		ASSUME	CS:CSEG, DS:CSEG, ES:NOTHING
INITIALIZE:
		MOV	DX,OFFSET COPYRIGHT
		CALL	STRING_CRLF	;Display the string

; Search for a previously installed copy of prn2file

		NOT	WORD PTR START	;Modify to avoid false match
		XOR	BX,BX		;Start search at segment zero
		MOV	AX,CS		;Compare to this code segment
NEXT_SEGMENT:
		INC	BX		;Look at next segment
		CMP	AX,BX		;Until reaching this code seg
		MOV	ES,BX
		JE	NOT_INSTALLED
		MOV	SI,OFFSET START	;Setup to compare strings
		MOV	DI,SI
		MOV	CX,16		;16 bytes must match
		REP	CMPSB		;Compare DS:SI to ES:DI
		OR	CX,CX
		JNZ	NEXT_SEGMENT	;If no match, try next segment
		MOV	ES:SWITCH,1	;Turn redirection on
		MOV	DX,ES:PRINTER_NUM ;Retrieve old printer number
		MOV	DS:PRINTER_NUM,DX ;Save it here
		MOV	AH,1		;Initialize the resident copy
		INT	17H		;To flush it's buffer
		ADD	DL,31H		;Convert printer num to ascii
		MOV	PRN_NUM,DL	;Put it into the message area
NOT_INSTALLED:
		MOV	INSTALLED_SEG,ES
		PUSH	CS
		POP	ES		;Set ES to this segment
		ASSUME	ES:CSEG
		CMP	BYTE PTR DS:[0080],0 ;Anything entered?
		JE	NO_PARAMS	;If not, take jump
PARSE:
		MOV	AL,"/"		;Look for a slash
		CALL	LOAD_PARAMS
		REPNE	SCASB		;Scan for slashes
		JCXZ	PARSE_DONE	;Quit when no more slashes
		MOV	AL,[DI]		;Get the parameter
		MOV WORD PTR [DI-1],2020H;Erase the slash and letter
		OR	AL,32		;Convert to lower case
		CMP	AL,"p"		;Is it the "p" parameter
		JE	SLASH_P
		CMP	AL,"b"		;Is it the "b" parameter
		JE	SLASH_B
		CMP	AL,"u"		;Is it the "u" parameter
		JE	SLASH_U
INVALID_PARAM:
		MOV	DX,OFFSET BAD_PARAM ;Point to error message
		JMP	ERR_EXIT
SLASH_U:
		JMP	UNINSTALL	;Slash "u" means uninstall it
SLASH_B:
		MOV	BUFF_SIZE,0	;Zero buff size for accumulator
NEXT_DIGIT:
		MOV	AX,BUFF_SIZE	;Get current buff size
		MOV	BL,10
		MUL	BL		;Times 10 for next digit
		INC	DI		;Point to next digit
		MOV	BL,[DI]		;And get the next one
		SUB	BL,30H		;Convert it to binary
		JC	PARSE		;If not a digit, keep parsing
		CMP	BL,9
		JA	PARSE		;If not a digit, keep parsing
		MOV	BYTE PTR [DI]," ";Erase character from command
		XOR	BH,BH
		ADD	AX,BX		;Add in this digit
		MOV	BUFF_SIZE,AX	;And save the new total
		JMP	NEXT_DIGIT
SLASH_P:
		INC	DI		;Point to the printer number
		MOV	AL,[DI]
		MOV	BYTE PTR [DI]," ";Erase this char from command
		MOV	PRN_NUM,AL	;Put it in the message area
		SUB	AL,31H		;Convert it to printer number
		XOR	AH,AH		;Make it a word
		CMP	AL,3		;Printer id must be less than 3
		JAE	INVALID_PARAM	;If it isn't, take jump
		MOV	PRINTER_NUM,AX	;Store the parameter
		JMP	PARSE		;Look for more parameters
NO_PARAMS:
		MOV	DX,OFFSET REDIRECT_MESS ;Point to message
		MOV	AH,9		;Display the string of text
		INT	21H		;Using DOS display function
		MOV	DX,OFFSET PRN_TXT ;Point to "PRN"
		CALL	STRING_CRLF	;Display the string
		MOV	AL,0		;Turn off redirection switch
		JMP	CHECK_FOR_INSTALL
PARSE_DONE:
		CMP	BUFF_SIZE,1	;Buff must be at least 1K
		JB	INVALID_PARAM	;If not, exit with error
		CMP	BUFF_SIZE,64	;Check for maximum buff size
		JA	INVALID_PARAM	;If above, exit with error
		MOV	AL," "		;Look for spaces
		CALL	LOAD_PARAMS
		REPE	SCASB		;Scan for non-space character
		JCXZ	NO_PARAMS	;Any letters found?

		CMP	BYTE PTR [DI],":" ;Was a drive specified?
		JNE	GET_DEF_DRIVE	;If not, get the default drive
		DEC	DI		;Now DI points to first letter
		MOV	AL,[DI]		;Get drive letter in AL
		MOV	WORD PTR [DI],2020H;Erase the drive and colon
		JMP	STORE_DRIVE
GET_DEF_DRIVE:
		MOV	AH,19H		;Get default drive
		INT	21H
		ADD	AL,65		;Convert integer drive to ascii
STORE_DRIVE:
		MOV	AH,":"		;AL has drive, AH has colon
		MOV	WORD PTR FILENAME,AX ;Store drive and colon
		MOV	AL,"\"		;Look for a backslash
		MOV	FILENAME+2,AL	;Add a backslash to filename
		CALL	LOAD_PARAMS
		REPNE	SCASB		;Scan for a backslash
		JCXZ	GET_DEF_PATH	;If no path, use current path
		MOV	DI,OFFSET FILENAME+2 ;Location to store path
		JMP	STORE_PATH
GET_DEF_PATH:
		MOV	DL,FILENAME	;Selected drive letter
		AND	DL,11011111B	;Convert it to upper case
		SUB	DL,64		;Convert it to integer
		MOV	SI,OFFSET FILENAME + 3 ;Put current path at SI
		MOV	DI,SI		;Save this for search later
		MOV	AH,47H		;DOS get current directory
		INT	21H
		JC	BAD_NAME_EXIT	;Exit if invalid drive
		MOV	AL,0		;Look for end of path
		CMP	[DI],AL		;Was there any path?
		JE	STORE_PATH	;If not, don't scan it
		MOV	CX,64		;Maximum number of bytes in path
		REPNE	SCASB		;Scan for end of path string
		MOV	BYTE PTR [DI-1],"\" ;Add the trailing backslash
STORE_PATH:
		PUSH	DI		;Save location to append path
		MOV	AL," "		;Look for blank spaces
		CALL	LOAD_PARAMS
		REPE	SCASB		;Scan for non-blank character
		MOV	SI,DI
		DEC	SI		;This is first letter of path
		POP	DI		;Get back location to append
COPY_PATH:
		LODSB			;Get next char of path
		CMP	AL," "		;Is it a blank?
		JE	VERIFY_NAME	;If yes, its the last char
		CMP	AL,13		;Is it a carriage return?
		JE	VERIFY_NAME	;If yes, its the last char
		STOSB			;Store this letter
		JMP	COPY_PATH	;Copy until end of path found
VERIFY_NAME:
		PUSH	DI		;Save end of string location
		MOV	BYTE PTR [DI],"$" ;Mark eos for dos display
		MOV	DX,OFFSET REDIRECT_MESS ;Point to message
		MOV	AH,9		;Display the string of text
		INT	21H		;Using dos display function
		MOV	DX,OFFSET FILENAME ;Point to filename for display
		CALL	STRING_CRLF	;Display the string
		POP	DI
		MOV	BYTE PTR [DI],0 ;Now make it an ascii string
		MOV	DX,OFFSET FILENAME ;Dx points to the filename
		MOV	AX,3D00H	;Open this file for reading
		INT	21H
		JC	OPEN_ERR	;Error may indicate not found
CLOSE_IT:
		MOV	BX,AX		;Get the handle into BX
		MOV	AH,3EH		;Close the file
		INT	21H
		JMP	FILENAME_OK
OPEN_ERR:
		MOV	CX,0020H	;Attribute for new file
		MOV	AH,3CH		;Create file for writing
		INT	21H		;Dos function to create file
		JNC	CLOSE_IT	;If no error, just close it
BAD_NAME_EXIT:
		MOV	DX,OFFSET BAD_FILENAME
ERR_EXIT:
		CALL	STRING_CRLF	;Display the string
		INT	20H		;Just exit to dos
FILENAME_OK:
		MOV	ES,INSTALLED_SEG;Point to installed program
		PUSH	DS:PRINTER_NUM	;This moves the new printer
		POP	ES:PRINTER_NUM	;number to the resident copy
		MOV	DI,OFFSET FILENAME ;Setup to copy the filename
		MOV	SI,DI
		MOV	CX,128		;Copy entire file specification
		REP	MOVSB		;String move instruction
		MOV	AL,1		;Turn redirection on
CHECK_FOR_INSTALL:
		MOV	CX,CS
		CMP	CX,INSTALLED_SEG
		MOV	ES,INSTALLED_SEG
		MOV	ES:SWITCH,AL	;Store the new on/off switch
		JE	INSTALL		;If not installed yet, do it now
		INT	20H		;Otherwise terminate

;----------------------------------------------------------------------
; This subroutine displays a string followed by a CR and LF
;----------------------------------------------------------------------
STRING_CRLF	PROC	NEAR

		MOV	AH,9		;Display the string of text
		INT	21H		;Using dos display function
		MOV	DX,OFFSET CRLF	;Now point to CR/LF characters
		MOV	AH,9		;Send the CR and LF
		INT	21H
		RET

STRING_CRLF	ENDP

;----------------------------------------------------------------------
; This subroutine sets DI to the command line and CX to the byte count
;----------------------------------------------------------------------
LOAD_PARAMS	PROC	NEAR

		MOV	DI,80H		;Point to parameter area
		MOV	CL,CS:[DI]	;Get number of chars into CL
		XOR	CH,CH		;Make it a word
		INC	DI		;Point to first character
		CLD			;String search forward
		RET

LOAD_PARAMS	ENDP

;----------------------------------------------------------------------
; This code does the actual installation by storing the existing
; interrupt vectors and replacing them with the new ones.
; Then allocate memory for the buffer.  Exit and remain resident.
;----------------------------------------------------------------------
		ASSUME	DS:CSEG, ES:CSEG
INSTALL:
		MOV	BX,OFFSET END_OF_CODE	;Get end of resident code
		ADD	BX,15
		MOV	CL,4		;Shift by 4 to divide by 16
		SHR	BX,CL		;This converts to paragraphs
		MOV	AH,4AH		;Modify memory block
		INT	21H		;Dos setblock function call
		JNC	ALLOCATE_BUFFER	;If it worked ok, then continue
ALLOC_ERROR:
		MOV	DX,OFFSET BAD_ALLOC ;Err message for bad allocation
		JMP	ERR_EXIT	;Display message and exit
ALLOCATE_BUFFER:
		MOV	BX,BUFF_SIZE	;Buffer size in K bytes
		MOV	CL,6		;Shift by 6 to get paragraphs
		SHL	BX,CL		;Buffersize is in paragraphs
		MOV	AH,48H
		INT	21H		;Dos allocate memory
		JC	ALLOC_ERROR	;If allocation error, take jump
		MOV	BUFF_SEGMENT,AX	;Save the segment for the buffer

		MOV	AX,BUFF_SIZE	;Buffer size in K bytes
		MOV	CL,10		;Shift by 10 to get bytes
		SHL	AX,CL
		OR	AX,AX		;Is buff_size=0 (64K)?
		JNZ	SIZE_OK
		DEC	AX		;If yes, make it FFFFh
SIZE_OK:
		MOV	BUFF_SIZE,AX	;Now buff_size is in bytes

		ASSUME	ES:NOTHING

		MOV	AH,34H		;Get dos busy flag location
		INT	21H
		MOV	WORD PTR [DOS_FLAG]  ,BX ;Store flag address
		MOV	WORD PTR [DOS_FLAG+2],ES

           	MOV	AX,3508H	;Get timer interrupt vector
		INT	21H
		MOV	WORD PTR [OLDINT08]  ,BX
		MOV	WORD PTR [OLDINT08+2],ES
		MOV	DX, OFFSET NEWINT08
		MOV	AX, 2508H
		INT	21H		;Dos function to change vector

           	MOV	AX,3517H	;Get printer interrupt vector
		INT	21H
		MOV	WORD PTR [OLDINT17]  ,BX
		MOV	WORD PTR [OLDINT17+2],ES
		MOV	DX, OFFSET NEWINT17
		MOV	AX, 2517H
		INT	21H		;Dos function to change vector

           	MOV	AX,3521H	;Get dos function vector
		INT	21H
		MOV	WORD PTR [OLDINT21]  ,BX
		MOV	WORD PTR [OLDINT21+2],ES
		MOV	DX, OFFSET NEWINT21
		MOV	AX, 2521H
		INT	21H		;Dos function to change vector

           	MOV	AX,3528H	;Get dos waiting vector
		INT	21H
		MOV	WORD PTR [OLDINT28]  ,BX
		MOV	WORD PTR [OLDINT28+2],ES
		MOV	DX, OFFSET NEWINT28
		MOV	AX, 2528H
		INT	21H		;Dos function to change vector

;----------------------------------------------------------------------
; Deallocate our copy of the enviornment.  Exit using interrupt 27h
; (TSR). This leaves code and space for buffer resident.
;----------------------------------------------------------------------

		MOV	AX,DS:[002CH]	;Get segment of enviornment
		MOV	ES,AX		;Put it into ES
		MOV	AH,49H		;Release allocated memory
		INT	21H
		MOV	DX,(OFFSET END_OF_CODE - OFFSET CSEG + 15)SHR 4
		MOV	AX,3100H
		INT	21H		;Terminate and stay resident

;----------------------------------------------------------------------
; This procedure removes PRN2FILE from memory by replacing the vectors
; and releasing the memory used for the code and buffer.
;----------------------------------------------------------------------
		ASSUME	DS:CSEG, ES:NOTHING
UNINSTALL:
		MOV	AL,08H		;Check the timer interrupt
		CALL	CHECK_SEG	;If changed, can't uninstall
		JNE	CANT_UNINSTALL

		MOV	AL,17H		;Check the printer interrupt
		CALL	CHECK_SEG	;If changed, can't uninstall
		JNE	CANT_UNINSTALL

		MOV	AL,21H		;Check dos interrupt
		CALL	CHECK_SEG	;If changed, can't uninstall
		JNE	CANT_UNINSTALL

		MOV	AL,28H		;Check dos idle interrupt
		CALL	CHECK_SEG	;If changed, can't uninstall
		JNE	CANT_UNINSTALL

		MOV	ES,INSTALLED_SEG
		ASSUME	DS:NOTHING, ES:NOTHING

		LDS	DX,ES:OLDINT08	;Get original vector
		MOV	AX,2508H
		INT	21H		;Dos function to change vector

		LDS	DX,ES:OLDINT17	;Get original vector
		MOV	AX,2517H
		INT	21H		;Dos function to change vector

		LDS	DX,ES:OLDINT21	;Get original vector
		MOV	AX,2521H
		INT	21H		;Dos function to change vector

		LDS	DX,ES:OLDINT28	;Get original vector
		MOV	AX,2528H
		INT	21H		;Dos function to change vector

		MOV	ES,ES:BUFF_SEGMENT;Get segment of buffer
		MOV	AH,49H		;Free its allocated memory
		INT	21H
		JC	RELEASE_ERR	;If error, take jump

		MOV	ES,INSTALLED_SEG;The resident program segment
		NOT	WORD PTR ES:START
		MOV	AH,49H		;Free its allocated memory
		INT	21H
		JC	RELEASE_ERR	;If error, take jump
		MOV	AX,4C00H
		INT	21H		;Exit to dos
RELEASE_ERR:
		MOV	DX,OFFSET BAD_ALLOC ;Memory allocation error
		JMP	ERR_EXIT	;Exit with error message
CANT_UNINSTALL:
		MOV	DX,OFFSET BAD_UNINSTALL ;Point to error message
		JMP	ERR_EXIT	;Exit with error message

;----------------------------------------------------------------------
; This subroutine checks to see if an interrupt vector points to the
; installed program segment. Returns with ZF=1 if it does.
;----------------------------------------------------------------------
CHECK_SEG	PROC	NEAR

		MOV	AH,35H		;Get the vector
		INT	21H		;Dos function to get the vector
		MOV	AX,ES
		CMP	AX,INSTALLED_SEG;Is it the installed segment?
		RET

CHECK_SEG	ENDP
;----------------------------------------------------------------------
FILENAME	LABEL	BYTE		;File name will go here
END_OF_CODE	=	$ + 128		;Allow 128 bytes for it

CSEG		ENDS
		END	START
