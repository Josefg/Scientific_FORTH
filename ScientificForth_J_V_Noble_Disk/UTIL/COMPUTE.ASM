;--------------------------------------------------------------------;
;  COMPUTE * PC Magazine * Michael J. Mefford                        ;
;  Command line five function math calculator.                       ;
;--------------------------------------------------------------------;

_TEXT          SEGMENT PUBLIC 'CODE'
               ASSUME  CS:_TEXT,DS:_TEXT,ES:_TEXT,SS:_TEXT
               ORG     100H
START:         JMP     MAIN


;              DATA AREA
;              ---------
SIGNATURE      DB      CR,SPACE,SPACE,SPACE,CR,LF

COPYRIGHT      DB      "COMPUTE 1.0 (C) 1990 Ziff Communications Co.",CR,LF
PROGRAMMER     DB      "PC Magazine ",BOX," Michael J. Mefford",CR,LF,LF,"$"
SYNTAX         DB      "Syntax: COMPUTE arithmetic expression",CR,LF
               DB      "Operators supported are + - * / % ( ) [ ] 0-9 x",CR,LF
               DB      "Note: % = modulo, remainder of integer division.",CR,LF
               DB      "      x = answer of last compute."
CRLFLF         DB      LF
CRLF           DB      CR,LF
               DB      "$",CTRL_Z

CR             EQU     13
LF             EQU     10
CTRL_Z         EQU     26
SPACE          EQU     32
BOX            EQU     254
COMMA          EQU     ","

INTEGER_PRECISION      =    20
DECIMAL_PRECISION      =    20
PRECISION      =       INTEGER_PRECISION + DECIMAL_PRECISION

NUMBER         STRUC
SIGN           DB      0
INTEGER        DB      INTEGER_PRECISION DUP (0)
DECIMAL        DB      DECIMAL_PRECISION DUP (0)
NUMBER         ENDS

SIZE_MANTISSA  =       SIZE INTEGER + SIZE DECIMAL
SIZE_DIVIDEND  =       SIZE_MANTISSA + SIZE DECIMAL + 1

OPERAND        DW      INITIAL_VALUE
OPERATORS      DB      "+-"
UNARY_PREFIXES LABEL   BYTE
PRECEDENCE     DB      "*/%"
PRECEDENCE_CNT =       $ - PRECEDENCE
OPERATOR_CNT   =       $ - OPERATORS
               DB      "([",0
PREFIXES_CNT   =       $ - UNARY_PREFIXES

LAST_CHAR      DB      ?

PAREN_CNT      DW      0
PAREN_TYPE     DB      50 DUP (0)
WORKSPACE      DB      2 * SIZE_MANTISSA DUP (?)
WORKSPACE_END  EQU     $ - 1
               DB      ?                       ;Dividend overflow.
DIVIDEND       DB      SIZE_DIVIDEND DUP (?)
ANSWER         DB      SIZE NUMBER DUP (0)

DECIMAL_PLACES DW      0                       ;Significant decimal digits.
SAVE_SIGN      DB      ?
MODULO_FLAG    DB      0                       ; =1 if modulo division.

COMPUTE        DB      "COMPUTE.COM",0
COMPUTE_LEN    EQU     $ - COMPUTE
PATH           DB      "PATH="
PATH_LEN       EQU     $ - PATH
NOT_FOUND      DB      CR,LF,"Could not save answer in Compute.com",CR,LF
               DB      "Compute.com must be in current directory",CR,LF
               DB      "or in DOS PATH= directory.",CR,LF,"$"
RESULT_MSG     DB      "Result is $"
UNRECOGNIZED   DB      "Unrecognized character$"
MISSING_PAREN  DB      "Missing parenthesis or bracket$"
UNMATCHED      DB      "Miss-matched parenthesis with bracket$"
ZERO_DIVIDE    DB      "Divide by zero$"
OVERFLOW       DB      "Overflow$"
LOGICAL        DB      "Logic error$"
MEMORY         DB      "Requires 10K of memory$"
ERROR1         DB      "Error ^",CR,LF,"$"
ERROR2         DB      "^ Error$",CR,LF,"$"


;              CODE AREA
;              ---------
MAIN           PROC    NEAR
               CLD                             ;Strings forward.

               MOV     DX,OFFSET SIGNATURE     ;Announce ourselves.
               CALL    PRINT_STRING

               MOV     BX,10 / 16 * 1024       ;Minimum of 10K required.
               MOV     AH,4AH                  ;Request via DOS.
               INT     21H
               MOV     DX,OFFSET MEMORY
               JC      ERROR_EXIT              ;Exit with message if not enough.

               MOV     SP,16 * 1024 - 2        ;Else, setup stack at end of seg.

               MOV     SI,81H                  ;Point to command line.
               XOR     AX,AX                   ;No characters yet.
               XOR     DX,DX                   ;Not unary.
               CALL    PARSE                   ;Parse command line.
               CALL    PARSE_DELIMIT           ;See if reached end.
               MOV     DX,OFFSET LOGICAL       ;If not, logical error.
               JNC     ERROR_EXIT

               CMP     OPERAND,OFFSET INITIAL_VALUE
               JNZ     CK_PARENS
               MOV     DX,OFFSET SYNTAX        ;Did we get a value.
               CALL    PRINT_STRING            ;If not, print syntax and exit.
               JMP     SHORT ERROR_END

CK_PARENS:     MOV     DX,OFFSET MISSING_PAREN
               CMP     PAREN_CNT,0             ;Did we match all parenthesis
               JNZ     ERROR_EXIT              ; and brackets?

               CALL    RESULT                  ;Every thing seems OK so
                                               ; display result.
EXIT:          MOV     AX,4C00H                ;Exit with ErrorLevel=0
               INT     21H
MAIN           ENDP

;----------------------------------------------;
ERROR_EXIT:    PUSH    DX                      ;Save error message.
               MOV     DX,OFFSET SYNTAX        ;Display syntax.
               CALL    PRINT_STRING
               POP     DX
               CALL    PRINT_STRING            ;Display error.
               MOV     DX,OFFSET CRLFLF
               CALL    PRINT_STRING
               MOV     BX,SI                   ;Current command line pointer.
               MOV     SI,81H                  ;Point at start of command line.
               CLD                             ;Make strings are forward.

NEXT_LINE:     MOV     CX,79                   ;1 less than 80 char. display.
NEXT_ERROR:    LODSB
               CMP     AL,CR                   ;End of command line?
               JZ      PRINT_POINTER
               CALL    PRINT_CHAR              ;If no, print character.
               CMP     SI,BX                   ;Pointing to error position?
               JZ      PRINT_POINTER           ;If yes, done here.
               LOOP    NEXT_ERROR              ;Else, continue.
               MOV     DX,OFFSET CRLF          ;Filled on line; new line.
               CALL    PRINT_STRING
               JMP     NEXT_LINE

PRINT_POINTER: MOV     DX,OFFSET CRLF          ;New line.
               CALL    PRINT_STRING
               SUB     CX,79                   ;Calculate chars. on line above.
               NEG     CX
               MOV     BX,OFFSET ERROR1        ;Assume left side of display.
               CMP     CX,40
               JB      LEFT_ARROW
               SUB     CX,6
               JMP     SHORT NEXT_SPACE
LEFT_ARROW:    MOV     BX,OFFSET ERROR2

NEXT_SPACE:    MOV     AL,SPACE                ;Pad with spaces over to error.
               CALL    PRINT_CHAR
               LOOP    NEXT_SPACE
               MOV     DX,BX                   ;Display "Error ^".
               CALL    PRINT_STRING

ERROR_END:     MOV     AX,4C01H                ;Exit with ErrorLevel = 1.
               INT     21H

;----------------------------------------------;
; Recursive parsing routine.                   ;
;----------------------------------------------;
PARSE:         PUSH    BP                      ;Preserve stack index.
               PUSH    DX                      ;Unary flag.
               PUSH    AX                      ;Operator.
               MOV     BP,SP                   ;Point to local vars.
NEXT_PARSE:    CALL    PARSE_DELIMIT           ;Parse white space.
               JNC     GET_CHAR                ;If command line end, exit.
               JMP     PARSE_END

GET_CHAR:      LODSB                           ;Else, get the character.
               MOV     DI,OFFSET OPERATORS     ;Is it a function operator?
               MOV     CX,OPERATOR_CNT
               REPNZ   SCASB
               JNZ     CK_NUM                  ;If no, check if number.

               XOR     DL,DL                   ;Assume not unary.
               CMP     AL,"+"                  ;Is it plus or minus?
               JZ      CK_UNARY
               CMP     AL,"-"
               JNZ     GET_OPERAND2            ;If no, not unary.
CK_UNARY:      PUSH    AX
               MOV     AL,LAST_CHAR            ;Else, does previous char
               MOV     DI,OFFSET UNARY_PREFIXES  ; suggest unary?
               MOV     CX,PREFIXES_CNT
               REPNZ   SCASB
               POP     AX
               JNZ     GET_OPERAND2
               INC     DL                      ;If yes, it's unary.

GET_OPERAND2:  PUSH    WORD PTR LAST_CHAR      ;Save last char.
               MOV     LAST_CHAR,AL            ;Store current char.
               PUSH    OPERAND                 ;Save operand pointer.
               CALL    PARSE                   ;Get second operand.
               POP     BX                      ;First operand.
               POP     CX                      ;Last character.
               MOV     DH,AL                   ;Save operator.
               MOV     DI,OPERAND              ;Second operand.
               MOV     AL,[DI]                 ;Get signs.
               MOV     AH,[BX]
               CMP     DH,"*"                  ;Check which operator.
               JZ      DO_MULT
               CMP     DH,"/"
               JZ      DO_DIV
               CMP     DH,"%"
               JZ      DO_MOD
                                               ;Must be plus or minus.
               OR      DL,DL                   ;Was it unary.
               JZ      DO_ADD                  ;If no, add operands.
               MOV     AL,CL                   ;Else, was last character
               CALL    CK_PRECEDENCE           ; "*", "/" or "%" ?
               JNZ     NEXT_PARSE              ;If no, just continue parsing.
               JMP     PARSE_END               ;Else, return second operand.

DO_ADD:        CALL    ADD_IT                  ;Add operands.
               MOV     AL,LAST_CHAR
               CMP     AL,")"                  ;Is this an equantity?
               JZ      PARSE_DONE              ;Is yes, return it.
               CMP     AL,"]"
               JZ      PARSE_DONE
               JMP     NEXT_PARSE              ;Else, continue parsing.

DO_MULT:       CALL    MULTIPLY
               JMP     NEXT_PARSE

DO_DIV:        CALL    DIVIDE
               JMP     NEXT_PARSE

DO_MOD:        CALL    MODULO
               JMP     NEXT_PARSE

PARSE_DONE:    JMP     PARSE_END               ;Lilly pad for return.

CK_NUM:        MOV     LAST_CHAR,AL            ;Store current character.
               CMP     AL,"X"                  ;Is it "answer" character?
               JZ      DO_ANS
               CMP     AL,"x"
               JNZ     CK_DIGIT
DO_ANS:        PUSH    SI
               MOV     SI,OFFSET ANSWER        ;If yes copy answer to operand.
               ADD     OPERAND,SIZE NUMBER     ;Point to next operand storage.
               MOV     DI,OPERAND
               PUSH    DI
               MOV     CX,SIZE NUMBER
               REP     MOVSB
               POP     DI
               MOV     AL,[BP]                 ;Get sign
               CMP     AL,"-"
               JZ      ANS_SIGN
               XOR     AL,AL                   ; and store.
ANS_SIGN:      XOR     [DI],AL
               POP     SI
               JMP     SHORT CK_UNARY_NUM      ;Check if return or not.

CK_DIGIT:      CMP     AL,"."                  ;Is char a num char?
               JZ      DO_NUM
               CMP     AL,"0"
               JB      CK_PAREN
               CMP     AL,"9"
               JA      CK_PAREN
DO_NUM:        DEC     SI                      ;If yes, point to it.
               ADD     OPERAND,SIZE NUMBER     ;Point to next operand storage.
               CALL    GET_NUMBER              ;Get the number.
CK_UNARY_NUM:  CMP     BYTE PTR [BP + 2],1     ;Is this a unary number?
               JZ      PARSE_END               ;If yes, return the number.
               MOV     AL,[BP]                 ;Get operator.
               CALL    CK_PRECEDENCE           ;If high precedence, go do it.
               JZ      PARSE_END
               JMP     NEXT_PARSE              ;Else, continue parsing.

CK_PAREN:      MOV     AH,")"
               CMP     AL,"("                  ;Is it an open parenthesis
               JZ      FOUND_PAREN             ; or bracket?
               CMP     AL,AH
               JZ      CK_OPEN
               MOV     AH,"]"
               CMP     AL,"["
               JZ      FOUND_PAREN
               CMP     AL,AH                   ;Was it a close parenthesis or
               MOV     DX,OFFSET UNRECOGNIZED  ; or bracket?
               JNZ     PARSE_ERROR             ;If no, must be invalid char.

CK_OPEN:       DEC     PAREN_CNT               ;If close, decrement level.
               MOV     DX,OFFSET MISSING_PAREN ;If overflow then had no
               JC      PARSE_ERROR             ; open.
               MOV     BX,PAREN_CNT            ;Retrieve level index
               MOV     PAREN_TYPE[BX],AH       ; and store close type.
               JMP     SHORT PARSE_END         ;Return results.

FOUND_PAREN:   PUSH    PAREN_CNT               ;Save level index.
               INC     PAREN_CNT               ;Next level.
               XOR     DX,DX                   ;Not unary flag.
               CALL    PARSE                   ;Parse the equantity.
               POP     BX                      ;Retrieve level.
               CMP     AH,PAREN_TYPE[BX]       ;Did we close with matching
               MOV     DX,OFFSET UNMATCHED     ; parenthesis or bracket?
               JNZ     PARSE_ERROR
               MOV     AL,[BP]                 ;Retrieve, prefix operator.
               CALL    CK_PRECEDENCE           ;Was it priority operator?
               JZ      PARSE_END               ;If yes, return value.
               CMP     AL,"-"                  ;Else store sign.
               JNZ     PAREN_END
               MOV     BX,OPERAND
               XOR     [BX],AL
PAREN_END:     JMP     NEXT_PARSE              ;And parse next.

PARSE_END:     POP     AX
               POP     DX
               POP     BP
               RET

PARSE_ERROR:   JMP     ERROR_EXIT
               RET

;-------------------------------------------------------------------;
; INPUT: SI -> string                                               ;
; OUTPUT SI -> first non-white space and CY=0 or SI -> CR and CY=1. ;
;-------------------------------------------------------------------;
PARSE_DELIMIT: LODSB                           ;Get a byte.
               CMP     AL,CR
               JZ      STRING_END
               CMP     AL,SPACE                ;Is it a space char or below?
               JBE     PARSE_DELIMIT
               DEC     SI                      ;Else, adjust pointer to
               CLC
               RET                             ; string start.

STRING_END:    DEC     SI
               STC
               RET

;----------------------------------------------;
CK_PRECEDENCE: MOV     DI,OFFSET PRECEDENCE
               MOV     CX,PRECEDENCE_CNT
               REPNZ   SCASB
               RET

;----------------------------------------------;
GET_NUMBER:    MOV     DI,OFFSET WORKSPACE     ;Store integer temporarily
               MOV     CX,SIZE INTEGER + 1     ; so can store right justified.
NEXT_INTEGER:  LODSB
               CMP     AL,COMMA                ;Ignore commas.
               JZ      NEXT_INTEGER
               CMP     AL,"."                  ;Decimal point; end of integer.
               JZ      STORE_INTEGER

GET_NUM2:      SUB     AL,"0"
               JC      INTEGER_END
               CMP     AL,9
               JA      INTEGER_END
               STOSB                           ;Store the number.
               LOOP    NEXT_INTEGER
               MOV     DX,OFFSET OVERFLOW      ;If got here, number too big.
               JMP     ERROR_EXIT

INTEGER_END:   DEC     SI
STORE_INTEGER: PUSH    SI                      ;Save command line pointer.
               MOV     SI,DI
               DEC     SI                      ;End of workspace.
               PUSHF
               STD
               MOV     DI,OPERAND
               ADD     DI,SIZE INTEGER         

               MOV     DX,CX
               DEC     DX                      ;Zero pad count.

               NEG     CX
               ADD     CX,SIZE INTEGER + 1
               JCXZ    PAD_INTEGER
               REP     MOVSB                   ;Store integer part in operand.

PAD_INTEGER:   MOV     CX,DX
               JCXZ    INTEGER_DONE
               XOR     AL,AL
               REP     STOSB                   ;Pad balance of integer with 0.
INTEGER_DONE:  MOV     AL,[BP]                 ;Get sign
               CMP     AL,"-"
               JZ      INT_SIGN
               XOR     AL,AL                   ; and store.
INT_SIGN:      STOSB
               POPF


DO_DECIMAL:    POP     SI                      ;Restore command line pointer.
               MOV     DI,OPERAND
               ADD     DI,SIZE SIGN + SIZE INTEGER   ;Decimal operand storage.
               MOV     CX,SIZE DECIMAL + 1           ;Left justified.
               XOR     DX,DX                   ;Decimal counter.
NEXT_DECIMAL:  LODSB
               SUB     AL,"0"
               JC      DECIMAL_END
               CMP     AL,9
               JA      DECIMAL_END
               STOSB
               INC     DX                      ;Increment decimal count.
               LOOP    NEXT_DECIMAL
               MOV     DX,OFFSET OVERFLOW      ;If got here, number too big.
               JMP     ERROR_EXIT

DECIMAL_END:   DEC     SI                      ;Adjust command line pointer.
               CMP     DX,DECIMAL_PLACES       ;Store significant decimal
               JBE     PAD_DECIMAL             ; places.
               MOV     DECIMAL_PLACES,DX

PAD_DECIMAL:   DEC     CX                      ;Pad balance with zeros.
               JCXZ    DECIMAL_DONE
               XOR     AL,AL
               REP     STOSB
DECIMAL_DONE:  RET

;----------------------------------------------;
; INPUT:  BX -> 1st and AH=sign; DI -> 2nd and AL=sign.
; OUTPUT: Result stored in DI.

ADD_IT:        PUSHF
               PUSH    SI
               STD                             ;Add or subtract right to left.
               MOV     SAVE_SIGN,0             ;Assume sign positive.

               MOV     DX,BX                   ;Save operand 2 pointer.
               MOV     CX,SIZE_MANTISSA        ;Point to end of operands.
               ADD     DI,CX
               ADD     BX,CX
               MOV     SI,BX

               CMP     AL,AH                   ;If both plus or minus, then
               JZ      ADD_SIGN                ; go ahead and add, else subtract
               CMP     AL,"-"                  ;If first operand
               JZ      SUB_IT                  ; negative and second positive
               XCHG    DI,SI                   ; swap operands before subtract.
               MOV     OPERAND,DX

SUB_IT:        MOV     BX,DI                   ;Save destination operand.
               CLC                             ;Start with no carry.
NEXT_SUB:      LODSB                           ;Get operand1
               SBB     AL,[DI]                 ;Subtract operand2 and the carry.
               AAS                             ;ASCII adjust after subtraction.
               STOSB                           ;Store it.
               LOOP    NEXT_SUB                ;Subtract next byte.
               MOV     AL,0                    ;If no carry, then positive.
               JNC     STORE_SIGN

               MOV     DI,BX                   ;Else negative so negate by
               CLC                             ; subtracting result from zero.
               MOV     CX,SIZE_MANTISSA
NEXT_NEGATE:   MOV     AL,CH                   ;Zero.
               SBB     AL,[DI]                 ;Subtract the byte.
               AAS                             ;ASCII adjust after subtraction.
               STOSB                           ;Store it.
               LOOP    NEXT_NEGATE
               MOV     AL,"-"                  ;Store negative sign.
               JMP     SHORT STORE_SIGN

;              Add procedure.
;----------------------------------------------;
ADD_SIGN:      MOV     SAVE_SIGN,AL            ;Either 2 +'s or 2 -'s; save it.
               CLC                             ;Start with no carry.
NEXT_ADD:      LODSB
               ADC     AL,[DI]                 ;Add operand two and carry.
               AAA                             ;ASCII adjust.
               STOSB
               LOOP    NEXT_ADD
               JC      ADD_ERROR               ;If carry out of number, overflow
               MOV     AL,SAVE_SIGN            ;Else, store sign.

STORE_SIGN:    STOSB
               POP     SI
               POPF
               RET

ADD_ERROR:     MOV     DX,OFFSET OVERFLOW
               POP     SI
               JMP     ERROR_EXIT

;----------------------------------------------;
; INPUT:  BX -> 1st and AH=sign; DI -> 2nd and AL=sign.
; OUTPUT: Result stored in DI.

MULTIPLY:      PUSH    BP
               PUSHF
               PUSH    SI
               XOR     DL,DL                   ;Assume plus result.
               CMP     AL,AH                   ;Signs the same?
               JZ      MULT_SIGN               ;If yes, assumed right.
               MOV     DL,"-"                  ;Else, negative.
MULT_SIGN:     MOV     SAVE_SIGN,DL

               STD                             ;Right to left.
               ADD     DI,SIZE_MANTISSA        ;Point to end of numbers.
               ADD     BX,SIZE_MANTISSA
               MOV     BP,DI                   ;Save destination.

               MOV     DI,OFFSET WORKSPACE_END
               MOV     DX,DI
               XOR     AX,AX                   ;Zero out scratch pad area.
               MOV     CX,SIZE WORKSPACE
               REP     STOSB

               MOV     DI,DX                   ;Point to workspace.
               MOV     CX,SIZE_MANTISSA        ;Number of digits to multiply.

NEXT_MULT:     PUSH    DI                      ;Save current product place.
               MOV     AL,[BX]                 ;Current multiplier digit zero?
               OR      AL,AL
               JZ      LOOP_MULT               ;If yes, no need to multiply.

               PUSH    CX                      ;Else, save multiplier counter.
               MOV     CX,SIZE_MANTISSA        ;Size of multiplicand size.
               MOV     DL,AL                   ;Multiplier.
               MOV     SI,BP                   ;Multiplicand pointer.
               XOR     DH,DH                   ;No carry yet.

NEXT_MULT2:    LODSB
               MUL     DL                      ;Multiply them.
               AAM                             ;Adjust BCD multiplication.
               ADD     AL,[DI]                 ;Accumulate as we go.
               AAA                             ;ASCII adjust.
               ADD     AL,DH                   ;Add the carry.
               AAA                             ;ASCII adjust.
               STOSB                           ;And store.
               MOV     DH,AH                   ;Save the carry.
               LOOP    NEXT_MULT2
               MOV     [DI],AH                 ;Store the carry.
               POP     CX                      ;Multiply by all digits.

LOOP_MULT:     POP     DI
               DEC     BX                      ;Next multiplier.
               DEC     DI                      ;Next product.
               LOOP    NEXT_MULT

               MOV     DI,OFFSET WORKSPACE_END - SIZE DECIMAL + 1
               MOV     CX,SIZE_MANTISSA
               CALL    ROUND                   ;Round up product.

               MOV     SI,OFFSET WORKSPACE_END - SIZE DECIMAL
               MOV     DI,BP                   ;Place decimal in product
               MOV     CX,SIZE_MANTISSA        ; by storing in destination.
               REP     MOVSB
               MOV     AL,SAVE_SIGN            ;Add the sign.
               STOSB

               MOV     CX,SIZE INTEGER         ;Check for overflow.
NEXT_OVER:     LODSB
               OR      AL,AL
               JNZ     MULT_ERROR
               LOOP    NEXT_OVER

               POP     SI
               POPF
               POP     BP
               RET

MULT_ERROR:    MOV     DX,OFFSET OVERFLOW
               POP     SI
               JMP     ERROR_EXIT


;----------------------------------------------;
; INPUT:  BX -> 1st and AH=sign; DI -> 2nd and AL=sign.
; OUTPUT: Result stored in DI.

DIVIDE:        PUSH    BP
               PUSHF
               PUSH    SI
               XOR     DL,DL                   ;Assume plus result.
               CMP     AL,AH                   ;Signs the same?
               JZ      DIV_SIGN                ;Is yes, assumed right.
               MOV     DL,"-"                  ;Else, negative.
DIV_SIGN:      MOV     SAVE_SIGN,DL

               MOV     BP,DI                   ;BP -> divisor.
               INC     BP                      ;Bump past sign.
               MOV     DI,OFFSET WORKSPACE     ;Quotient scratch pad.
               XOR     AL,AL
               MOV     CX,SIZE_DIVIDEND
               REP     STOSB                   ;Zero out quotient to start.

               MOV     DI,OFFSET DIVIDEND - 1  ;Move dividend to scratch pad.
               STOSB                           ;Zero in overflow byte.
               MOV     SI,BX                   ;Dividend.
               INC     SI                      ;Bump past sign.
               MOV     CX,SIZE_MANTISSA
               REP     MOVSB
               MOV     CX,SIZE_DIVIDEND - SIZE_MANTISSA
               REP     STOSB                   ;Zero out dividend underflow.

               MOV     DI,BP                   ;Look for zero divisor.
               MOV     CX,SIZE_MANTISSA
               REPZ    SCASB
               JNZ     DIVISOR_SIZE
               MOV     DX,OFFSET ZERO_DIVIDE   ;Can't divide by zero.
               POP     SI                      ;Command line pointer.
               JMP     ERROR_EXIT

DIVISOR_SIZE:  MOV     DI,OFFSET WORKSPACE     ;Quotient.
               ADD     DI,CX                   ;Point to first quotient storage.
               ADD     BP,SIZE_MANTISSA - 1
               SUB     BP,CX                   ;First non-zero divisor digit.
               INC     CX                      ;DH = subtraction counter.
               MOV     DX,CX                   ;DL = significant divisor digits.
               MOV     SI,OFFSET DIVIDEND
               MOV     CX,SIZE_DIVIDEND + 1
               SUB     CX,DX                   ;Number of times to divide.

NEXT_DIVIDE:   CMP     MODULO_FLAG,1           ;Is this a modulo division?
               JNZ     NO_MODULO
               CMP     DI,OFFSET WORKSPACE + SIZE_MANTISSA
               JAE     DIVIDE_END              ;If yes, divide integer only.

NO_MODULO:     PUSH    CX                      ;Division count.
               PUSH    DI                      ;Quotient.
               MOV     BX,SI                   ;Save dividend pointer.
               XOR     DH,DH                   ;Initialize sub counter to zero.

NEXT_DIVIDE2:  CMP     BYTE PTR [SI - 1],0     ;Overflow in remainder?
               JNZ     SUB_DIVISOR             ;If yes, divisor fits.
               MOV     CL,DL                   ;Divisor length.
               XOR     CH,CH                   ;Zero high half.
               MOV     DI,BP                   ;Divisor.
NEXT_DIV:      CMPSB                           ;Does it fit in remainder?
               JA      SUB_DIVISOR             ;If yes, subtract.
               JB      DIV_TOO_BIG             ;If no, save subtraction count.
               LOOP    NEXT_DIV

SUB_DIVISOR:   INC     DH                      ;Sub counter.
               MOV     SI,BP                   ;Divisor position.
               MOV     DI,BX                   ;Dividend position.
               MOV     CL,DL                   ;Significant divisor digits.
               XOR     CH,CH                   ;Zero in high half.
               DEC     CX                      ;Adjust for a moment.
               ADD     SI,CX                   ;Point to current position.
               ADD     DI,CX
               INC     CX

               STD                             ;Subtract right to left.
               CLC                             ;Start with no carry.
NEXT_DIV_SUB:  LODSB
               MOV     AH,AL                   ;Divisor digit.
               MOV     AL,[DI]                 ;Remainder digit.
               SBB     AL,AH                   ;Subtract.
               AAS                             ;ASCII adjust.  (BCD)
               STOSB                           ;Store in dividend.
               LOOP    NEXT_DIV_SUB
               SBB     BYTE PTR [DI],0         ;Subtract the carry, if any.

               CLD                             ;Back to forward.
               MOV     SI,BX
               JMP     NEXT_DIVIDE2            ;Next divisor.

DIV_TOO_BIG:   MOV     SI,BX                   ;Next dividend.
               POP     DI
               POP     CX
               MOV     AL,DH                   ;Store quotient.
               STOSB
               INC     SI
               LOOP    NEXT_DIVIDE

               MOV     DI,OFFSET WORKSPACE + SIZE_DIVIDEND - 1
               MOV     CX,SIZE_MANTISSA
               STD
               CALL    ROUND                   ;Round quotient.
               JC      DIVIDE_ERROR

               MOV     CX,SIZE_MANTISSA
               MOV     SI,OFFSET WORKSPACE + SIZE_DIVIDEND - 2
               MOV     DI,OPERAND              ;Store quotient in operand.
               ADD     DI,CX
               REP     MOVSB
               MOV     AL,SAVE_SIGN            ;Add the sign.
               STOSB

               MOV     CX,SIZE DECIMAL         ;Check for overflow.
NEXT_OVER2:    LODSB
               OR      AL,AL
               JNZ     DIVIDE_ERROR
               LOOP    NEXT_OVER2

DIVIDE_END:    POP     SI
               POPF
               POP     BP
               RET

DIVIDE_ERROR:  MOV     DX,OFFSET OVERFLOW
               POP     SI
               JMP     ERROR_EXIT

;----------------------------------------------;
; INPUT:  BX -> 1st and AH=sign; DI -> 2nd and AL=sign.
; OUTPUT: Result stored in DI.

MODULO:        PUSH    AX                      ;Save sign of dividend.
               PUSH    DI                      ;Divisor pointer.
               ADD     DI,SIZE INTEGER + 1
               MOV     CX,SIZE INTEGER
               STD
               CALL    ROUND                   ;Round divisor.
               JC      MODULO_ERROR
               POP     DI
               PUSH    DI
               CALL    MAKE_INTEGER            ;Truncate to integer.

               MOV     DI,BX
               ADD     DI,SIZE INTEGER + 1
               MOV     CX,SIZE INTEGER
               CALL    ROUND                   ;Round dividend.
               JC      MODULO_ERROR
               MOV     DI,BX
               CALL    MAKE_INTEGER            ;Truncate to integer.

               POP     DI
               MOV     MODULO_FLAG,1           ;Do modulo division.
               CLD
               CALL    DIVIDE
               MOV     MODULO_FLAG,0
               MOV     DI,OPERAND
               POP     AX                      ;Sign of dividend.
               MOV     AL,AH
               STOSB                           ;Store it.
               PUSH    SI
               MOV     SI,OFFSET DIVIDEND      ;Dividend has remainder.
               MOV     CX,SIZE_MANTISSA
               REP     MOVSB                   ;Store it in operand.
               POP     SI
               RET

MODULO_ERROR:  MOV     DX,OFFSET OVERFLOW
               JMP     ERROR_EXIT

;----------------------------------------------;
; INPUT: DI -> Start of mantissa.

MAKE_INTEGER:  PUSHF
               CLD
               ADD     DI,SIZE INTEGER + 1
               MOV     CX,SIZE DECIMAL
               XOR     AL,AL
               REP     STOSB
               POPF
               RET

;----------------------------------------------;
; INPUT:  DI -> End of mantissa + 1; Direction flag reversed.
;         CX = number of bytes to round.
; OUTPUT: CY=1 if overflow.

ROUND:         CMP     BYTE PTR [DI],5         ; 5 or greater?
               JB      ROUND_DONE              ;If no, done here.
               DEC     DI                      ;Else, move left one digit.
               STC                             ;Start with carry.
NEXT_ROUND:    MOV     AL,[DI]
               ADC     AL,0                    ;Add carry.
               AAA                             ;ASCII adjust.
               STOSB
               JNC     ROUND_END               ;If no carry, done.
               LOOP    NEXT_ROUND
               JMP     SHORT ROUND_END
ROUND_DONE:    CLC                             ;Return overflow.
ROUND_END:     RET

;----------------------------------------------;
RESULT:        MOV     DX,OFFSET RESULT_MSG    ;Display "Result is ".
               CALL    PRINT_STRING
               MOV     SI,OPERAND              ;Point to result.
               LODSB
               OR      AL,AL                   ;Print sign if negative.
               JZ      PRINT_INTEGER
               CALL    PRINT_CHAR

PRINT_INTEGER: XOR     DH,DH                   ;Non-zero digit flag.
               MOV     BL,3                    ;Comma every third digit.
               MOV     CX,INTEGER_PRECISION
               JMP     SHORT NEXT_INT2         ;Skip prefacing comma.

NEXT_INT:      OR      DH,DH                   ;Non-zero digit yet?
               JZ      NEXT_INT2               ;If no, skip.
               MOV     AX,CX                   ;Is position MOD 3 = 0?
               DIV     BL
               OR      AH,AH
               JNZ     NEXT_INT2               ;If no, no comma.
               MOV     AL,","                  ;Else, delimiting comma.
               CALL    PRINT_CHAR
 
NEXT_INT2:     LODSB
               OR      AL,AL                   ;Is digit zero?
               JNZ     PRINT_INT               ;If no, print it.
               OR      DH,DH                   ;Else, non-zero digit yet?
               JZ      LOOP_INTEGER            ;If no, skip.
PRINT_INT:     OR      DH,AL
               CALL    PRINT_NUMBER
LOOP_INTEGER:  LOOP    NEXT_INT

               PUSH    SI                      ;Save pointer.
               MOV     CX,DECIMAL_PRECISION    ;Count the significant decimal
               MOV     BX,CX                   ; places.
               INC     BX
NEXT_FRACTION: LODSB
               OR      AL,AL
               JZ      LOOP_FRACTION
               MOV     BX,CX                   ;If non-zero, save count.
LOOP_FRACTION: LOOP    NEXT_FRACTION
               POP     SI                      ;Restore pointer.
               NEG     BX                      ;Get count of significant.
               ADD     BX,DECIMAL_PRECISION + 1
               MOV     CX,DECIMAL_PLACES       ;Use the larger of significant
               CMP     CX,BX                   ; digits; number entered or
               JAE     DO_FRACTION             ; result.
               MOV     CX,BX

DO_FRACTION:   JCXZ    CK_NULL                 ;If no significant, done here.
               OR      DH,DH                   ;Else, was there a non-zero
               JNZ     PRINT_DECIMAL           ; integer?
               XOR     AL,AL                   ;If no, preface with zero.
               CALL    PRINT_NUMBER
PRINT_DECIMAL: MOV     AL,"."                  ;Display delimiting decimal.
               CALL    PRINT_CHAR

               XOR     BL,BL                   ;Comma counter.
NEXT_DEC:      CMP     BL,3
               JNZ     NEXT_DEC2
               MOV     AL,","
               CALL    PRINT_CHAR
               XOR     BL,BL
NEXT_DEC2:     LODSB                           ;Display decimal digits.
               CALL    PRINT_NUMBER
               INC     BL
LOOP_DEC:      LOOP    NEXT_DEC
               JMP     SHORT RESULT_END

CK_NULL:       OR      DH,DH                   ;Was there a non-zero integer?
               JNZ     RESULT_END              ;If yes, done here.
               XOR     AL,AL                   ;Else, display solitary, zero.
               CALL    PRINT_NUMBER
RESULT_END:    MOV     DX,OFFSET CRLF          ;New line.
               CALL    PRINT_STRING

;----------------------------------------------;
SAVE_ANSWER:   MOV     SI,OPERAND              ;Copy operand to answer.
               MOV     DI,OFFSET ANSWER
               MOV     CX,SIZE NUMBER
               REP     MOVSB

               MOV     AH,30H                  ;Get DOS version.
               INT     21H
               CMP     AL,3                    ;Filename in environment
               JB      TRY_CURRENT             ; not support before DOS 3.x.

               MOV     DS,DS:[2CH]             ;Segment of environment.
               XOR     SI,SI
FIND_ENV_END:  LODSB                           ;Find end of environment.
               OR      AL,AL
               JNZ     FIND_ENV_END
               LODSB
               OR      AL,AL
               JNZ     FIND_ENV_END

               INC     SI                      ;Bump pointer past word count.
               INC     SI
               MOV     DI,OFFSET WORKSPACE     ;Copy our name.
               CALL    COPY_NAME
               PUSH    CS
               POP     DS
               MOV     DX,OFFSET WORKSPACE     ;Try to write answer.
               CALL    WRITE_ANS
               JNC     ANSWER_END

TRY_CURRENT:   MOV     DX,OFFSET COMPUTE       ;Try current directory.
               CALL    WRITE_ANS
               JNC     ANSWER_END

; See if PATH= is in environment.
               MOV     DS,DS:[2CH]             ;Segment of environment.
LOOK_AT_PATH:  MOV     BX,-1                   ;Offset zero in environment.
NEXT_PATH:     INC     BX
               MOV     SI,BX
               CMP     BYTE PTR [SI],0         ;Terminating null?
               JNZ     PATH_SEARCH
               CMP     BYTE PTR [SI+1],0       ;Double null?
               JNZ     NEXT_PATH
               JMP     SHORT CANT_SAVE         ;If so, end of environment.

PATH_SEARCH:   MOV     DI,OFFSET PATH          ;Search for "PATH=".
               MOV     CX,PATH_LEN
               REP     CMPSB
               JNZ     NEXT_PATH

; PATH= found; search for paths.
NEXT_STRING:   CMP     BYTE PTR [SI],0         ;Terminating null?
               JZ      CANT_SAVE
               CMP     BYTE PTR [SI-1],0       ;Double null?
               JZ      CANT_SAVE

MOVE_STRING:   MOV     DI,OFFSET WORKSPACE     ;Get path.
NEXT_BYTE:     LODSB
               OR      AL,AL
               JZ      SEARCH_PATH
               CMP     AL,";"
               JZ      SEARCH_PATH
               STOSB
               JMP     NEXT_BYTE

SEARCH_PATH:   CALL    COPY_PATH               ;Create filespec.
               PUSH    DS
               PUSH    CS
               POP     DS
               MOV     DX,OFFSET WORKSPACE
               CALL    WRITE_ANS               ;Try to write answer.
               POP     DS
               JC      NEXT_STRING             ;If failed, try next path.
               JMP     SHORT ANSWER_END        ;Else, done.

CANT_SAVE:     PUSH    CS
               POP     DS
               MOV     DX,OFFSET NOT_FOUND     ;Answer write error message.
               CALL    PRINT_STRING

ANSWER_END:    RET

;----------------------------------------------;
COPY_NAME:     LODSB
               STOSB
               OR      AL,AL
               JNZ     COPY_NAME
               RET

;----------------------------------------------;
COPY_PATH:     PUSH    SI
               PUSH    DS
               CMP     DI,OFFSET WORKSPACE
               JZ      MOVE_NAME
               CMP     BYTE PTR ES:[DI-1],"\"  ;Filespec delimiter?
               JZ      MOVE_NAME
               CMP     BYTE PTR ES:[DI-1],":"
               JZ      MOVE_NAME
               MOV     AL,"\"                  ;If no, add one.
               STOSB

MOVE_NAME:     PUSH    CS
               POP     DS
               MOV     SI,OFFSET COMPUTE       ;Tack on "COMPUTE.COM".
               MOV     CX,COMPUTE_LEN
               REP     MOVSB

COPY_END:      POP     DS
               POP     SI
               RET

;----------------------------------------------;
; INPUT:  DX -> Compute.com filespec.
; OUTPUT: CY=0 if successful; CY=1 if failed.

WRITE_ANS:     PUSH    SI
               MOV     AX,3D02H                ;Open for reading and writing.
               INT     21H
               JC      WRITE_END
               MOV     BX,AX
               MOV     DX,OFFSET WORKSPACE
               MOV     CX,SIZE WORKSPACE
               MOV     AH,3FH                  ;Read signature.
               INT     21H
               JC      WRITE_END

               MOV     SI,100H                 ;Make sure we found correct
               MOV     DI,OFFSET WORKSPACE     ; Compute.com.
               REP     CMPSB
               STC
               JNZ     WRITE_END

               XOR     CX,CX
               MOV     DX,OFFSET ANSWER - 100H
               MOV     AX,4200H                ;Seek offset of answer.
               INT     21H
               JC      WRITE_END

               MOV     DX,OFFSET ANSWER
               MOV     CX,SIZE ANSWER
               MOV     AH,40H                  ;Write answer to disk.
               INT     21H
               JC      WRITE_END

               MOV     AH,3EH                  ;Close file.
               INT     21H
WRITE_END:     POP     SI
               RET

;----------------------------------------------;
PRINT_NUMBER:  ADD     AL,"0"                  ;Binary to ASCII.
PRINT_CHAR:    MOV     DL,AL
               MOV     AH,2                    ;Print char via DOS.
               INT     21H
               RET

;----------------------------------------------;
PRINT_STRING:  MOV     AH,9                    ;Print string via DOS.
               INT     21H
               RET

;----------------------------------------------;
; HEAP

INITIAL_VALUE  NUMBER  <>

_TEXT          ENDS
               END     START
