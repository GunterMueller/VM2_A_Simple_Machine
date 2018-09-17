    ======== BEGIN ENTERSTRING.ASM =========


; Enter a string, then print it out.  Beeps if string is too long for buffer.

        PUSHC   BUFFER
        POPC    BUFPTR     ; BUFPTR := ADDRESS OF BUFFER
        PUSHC   '?
        WRCHAR
RDLOOP: RDCHAR
        POPC    INCHAR     ; INCHAR := INPUT CHARACTER
        PUSH    INCHAR     ; DISPLAY IT
        WRCHAR
        PUSH    INCHAR
        PUSHC   13
        SUB
        BREQL   PRINTST    ; WHEN WE SEE A CR, STOP COLLECTING CHARACTERS
                            ; AND PRINT THE STRING
        PUSH    BUFPTR
        PUSHC   BUFFER
        SUB
        PUSHC   19         ; THIS NUMBER SHOULD BE SIZE OF BUFFER - 1
        SUB
        BREQL   BEEP       ; IF BUFPTR AT LAST SPOT IN BUFFER, BEEP AND PRINT
        PUSH    INCHAR
        PUSH    BUFPTR
        POP                 ; PUT INPUT CHAR INTO NEXT SPOT IN BUFFER
        PUSH    BUFPTR
        PUSHC   1
        ADD                 ; INCREMENT BUFPTR
        POPC    BUFPTR
        BRANCH  RDLOOP
BEEP:   PUSHC   7          ; ASCII BEL CHARACTER
        WRCHAR
PRINTST:
        PUSHC   0
        PUSH    BUFPTR        POP                 ; PUT A 0 AT THE END OF THE STRING
        PUSHC   13
        WRCHAR              ; WRITE A CR
        PUSHC   RET
        PUSHC   BUFFER
        BRANCH  STRINGPRINT ; PRINT STRING
RET:    PUSHC   13
        WRCHAR
        HALT

; Data for main program
BUFFER: .BLOCK 20
BUFPTR: 0
INCHAR: 0

STRINGPRINT:
        POPC    STRADDR

SPLOOP: PUSH    STRADDR    ; SEE IF NEXT CHAR IS 0
        CONTENTS
        BREQL   SPEND      ; IF SO, END
        PUSH    STRADDR
        CONTENTS            ; WRITE NEXT CHAR
        WRCHAR
        PUSH    STRADDR    ; INCREMENT POINTER TO STRING
        PUSHC   1
        ADD
        POPC    STRADDR
        BRANCH  SPLOOP
SPEND:  JUMP                ; TOS IS RETURN ADDRESS; JUMP TO IT
STRADDR: 0                  ; HOLDS ADDRESS OF STRING
