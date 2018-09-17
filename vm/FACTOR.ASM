
   ========= BEGIN FACTORIAL.ASM ==========


; Factorial, full-blown, recursive.

START:  PUSHC   RET1
        PUSHC   PROMPT
        BRANCH  STRINGPRINT ; print the prompt
RET1:   RDINT               ; READ IN A NUMBER TO TAKE FACTORIAL OF
        PUSHC   13
        WRCHAR              ; WRITE CR
        POPC    INPUT
        PUSH    INPUT
        BRLSS   END        ; FINISH WHEN INPUT < 0
        PUSHC   RET2
        PUSH    INPUT
        BRANCH  FACT       ; CALL FACT WITH INPUT
RET2:   WRINT               ; WRITE RESULT
        PUSHC   13
        WRCHAR              ; WRITE CR
        BRANCH  START
END:    HALT
INPUT:  0
PROMPT: "Enter N (< 0 to quit): " 0

FACT:   ; N ON TOP OF STACK, RETURN ADDRESS BELOW IT.
        POPC    N
        PUSH    N
        BREQL   FACTZERO   ; IF N = 0, RETURN 1
        PUSH    N          ; SAVE N ON STACK
        PUSHC   FACTRET    ; PUSH RETURN ADDRESS
        PUSH    N
        PUSHC   1
        SUB                 ; PUT N-1 ON STACK
        BRANCH  FACT       ; AFTER CALL, STACK IS: FACT(N-1) / N / RET.ADDR.
FACTRET:
        MUL                ; FACT(N-1) * N
        POPC    N          ; EXCHANGE RESULT AND RET. ADDR. ON STACK
        POPC    FACTRA
        PUSH    N
        PUSH    FACTRA
        JUMP                ; RETURN
FACTZERO:
        POPC    FACTRA     ; PUT RETURN ADDRESS IN FACTRA
        PUSHC   1          ; RETURN A 1
        PUSH    FACTRA
        JUMP
N:      0
FACTRA: 0


STRINGPRINT:                ; print a zero-terminated string
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