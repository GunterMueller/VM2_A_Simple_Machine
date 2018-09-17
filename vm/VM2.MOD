IMPLEMENTATION MODULE VM2;

(* The VM2 virtual machine. *)

FROM VM2Defs IMPORT address, maxMem, opCode;
FROM InOut IMPORT WriteString, Write, WriteLn, WriteInt, ReadInt, Read;
FROM MyTerminal IMPORT fatal;
FROM SYSTEM IMPORT WORD;

CONST SPoffset      = 1;    (* offsets from FP *)
      oldFPoffset   = 2;


VAR memory: ARRAY address OF WORD;
    stackPtr, programCtr, stackLimit, framePtr: address;
    errorFlag: BOOLEAN;

                (*** Interface procedures ***)

PROCEDURE putWord(w:WORD; a:address);
BEGIN
    memory[a] := w;
END putWord;


PROCEDURE getWord(a:address):WORD;
BEGIN
    RETURN memory[a];
END getWord;

PROCEDURE getStackLimit():address;
BEGIN
    RETURN stackLimit;
END getStackLimit;

PROCEDURE setStackLimit(a:address);
BEGIN
    stackLimit := a;
END setStackLimit;

PROCEDURE getProgramCtr():address;
BEGIN
    RETURN programCtr;
END getProgramCtr;

PROCEDURE setProgramCtr(a:address);
BEGIN
    programCtr := a;
END setProgramCtr;

PROCEDURE getStackPtr():address;
BEGIN
    RETURN stackPtr;
END getStackPtr;

PROCEDURE setStackPtr(a:address);
BEGIN
    stackPtr := a;
END setStackPtr;

PROCEDURE getFramePtr():address;
BEGIN
    RETURN framePtr;
END getFramePtr;

PROCEDURE setFramePtr(a:address);
BEGIN

    framePtr := a;
END setFramePtr;

PROCEDURE reset;
BEGIN
    flush;
    framePtr := 0;
END reset;

PROCEDURE run(initialPC:address);
BEGIN
    programCtr := initialPC;
    errorFlag := FALSE;
    WHILE NOT errorFlag DO      (* keep executing instructions until error *)
        singleStep;
    END;
END run;

PROCEDURE singleStep;
VAR instruction:opCode;
BEGIN
    instruction := opCode(memory[programCtr]);
    INC(programCtr);
    CASE instruction OF
        Add:        pushInt(popInt() + popInt());
    |   Sub:        sub;
    |   Mul:        pushInt(popInt() * popInt());
    |   Div:        div;
    |   Neg:        pushInt(-popInt());
    |   Push:       pushWord(memory[address(memory[programCtr])]);
                    INC(programCtr);
    |   PushConst:  pushWord(memory[programCtr]);
                    INC(programCtr);
    |   Pop:        pop;
    |   PopConst:   memory[address(memory[programCtr])] := popWord();
                    INC(programCtr);
    |   Contents:   pushWord(memory[address(popWord())]);
    |   BranchEqual:condBranch(popInt() = 0);
    |   BranchLess: condBranch(popInt() < 0);
    |   BranchGreater: condBranch(popInt() > 0);
    |   Branch:     branch;
    |   Jump:       programCtr := address(popWord());
    |   Not:        pushBool(popInt() = 0);
    |   Equal:      pushBool(popInt() = popInt());
    |   NotEqual:   pushBool(popInt() <> popInt());
    |   Greater, Less, GreaterEqual, LessEqual: compBool(instruction);
    |   Writeint:   WriteInt(popInt(), 0);
    |   WriteChar:  Write(CHAR(popWord()));
    |   Readint:    readInt;
    |   ReadChar:   readChar;
    |   PushL:      pushL;
    |   PopL:       popL;
    |   AddrL:      addrL;
    |   Call:       call;

    |   Return:     return;
    |   FReturn:    fReturn;
    |   SetSP:      setSP;
    |   Min:        min;
    |   Copy:       copy;
    |   Aref:       aref;
    |   Halt:       errorFlag := TRUE;
    ELSE
        error("Unknown opcode");
    END;
END singleStep;


                (*** Complicated opcodes ***)

PROCEDURE sub;
(* We have to make sure things are popped off in the right order. *)
VAR i:INTEGER;
BEGIN
    i := popInt();
    pushInt(popInt() - i);
END sub;

PROCEDURE div;
(* Things must be popped off in the right order. *)
VAR i:INTEGER;
BEGIN
    i := popInt();
    pushInt(popInt() DIV i);
END div;


PROCEDURE pop;
VAR dest:address;
BEGIN
    dest := address(popWord());
    memory[dest] := popWord();
END pop;


PROCEDURE condBranch(b:BOOLEAN);
BEGIN
   IF b THEN
        branch;
    ELSE
        INC(programCtr);    (* skip over argument *)
    END;
END condBranch;

PROCEDURE branch;
BEGIN
    programCtr := address(memory[programCtr]);
END branch;


                (*** Boolean ***)

PROCEDURE compBool(oc:opCode);
(* For these Boolean instructions, the order in which the pops are done
   matters.  They are defined so that the first thing pushed is the first
   thing in the comparison, e.g. push a, push b, greater ==  a > b. *)
VAR tos:INTEGER;
BEGIN
    tos := popInt();
    CASE oc OF
        Greater:      pushBool(popInt() > tos);
    |   Less:         pushBool(popInt() < tos);
    |   GreaterEqual: pushBool(popInt() >= tos);
    |   LessEqual:    pushBool(popInt() <= tos);
    ELSE
        fatal('compBool: unknown opcode');
    END;
END compBool;

                (*** I/O ***)

PROCEDURE readInt;
VAR i:INTEGER;
BEGIN
    ReadInt(i);
    pushInt(i);
END readInt;

PROCEDURE readChar;
VAR c:CHAR;
BEGIN
    Read(c);
    pushWord(WORD(c));
END readChar;

        (*** High Level Language support ***)

(* The PUSHL instruction takes a lexical level difference as a first argument
   and an offset from the FP as second.  It passes these to calculateEA, then
   pushes the word at the resulting address on the stack. *)
PROCEDURE pushL;
BEGIN
    pushWord(memory[calculateEA(memory[programCtr], memory[programCtr+1])]);
    INC(programCtr, 2);
END pushL;

(* POPL is like PUSHL, but pops the top of the stack into the address. *)
PROCEDURE popL;
BEGIN
    memory[calculateEA(memory[programCtr], memory[programCtr+1])] :=
popWord();
    INC(programCtr, 2);
END popL;


(* ADDRL is like PUSHL, but puts the addres on the stack, not its contents *)
PROCEDURE addrL;
BEGIN
    pushWord(calculateEA(memory[programCtr], memory[programCtr+1]));
    INC(programCtr, 2);
END addrL;

(* calculateEA calculates the effective address of a non-global variable,
given
   the number of static pointers to follow (= the difference in lexical
levels)
   and the offset of the variable within the activation record *)
PROCEDURE calculateEA(follow, offset:WORD):address;
BEGIN
    (* We have to add the two as integers, because the argument may be
       negative.  Then we convert back to an address. *)
    RETURN address(INTEGER(followSP(CARDINAL(follow))) + INTEGER(offset));
END calculateEA;

(* Follows the static pointer chain. *)
PROCEDURE followSP(num:CARDINAL):address;
VAR fp:address;
    n:CARDINAL;
BEGIN
    fp := framePtr;
    FOR n := 1 TO num DO
        fp := address(memory[fp + SPoffset]);
    END;
    RETURN fp;
END followSP;

(* CALL takes two arguments, the address to branch to and the difference in
   lexical levels.  It does the following things:
    1. Pushes the current FP
    2. Computes and pushes the SP
    3. Pushes the return address
    4. Branches to the address.
*)
PROCEDURE call;
BEGIN
    pushWord(WORD(framePtr));           (* save current FP *)
        (* use the difference in lexical levels (2nd arg) to set the SP *)
    pushWord(followSP(CARDINAL(memory[programCtr+1])));
    framePtr := stackPtr;                (* FP will point to return address *)
    pushWord(WORD(programCtr + 2));     (* return address *)
    branch;
END call;

PROCEDURE return;
BEGIN
    (* Set the stack pointer so that top of stack will be where it was
       before we pushed the first argument. The contents of the word of
       memory just after this instruction tell us how many words of args
       there are on the stack. The "+2" is for the words occupied by the

       SP and old FP.
    *)
    stackPtr := framePtr + address(memory[programCtr]) + 2;
    (* The FP points to the return address *)
    programCtr := address(memory[framePtr]);
    (* Restore the old FP *)
    framePtr := address(memory[framePtr + oldFPoffset]);
END return;

PROCEDURE fReturn;
VAR w:WORD;
BEGIN
    w := popWord();
    return;
    pushWord(w);
END fReturn;

            (*** Part 3 compiler extensions ***)

PROCEDURE copy;
(* Copies TOS words from TOS-2 to TOS-1 *)
VAR i,n:CARDINAL;
    source, dest:address;
BEGIN
    n := CARDINAL(popInt());
    dest := address(popWord());
    source := address(popWord());
    FOR i := 0 TO n-1 DO
        memory[dest+address(i)] := memory[source+address(i)];
    END;
END copy;

PROCEDURE min;
(* takes min of top two integers on stack *)
VAR i,j:INTEGER;
BEGIN
    i := popInt();
    j := popInt();
    IF i < j THEN
        pushInt(i);
    ELSE
        pushInt(j);
    END;
END min;

PROCEDURE setSP;
(* dec argument from stack pointer *)
BEGIN
    DEC(stackPtr, address(memory[programCtr]));
    INC(programCtr);
END setSP;

PROCEDURE aref;
(* Array indexing with bounds checking. Pushes address of indexed element. *)

VAR index, upper, lower, size, offset:INTEGER;
    starting:address;
BEGIN
    size := INTEGER(memory[programCtr]);
    INC(programCtr);
    index := popInt();
    upper := popInt();
    lower := popInt();
    starting := address(popWord());
    IF (index >= lower) AND (index <= upper) THEN
           (* go from index and size to actual offset in memory *)
        offset := (index-lower)*size;
           (* push address of element *)
        pushWord(starting + address(offset));
    ELSE
        error("array index out of bounds");
    END;
END aref;


        (* ------- Stack Operations -------- *)


PROCEDURE popInt():INTEGER;
BEGIN
    RETURN INTEGER(popWord());
END popInt;

PROCEDURE pushInt(i:INTEGER);
BEGIN
    pushWord(WORD(i));
END pushInt;

PROCEDURE pushBool(b:BOOLEAN);  (* used for boolean instructions *)
BEGIN
    IF b THEN
        pushInt(1);     (* 1 = TRUE *)
    ELSE
        pushInt(0);     (* 0 = FALSE *)
    END;
END pushBool;

PROCEDURE pushWord(w:WORD);
BEGIN
    IF stackPtr <= stackLimit THEN
        error("Stack blown");
    ELSE
        memory[stackPtr] := w;
        DEC(stackPtr);
    END;
END pushWord;

PROCEDURE popWord():WORD;
BEGIN

    IF stackPtr = maxMem THEN
        error("stack empty");
        RETURN WORD(0);
    ELSE
        INC(stackPtr);
        RETURN memory[stackPtr];
    END;
END popWord;

PROCEDURE flush;
BEGIN
    stackPtr := maxMem;
END flush;

PROCEDURE error(msg:ARRAY OF CHAR);
BEGIN
    WriteString(msg);
    WriteLn;
    errorFlag := TRUE;
END error;

BEGIN
    reset;
END VM2.
