MODULE Monitor;

(* This is the monitor program for VM2.  It allows you to access the
   virtual machine, read and change registers and memory locations, etc.  See
   the "help" procedure for a list of commands. *)

FROM VM2Defs IMPORT maxMem, address, opCode;
FROM VM2 IMPORT putWord, getWord, run, singleStep, getProgramCtr,
    setProgramCtr, getFramePtr, getStackPtr, getStackLimit, setStackLimit,
    reset;
FROM OpNames IMPORT opType, opCodeToName, nameToOpCode;
FROM InOut IMPORT Read, WriteString, WriteLn, WriteInt, WriteCard, ReadInt,
    ReadCard, ReadString, Write, OpenInput, OpenOutput, Done, CloseInput,
    CloseOutput;
FROM SYSTEM IMPORT WORD;
FROM Terminal IMPORT ClearScreen, Beep;

CONST
    CR = 15C;
    BS = 10C;
    TAB = 11C;
    tabsize = 10;

TYPE modeType = (Int, Op, Addr);
 (* this controls the way memory locations are interpreted: as integers,
    opcodes, or addresses. *)

VAR curAddress: address;        (* current address *)
    mode: modeType;             (* the mode *)
    highestOpCode:CARDINAL;

PROCEDURE comLoop;
VAR ch:CHAR;
BEGIN
    LOOP
        writeAddress(curAddress);
        Write(">");
        Read(ch);
        IF (ch <> CR) AND (ch <> BS) THEN
            Write(ch);
        END;
        CASE ch OF
             CR: dispAdvance(curAddress, mode);
        |    BS: IF curAddress <> 0 THEN
                    display(curAddress); DEC(curAddress);
                 ELSE Beep END;
        |   TAB: doSingleStep;
        |   '\': dispAdvance(curAddress, Int);
        |   '!': store;
        |   '.': display(curAddress);
        |   '%': registers;
        |   'a': mode := Addr; WriteLn;
        |   'c': curAddress := readAddress();
        |   'd': disp;

        |   'h': help;
        |   'i': mode := Int; WriteLn;
        |   'l': load;
        |   'o': mode := Op; WriteLn;
        |   'p': setProgramCtr(readAddress());
        |   'q': EXIT;
        |   'r': run(curAddress);
        |   's': showStack;
        |   't': setStackLimit(readAddress());
        |   'w': write;
        |   'z': reset;
        ELSE
            WriteString("unknown command"); WriteLn;
        END;
    END;
END comLoop;

PROCEDURE help;
BEGIN
    WriteLn;
    com('CR', 'display and advance current address');
    com('BS', 'display and move current address back one');
    com('TAB', 'single step');
    com('\ ', 'display as integer and advance current address');
    com('!<a/o/i>', 'store value at current address');
    com('% ', 'processor registers');
    com('. ', 'display contents at current address');
    com('a ', 'address mode');
    com('c<addr>', 'set current address');
    com('d<addr>', 'display contents at <addr>');
    com('h ', 'help');
    com('i ', 'integer mode');
    com('l ', 'load a binary file');
    com('o ', 'opcode mode');
    com('p<addr>', 'set program counter');
    com('q ', 'quit');
    com('r ', 'run from current address');
    com('s<card>', 'show stack to depth <card> (0 means all)');
    com('t<addr>', 'set stackLimit');
    com('w ', 'write memory to a binary file');
    com('z ', 'reset VM2');
END help;

PROCEDURE com(s1, s2:ARRAY OF CHAR);
BEGIN
    WriteString(s1);
    spaces(tabsize - HIGH(s1) - 1);
    WriteString(s2);
    WriteLn;
END com;


PROCEDURE spaces(c:CARDINAL);
VAR c2:CARDINAL;

BEGIN
    FOR c2 := 1 TO c DO
        Write(' ');
    END;
END spaces;

PROCEDURE dispAdvance(a:address; m:modeType);
VAR temp:modeType;
BEGIN
    IF curAddress <> maxMem THEN
        temp := mode;
        mode := m;
        display(curAddress);
        INC(curAddress);
        mode := temp;
    ELSE
        Beep;
    END;
END dispAdvance;

PROCEDURE display(a:address);
VAR w:WORD;
    s: ARRAY[1..10] OF CHAR;
    ot:opType;
BEGIN
    w := getWord(a);
    CASE mode OF
        Int: WriteInt(INTEGER(w), 0);
    |   Op: IF CARDINAL(w) > highestOpCode THEN
                WriteString("???");
            ELSE
                ot := opCodeToName(opCode(w), s);
                WriteString(s);
            END;
    |   Addr: IF CARDINAL(w) > maxMem THEN
                WriteString("xxx");
              ELSE
                writeAddress(address(w));
              END;
    END;
    WriteLn;
END display;

PROCEDURE disp;
BEGIN
    display(readAddress());
END disp;

PROCEDURE store;
BEGIN
    putWord(readWordInMode(), curAddress);
END store;

PROCEDURE showStack;

VAR depth:CARDINAL;
    a, tos:address;

BEGIN
    ReadCard(depth);
    WriteLn;
    tos := getStackPtr();
    IF tos = maxMem THEN
        WriteString("stack empty"); WriteLn;
        RETURN;
    END;
    INC(tos);
    IF (depth = 0) OR (CARDINAL(tos) + depth > maxMem) THEN
        (* show the whole stack *)
        FOR a := tos TO maxMem DO
            display(a);
        END;
    ELSE
        FOR a := tos TO tos + depth DO
            display(a);
        END;
    END;
END showStack;

PROCEDURE registers;

    PROCEDURE reg(s:ARRAY OF CHAR; a:address);
    BEGIN
        WriteString(s);
        spaces(3);
        writeAddress(a);
        WriteLn;
    END reg;

BEGIN (* registers *)
    WriteLn;
    reg('PC', getProgramCtr());
    reg('SP', getStackPtr());
    reg('FP', getFramePtr());
    WriteString("stackLimit = ");
    writeAddress(getStackLimit());
    WriteLn;
END registers;

PROCEDURE doSingleStep;
VAR m:modeType;
    pc:address;
BEGIN
    pc := getProgramCtr();
    writeAddress(pc);
    WriteString(": ");
    m := mode;      (* switch mode to Op and display the instruction *)
    mode := Op;
    display(pc);

    mode := m;
    singleStep;
END doSingleStep;

PROCEDURE load;
(* Load a "binary" file (really, a test file containing integers).  This is
   used in conjunction with the assembler. It sets stackLimit to point just
   after the end of the file in memory. *)
VAR i:INTEGER;
    a:address;
BEGIN
    OpenInput('BIN');
    a := 0;
    LOOP
        ReadInt(i);
        IF NOT Done THEN EXIT END;
        putWord(WORD(i), a);
        INC(a);
    END;
    setStackLimit(a);
    CloseInput;
END load;

PROCEDURE write;
(* write a binary file. *)
VAR a, from, to:address;
BEGIN
    WriteString("From: ");
    ReadCard(from); WriteLn;
    WriteString("To: ");
    ReadCard(to); WriteLn;
    OpenOutput('BIN');
    FOR a := from TO to DO
        WriteInt(INTEGER(getWord(a)), 0);
        WriteLn;
    END;
    CloseOutput;
END write;

(* ---- *)

PROCEDURE writeAddress(a:address);
BEGIN
    WriteCard(CARDINAL(a), 0);
END writeAddress;

PROCEDURE readAddress():address;
VAR c:CARDINAL;
BEGIN
    LOOP
        ReadCard(c); WriteLn;
        IF c <= maxMem THEN
            RETURN address(c);
        ELSE

            WriteString("address too large--reenter: ");
        END;
    END;
END readAddress;


PROCEDURE readOpCode():opCode;
(* Read in a string and convert it to an opcode. *)
VAR s:ARRAY[1..10] OF CHAR;
    oc:opCode;
BEGIN
    LOOP
        ReadString(s);
        WriteLn;
        IF nameToOpCode(s, oc) = NotFound THEN
            WriteString("unknown opcode--reenter: ");
        ELSE
            RETURN oc;
        END;
    END;
END readOpCode;

PROCEDURE readWordInMode():WORD;
VAR i:INTEGER;
BEGIN
    CASE mode OF
        Op: RETURN WORD(readOpCode());
    |   Int: ReadInt(i);
             WriteLn;
             RETURN WORD(i);
    |   Addr: RETURN WORD(readAddress());
    END;
END readWordInMode;



PROCEDURE computeHighestOpCode;
(* This is just a hack to figure out what the highest opcode value is. *)
VAR ocs:ARRAY opCode OF CHAR;
BEGIN
    highestOpCode := HIGH(ocs);
END computeHighestOpCode;

BEGIN (* main program *)
    curAddress := 0;
    mode := Op;
    computeHighestOpCode;
    ClearScreen;
    WriteString("VM2 Monitor V1.2"); WriteLn;
    comLoop;
END Monitor.
