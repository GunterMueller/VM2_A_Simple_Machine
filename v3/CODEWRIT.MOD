IMPLEMENTATION MODULE CodeWrite;
      
FROM InOut IMPORT WriteString, WriteLn, WriteInt, WriteCard;
        (* We can't use Write because of a conflict inside tokenClass *)
IMPORT InOut;
FROM Symbol IMPORT symbol, Class, modeType;
IMPORT Symbol;
FROM SymbolTable IMPORT currentLexLevel;
FROM Token IMPORT tokenClass, stringType;
IMPORT MyTerminal;

PROCEDURE writeStringLabel(s:ARRAY OF CHAR);
BEGIN
    WriteString(s);
    WriteString(': ');
END writeStringLabel;

PROCEDURE writeRoutineLabel(routine:symbol);
VAR name:stringType;
BEGIN
    writeRoutineName(routine);
    WriteString(': ');
    WriteLn;
    Symbol.string(routine, name);
    MyTerminal.WriteString(name);
    MyTerminal.WriteLnString("...");
END writeRoutineLabel;

PROCEDURE writeRoutineName(routine:symbol);
VAR name:stringType;
BEGIN
    Symbol.string(routine, name);
    WriteString(name);
    IF Symbol.lexLevel(routine) <> 0 THEN
        WriteInt(Symbol.offset(routine), 0);
    END;
END writeRoutineName;

PROCEDURE writeLabel(c:CARDINAL);
BEGIN
    InOut.Write('L');
    WriteCard(c, 0);
    InOut.Write(':');
    WriteLn;
END writeLabel;

PROCEDURE writeStringBranch(s:ARRAY OF CHAR);
BEGIN
    writeOpCode('BRANCH  ');
    WriteLnString(s);
END writeStringBranch;

PROCEDURE writeBranch(c:CARDINAL);
BEGIN
    writeOpCode('BRANCH  L');
    WriteCard(c, 0);
    WriteLn;
END writeBranch;
    
PROCEDURE writeCondBranch(tc:tokenClass; c:CARDINAL);
BEGIN
    CASE tc OF
        Equal:  writeOpCode('BREQL   L');
    |   Less:   writeOpCode('BRLSS   L');
    |   Greater:writeOpCode('BRGTR   L');
    ELSE
        MyTerminal.fatal('writeCondBranch: unknown branch type');
    END;
    WriteCard(c, 0);
    WriteLn;
END writeCondBranch;

PROCEDURE writeWriteInt;
BEGIN
    writeOpCode('WRINT');
    WriteLn;
END writeWriteInt;

PROCEDURE writeReadInt;
BEGIN
    writeOpCode('RDINT');
    WriteLn;
END writeReadInt;

PROCEDURE writeWriteChar;
BEGIN
    writeOpCode('WRCHAR');
    WriteLn;
END writeWriteChar;

PROCEDURE writeReadChar;
BEGIN
    writeOpCode('RDCHAR');
    WriteLn;
END writeReadChar;

PROCEDURE writeHalt;
BEGIN
    writeOpCode('HALT');
    WriteLn;
END writeHalt;

PROCEDURE writeReturn(numFormals:CARDINAL);
BEGIN
    writeOpCode('RETURN  ');
    WriteCard(numFormals, 0);
    WriteLn;
END writeReturn;

PROCEDURE writeFReturn(numFormals:CARDINAL);
BEGIN
    writeOpCode('FRETURN ');
    WriteCard(numFormals, 0);
    WriteLn;
END writeFReturn;

PROCEDURE writeInt(i:INTEGER);
BEGIN
    writeOpCode('PUSHC   ');
    WriteInt(i, 0);
    WriteLn;
END writeInt;

PROCEDURE writeChar(c:CHAR);
BEGIN
    writeOpCode('PUSHC   ');
    InOut.Write("'");
    InOut.Write(c);
    WriteLn;
END writeChar;

PROCEDURE writeBool(b:BOOLEAN);
BEGIN
    IF b THEN
        writeInt(1);
    ELSE
        writeInt(0);
    END;
END writeBool;

PROCEDURE writeSymPop(s:symbol);
(* Not defined on array symbols. *)
BEGIN
    writeSymAddr(s, "POPC    ", "POPL    ", "PUSHL   ");
    IF Symbol.class(s) = Formal THEN
        writePop;
    END;
END writeSymPop;

PROCEDURE writeSymbol(s:symbol);
(* Put the contents of the symbol on the stack.  Not defined for
   array symbols. *)
BEGIN
    writeSymAddr(s, "PUSH    ", "PUSHL   ", "PUSHL   ");
    IF (Symbol.class(s) = Formal) AND (Symbol.mode(s) = mInOut) THEN
        writeContents;
    END;
END writeSymbol;

PROCEDURE writeAddr(s:symbol);
(* Put the address of the symbol on the stack. Rules:
   array:
     global: pushc;
     local:  addrl;
     formal: pushl;
   scalar:
     global: pushc;
     local:  addrl;
     formal: IN: addrl; OUT, IN OUT; pushl
*)
BEGIN
    IF (Symbol.class(Symbol.type(s)) = ScalarType) AND
       (Symbol.class(s) = Formal) AND 
       (Symbol.mode(s) = mIn) THEN
        writeSymAddr(s, "dummy", "dummy", "ADDRL   ");
    ELSE
       writeSymAddr(s, "PUSHC   ", "ADDRL   ", "PUSHL   ");
    END;
END writeAddr;

PROCEDURE writeSymAddr(s:symbol; global, local, formal:ARRAY OF CHAR);
VAR name:stringType;
BEGIN
    Symbol.string(s, name);
    IF Symbol.class(s) = Global THEN
        writeOpCode(global);
        WriteLnString(name);
    ELSE
        IF Symbol.class(s) = Local THEN
            writeOpCode(local);
        ELSIF Symbol.class(s) = Formal THEN
            writeOpCode(formal);
        ELSE
            MyTerminal.fatal('writeSymAddr: not variable');
        END;
        WriteInt(currentLexLevel() - Symbol.lexLevel(s), 0);
        WriteString(', ');
        WriteInt(Symbol.offset(s), 0);
        writeComment(name);
    END;
END writeSymAddr;

PROCEDURE writeCall(s:symbol);
BEGIN
    writeOpCode("CALL    ");
    writeRoutineName(s);
    WriteString(", ");
    WriteInt(currentLexLevel() - Symbol.lexLevel(s), 0);
    WriteLn;
END writeCall;

PROCEDURE writeOp(tc:tokenClass);
BEGIN
    CASE tc OF
        Plus:         writeOpCode('ADD');
    |   Minus:        writeOpCode('SUB');
    |   UMinus:       writeOpCode('NEG');
    |   Times:        writeOpCode('MUL');
    |   Divide:       writeOpCode('DIV');
    |   Not:          writeOpCode('NOT');
    |   Equal:        writeOpCode('EQUAL');
    |   Greater:      writeOpCode('GREATER');
    |   Less:         writeOpCode('LESS');
    |   NotEqual:     writeOpCode('NOTEQL');
    |   LessEqual:    writeOpCode('LSSEQL');
    |   GreaterEqual: writeOpCode('GTREQL');
    ELSE MyTerminal.fatal("writeOp: unknown operator");
    END;
    WriteLn;
END writeOp;

PROCEDURE WriteLnString(s:ARRAY OF CHAR);
BEGIN
    WriteString(s);
    WriteLn;
END WriteLnString;

PROCEDURE writeOpCode(s:ARRAY OF CHAR);
BEGIN
    WriteString("   ");
    WriteString(s);
END writeOpCode;

PROCEDURE writeComment(s:ARRAY OF CHAR);
BEGIN
    WriteString("    ; ");
    WriteString(s);
    WriteLn;
END writeComment;


PROCEDURE writeCopy;
BEGIN
    writeOpCode("COPY");
    WriteLn;
END writeCopy;

PROCEDURE writeMin;
BEGIN
    writeOpCode("MIN");
    WriteLn;
END writeMin;

PROCEDURE writePop;
BEGIN
    writeOpCode("POP");
    WriteLn;
END writePop;

PROCEDURE writeContents;
BEGIN
    writeOpCode("CONTENTS");
    WriteLn;
END writeContents;

PROCEDURE writeLow(s:symbol);
(* stack looks like this:
   | highBound |
   |  lowBound |
   | startaddr | <-- offset
*)
BEGIN
    writeOpCode("PUSHL   ");
    WriteInt(currentLexLevel() - Symbol.lexLevel(s), 0);
    WriteString(', ');
    WriteInt(Symbol.offset(s)+1, 0);
    writeComment("LOW");
END writeLow;

PROCEDURE writeHigh(s:symbol);
(* stack looks like this:
   | highBound |
   |  lowBound |
   | startaddr | <-- offset
*)
BEGIN
    writeOpCode("PUSHL   ");
    WriteInt(currentLexLevel() - Symbol.lexLevel(s), 0);
    WriteString(', ');
    WriteInt(Symbol.offset(s)+2, 0);
    writeComment("HIGH");
END writeHigh;

PROCEDURE writeSetSP(i:INTEGER);
BEGIN
    writeOpCode("SETSP   ");
    WriteInt(i, 0);
    WriteLn;
END writeSetSP;

PROCEDURE writeAref(size:CARDINAL);
BEGIN
    writeOpCode("AREF    ");
    WriteCard(size, 0);
    WriteLn;
END writeAref;

MODULE Strings;     (* for handling string constants *)
FROM MyTerminal IMPORT fatal;
FROM InOut IMPORT Write, EOL;
IMPORT  writeLabel, WriteString, WriteLn, WriteCard, stringType,
    writeOpCode, writeComment;
EXPORT writeStrings, recordString;

    CONST   maxStrings = 20; (* max string consts per routine *)
    TYPE stringRec = RECORD
                        string:stringType;
                        label:CARDINAL;
                     END;
    VAR strings: ARRAY[1..maxStrings] OF stringRec;
        nStrings:[0..maxStrings];

    PROCEDURE recordString(lab:CARDINAL; VAR s:stringType);
    (* record the string in the array and write code to push its address *)
    BEGIN
        IF nStrings = maxStrings THEN
            fatal('too many strings in routine');
        ELSE
            INC(nStrings);
            WITH strings[nStrings] DO
                label := lab;
                string := s;
            END;
            writeOpCode('PUSHC   L');
            WriteCard(lab, 0);
            WriteLn;
        END;
    END recordString;

    PROCEDURE writeStrings;
    VAR i:CARDINAL;
    BEGIN
        FOR i := 1 TO nStrings DO
            writeLabel(strings[i].label);
            WriteString('   "');
            formatString(strings[i].string);
            WriteString('" ');
            WriteCard(0, 0);
            WriteLn;
        END;
        nStrings := 0;
    END writeStrings;

    PROCEDURE formatString(VAR s:ARRAY OF CHAR);
    (* puts slashes back in *)
    CONST Tab = 11C;
    VAR i:CARDINAL;
    BEGIN
        i := 0;
        WHILE (i <= HIGH(s)) AND (s[i] <> 0C) DO
            IF s[i] = '"' THEN
                WriteString('\"');
            ELSIF s[i] = EOL THEN
                WriteString("\n");
            ELSIF s[i] = Tab THEN
                WriteString("\t");
            ELSIF s[i] = "\" THEN
                WriteString("\\");
            ELSE
                Write(s[i]);
            END;
            INC(i);
        END;
    END formatString;

BEGIN (* module Strings *)
    nStrings := 0;
END Strings;
    


BEGIN
END CodeWrite. 
