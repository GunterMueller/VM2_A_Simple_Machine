IMPLEMENTATION MODULE CodeWrite;
      
FROM InOut IMPORT WriteString, WriteLn, WriteInt, WriteCard;
        (* We can't use Write because of a conflict inside tokenClass *)
FROM Symbol IMPORT symbol, scopeType, symbolString, symbolScope, 
    symbolOffset;
FROM Token IMPORT tokenClass, stringType;
IMPORT MyTerminal;

PROCEDURE writeStringLabel(s:ARRAY OF CHAR);
BEGIN
    WriteString(s);
    WriteString(': ');
END writeStringLabel;

PROCEDURE writeRoutineLabel(s:ARRAY OF CHAR);
BEGIN
    writeStringLabel(s);
    WriteLn;
    MyTerminal.WriteString(s);
    MyTerminal.WriteLnString("...");
END writeRoutineLabel;

PROCEDURE writeLabel(c:CARDINAL);
BEGIN
    WriteChar('L');
    WriteCard(c, 0);
    WriteChar(':');
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
(* unimplemented *)
END writeReturn;

PROCEDURE writeFReturn(numFormals:CARDINAL);
BEGIN
    (* unimplemented *)
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
    WriteChar("'");
    WriteChar(c);
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

PROCEDURE writePop(s:symbol);
BEGIN
    writeSymAddr(s, 'POPC    ', '');
END writePop;

PROCEDURE writeCall(s:symbol);
BEGIN
   (* unimplemented *)
END writeCall;

PROCEDURE writeSymbol(s:symbol);
BEGIN
    writeSymAddr(s, "PUSH    ", "");
END writeSymbol;

PROCEDURE writeSymAddr(s:symbol; global, local:ARRAY OF CHAR);
VAR name:stringType;
BEGIN
    IF symbolScope(s) = Global THEN
        symbolString(s, name);
        writeOpCode(global);
        WriteLnString(name);
    ELSE
        writeOpCode(local);
        WriteInt(symbolOffset(s), 0);
        symbolString(s, name);
        writeComment(name);
    END;
END writeSymAddr;

PROCEDURE writeOp(tc:tokenClass);
BEGIN
    CASE tc OF
        Plus:   writeOpCode('ADD');
    |   Minus:  writeOpCode('SUB');
    |   UMinus: writeOpCode('NEG');
    |   Times:  writeOpCode('MUL');
    |   Divide: writeOpCode('DIV');
    |   Not:    writeOpCode('NOT');
    |   Equal:  writeOpCode('EQUAL');
    |   Greater:writeOpCode('GREATER');
    |   Less:   writeOpCode('LESS');
    |   NotEqual:   writeOpCode('NOTEQL');
    |   LessEqual:  writeOpCode('LSSEQL');
    |   GreaterEqual:   writeOpCode('GTREQL');
    ELSE MyTerminal.fatal("writeOp: unknown operator");
    END;
    WriteLn;
END writeOp;

PROCEDURE WriteLnString(s:ARRAY OF CHAR);
BEGIN
    WriteString(s);
    WriteLn;
END WriteLnString;

PROCEDURE WriteChar(c:CHAR);
(* can't use InOut.Write, because the name conflicts with the Write in
   tokenClass. *)
VAR s:ARRAY[0..1] OF CHAR;
BEGIN
    s[0] := c;
    s[1] := 0C;
    WriteString(s);
END WriteChar;

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

BEGIN
END CodeWrite. 
