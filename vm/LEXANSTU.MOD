IMPLEMENTATION MODULE LexAnStuff;

FROM MyTerminal IMPORT WriteString, WriteLnString, WriteLn, Write, WriteCard,
        fatal, spaces, places;
FROM InOut IMPORT Read, EOL;
FROM CharStuff IMPORT TAB, EOF, isAlphaNum, isDigit;


VAR charTable: ARRAY CHAR OF charProc;
    line: ARRAY[1..maxLineLength] OF CHAR;
    lineEnd, lineNum, linePos: CARDINAL;
    ungotten: BOOLEAN;


            (*** Accessing variables ***)

PROCEDURE getLine(VAR s:ARRAY OF CHAR);
(* Reads line into the string.  If the string is too short, we just truncate
   line. The last character of line is EOL or EOF; we remove it from the
   string. *)
VAR i:CARDINAL;
BEGIN
    FOR i := 1 TO lineEnd DO
        IF i-1 > HIGH(s) THEN
            RETURN;
        ELSE
            s[i-1] := line[i];
        END;
    END;
    s[lineEnd-1] := 0C;     (* replace the EOL or EOF with a 0C *)
END getLine;
PROCEDURE getLineNum():CARDINAL;
BEGIN
    RETURN lineNum;
END getLineNum;

PROCEDURE getLinePos():CARDINAL;
BEGIN
    RETURN linePos;
END getLinePos;

PROCEDURE writeLine;
CONST spacesBeforeLine = 5;
VAR s:ARRAY[1..maxLineLength] OF CHAR;
BEGIN
    getLine(s);
    WriteCard(lineNum, 0);
    Write(":");
    spaces(spacesBeforeLine - places(lineNum) - 1);
    WriteLnString(s);
    spaces(spacesBeforeLine + linePos - 2);
    Write('^');
    WriteLn;
END writeLine;


            (*** low-level input ***)

(* The following assertions hold for the low-level input procedures:
    - lineEnd always points to the last character on the line, except at
        the very beginning when it is zero.
    - linePos always points to the next character to be returned.
  Also, getChar will repeatedly return EOF at end of file.
*)

PROCEDURE getChar():CHAR;
BEGIN
    ungotten := FALSE;
    IF linePos > lineEnd THEN
        fillLine;
    END;
    INC(linePos);
    RETURN line[linePos-1];
END getChar;

PROCEDURE ungetChar;
BEGIN
    IF ungotten THEN
        fatal("ungetChar: can only unget one char at a time");
    ELSIF linePos = 1 THEN
        (* can only happen at beginning *)
        fatal("ungetChar: no characters read yet");
    ELSE
        DEC(linePos);        ungotten := TRUE;
    END;
END ungetChar;

PROCEDURE fillLine;
VAR ch:CHAR;
BEGIN
    lineEnd := 0;
    linePos := 1;
    REPEAT
        Read(ch);
        INC(lineEnd);
        line[lineEnd] := ch;
        IF (ch = EOF) OR (ch = EOL) THEN RETURN END;
    UNTIL lineEnd = maxLineLength - 1;
    INC(lineEnd);
    line[lineEnd] := EOL;
END fillLine;


(* reading procedures *)

PROCEDURE illegalChar(c:CHAR);
BEGIN
    writeLine;
    WriteLnString("illegal character");
    dispatch;
END illegalChar;

PROCEDURE ignore(c:CHAR);
BEGIN
    dispatch;
END ignore;

PROCEDURE posInteger(c:CHAR):INTEGER;
VAR n:INTEGER;
BEGIN
    n := 0;
    WHILE isDigit(c) DO
        n := 10*n + INTEGER(ORD(c) - ORD('0'));
        c := getChar();
    END;
    ungetChar;
    RETURN n;
END posInteger;

PROCEDURE integer(c:CHAR):INTEGER;
BEGIN
    IF c = '-' THEN
       RETURN -posInteger(getChar());
    ELSIF c = '+' THEN
        RETURN posInteger(getChar());
    ELSE
        RETURN posInteger(c);    END;
END integer;


PROCEDURE alphaNumString(c:CHAR; VAR s:ARRAY OF CHAR):BOOLEAN;
(* Returns TRUE if ok, FALSE if identifier too long. Doesn't skip identifier.*)
VAR i:CARDINAL;
BEGIN
    i := 0;
    WHILE (i < HIGH(s)) AND isAlphaNum(c) DO
        s[i] := c;
        INC(i);
        c := getChar();
    END;
    s[i] := 0C;
    ungetChar;
    RETURN NOT isAlphaNum(c);
END alphaNumString;

PROCEDURE string(delimiter:CHAR; VAR s:ARRAY OF CHAR):BOOLEAN;
(* Reads a string delimited by delimiter.  Backslash is an escape: t for tab,
   n for newline (carriage return), anything else taken literally.  Returns
   FALSE if s isn't long enough.  Doesn't skip rest of string. *)
VAR i:CARDINAL;
    c:CHAR;
BEGIN
    i := 0;
    c := getChar();
    WHILE (i < HIGH(s)) AND (c <> delimiter) DO
        IF c = '\' THEN
            c := getChar();
            IF c = 'n' THEN
                s[i] := EOL;
            ELSIF c = 't' THEN
                s[i] := TAB;
            ELSE
                s[i] := c;
            END;
        ELSE
            s[i] := c;
        END;
        INC(i);
        c := getChar();
    END;
    s[i] := 0C;
    IF c = delimiter THEN
        RETURN TRUE;
    ELSE
        ungetChar;
        RETURN FALSE;
    END;
END string;

                (*** reading utilities ***)
PROCEDURE skipAlphaNum;
(* Read over the input until an alphanumeric character is reached.  Unget it.*)
BEGIN
    REPEAT UNTIL NOT isAlphaNum(getChar());
    ungetChar;
END skipAlphaNum;

PROCEDURE skipToChar(c:CHAR);
(* Read over the input until c is reached.  Unget it. *)
VAR c1:CHAR;
BEGIN
    REPEAT
        c1 := getChar();
        IF c1 = EOL THEN
            INC(lineNum);
        ELSIF c1 = EOF THEN
            ungetChar;
            RETURN;
        END;
    UNTIL c = c1;
END skipToChar;

PROCEDURE skipToChars(clist:ARRAY OF CHAR);
(* Read over the input until any character in clist is found.  Unget it. *)
VAR c:CHAR;
BEGIN
    REPEAT
        c := getChar();
        IF c = EOL THEN
            INC(lineNum);
        ELSIF c = EOF THEN
            ungetChar;
            RETURN;
        END;
    UNTIL member(c, clist);
END skipToChars;

PROCEDURE member(c:CHAR; clist:ARRAY OF CHAR):BOOLEAN;
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(clist) DO
        IF c = clist[i] THEN
            RETURN TRUE;
        END;
    END;
    RETURN FALSE;
END member;

                (*** character table ***)

PROCEDURE dispatch;
VAR c:CHAR;
BEGIN    c := getChar();
    IF c = EOL THEN
        INC(lineNum);       (* keep track of current line number *)
    END;
    charTable[c](c);        (* call the procedure associated with c *)
END dispatch;

PROCEDURE enterAll(cp:charProc);
VAR c:CHAR;
BEGIN
    FOR c := CHR(0) TO CHR(maxCharNum) DO
        charTable[c] := cp;
    END;
END enterAll;

PROCEDURE enterWhite(cp:charProc);
BEGIN
    charTable[' '] := cp;
    charTable[TAB] := cp;
    charTable[EOL] := cp;
END enterWhite;

PROCEDURE enterChar(c:CHAR; cp:charProc);
BEGIN
    charTable[c] := cp;
END enterChar;

PROCEDURE enterAlphas(cp:charProc);
VAR c:CHAR;
BEGIN
    FOR c := 'A' TO 'Z' DO
        charTable[c] := cp;
    END;
    FOR c := 'a' TO 'z' DO
        charTable[c] := cp;
    END;
END enterAlphas;

PROCEDURE enterDigits(cp:charProc);
VAR c:CHAR;
BEGIN
    FOR c := '0' TO '9' DO
        charTable[c] := cp;
    END;
END enterDigits;


PROCEDURE enterEndOfFile(cp:charProc);
BEGIN
    charTable[EOF] := cp;
END enterEndOfFile;

(* initialization of charTable *)
PROCEDURE initCharTable;
VAR c:CHAR;
BEGIN
    enterAll(illegalChar);
    enterWhite(ignore);
END initCharTable;




BEGIN
    initCharTable;
    lineNum := 1;
    lineEnd := 0;
    linePos := 1;
    ungotten := FALSE;
END LexAnStuff.
