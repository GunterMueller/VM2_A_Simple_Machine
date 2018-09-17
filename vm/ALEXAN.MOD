IMPLEMENTATION MODULE ALexAn;

(* The lexical analyzer for the VM2 assembler.  It uses many routines from
   LexAnStuff. It skips over comments.  It recognizes labels, directives,
   identifiers, integers, characters and strings.  An identifier may be a label
   name or an opcode; the lexical analyzer doesn't decide that. *)

FROM MyTerminal IMPORT WriteLnString;
FROM InOut IMPORT EOL;
FROM LexAnStuff IMPORT enterAlphas, enterDigits, enterChar, integer,
    alphaNumString, dispatch, enterEndOfFile, skipToChar, skipAlphaNum,
    getChar, ungetChar, writeLine, string;
FROM StringStuff IMPORT stringLen, stringCap;

VAR
    tok: token;

PROCEDURE getToken(VAR t:token);
BEGIN
    dispatch;
    t := tok;
END getToken;


PROCEDURE comment(c:CHAR);  (* goes on semicolon; skips to end of line *)
BEGIN
    skipToChar(EOL);
    dispatch;
END comment;


PROCEDURE comma(c:CHAR);
BEGIN
    tok.class := Comma;
END comma;

PROCEDURE endOfFile(c:CHAR);
BEGIN
    tok.class := EndOfFile;
END endOfFile;

PROCEDURE labelOrId(c:CHAR);        (* goes on letters *)
BEGIN
    IF NOT alphaNumString(c, tok.string) THEN
        writeLine;
        WriteLnString("String too long");
        skipAlphaNum;
    END;
    c := getChar();
    IF c = ':' THEN
        tok.class := Label;
    ELSE
        ungetChar;        tok.class := Identifier;
    END;
    stringCap(tok.string);
END labelOrId;

PROCEDURE directive(c:CHAR);    (* goes on dot *)
BEGIN
    dispatch;
    IF tok.class <> Identifier THEN
        writeLine;
        WriteLnString("Illegal directive");
    ELSE
        tok.class := Directive;
    END;
END directive;

PROCEDURE int(c:CHAR);          (* goes on digits *)
BEGIN
    tok.int := integer(c);
    tok.class := Integer;
END int;

PROCEDURE character(c:CHAR);    (* goes on single quote *)
BEGIN
    tok.char := getChar();
    tok.class := Char;
END character;

PROCEDURE stringProc(c:CHAR);       (* goes on double quote *)
BEGIN
    IF NOT string(c, tok.string) THEN
        writeLine;
        WriteLnString("string too long");
        skipToChar(c);      (* skip to the closing quote mark... *)
        c := getChar();     (* and get rid of it. *)
    END;
    tok.class := String;
END stringProc;

PROCEDURE lineEnd(c:CHAR);
BEGIN
    tok.class := EndOfLine;
END lineEnd;


(* initialization of charTable *)

PROCEDURE initCharTable;
VAR c:CHAR;
BEGIN
    enterAlphas(labelOrId);
    enterDigits(int);
    enterChar('-', int);
    enterChar('+', int);    enterChar('.', directive);
    enterChar(';', comment);
    enterChar(',', comma);
    enterChar("'", character);
    enterChar('"', stringProc);
    enterChar(EOL, lineEnd);
    enterEndOfFile(endOfFile);
END initCharTable;


BEGIN
    initCharTable;
END ALexAn.
