
MODULE Assembler;

(* An assembler for the VM2 virtual machine, as described in "A VM2 Assembler".
   By Jonathan Amsterdam. *)

FROM VM2Defs IMPORT opCode, address, maxMem;
FROM OpNames IMPORT opType, nameToOpCode;
FROM LabelTable IMPORT defType, insert, lookup, map;
FROM ALexAn IMPORT getToken, token, tokenClass;
FROM LexAnStuff IMPORT writeLine;
FROM StringStuff IMPORT stringLen, stringCap, stringEqual;
FROM InOut IMPORT WriteString, WriteLn, WriteInt, WriteCard,
        OpenInput, OpenOutput,
        CloseInput, CloseOutput, Done;
FROM MyTerminal IMPORT fatal, WriteLnString, pause, ClearScreen;

CONST
    sourceExt   = 'ASM';    (* Default extensions for files *)
    binaryExt   = 'BIN';


VAR ppc:address;    (* The "pseudo program counter": keeps track of the
                       current location. *)
    mem: ARRAY address OF INTEGER;  (* The program array *)
    nErrors:CARDINAL;           (* number of errors encountered *)


PROCEDURE openSource;
BEGIN
    OpenInput(sourceExt);
    IF NOT Done THEN
        fatal("Can't open file");
    END;
END openSource;

PROCEDURE openBinary;
BEGIN
    OpenOutput(binaryExt);
END openBinary;

PROCEDURE assemble;
VAR t:token;
BEGIN
    ppc := 0;
    nErrors := 0;
    getToken(t);
    WHILE t.class <> EndOfFile DO
        CASE t.class OF
            EndOfLine:  (* do nothing *)
        |   Directive:
                IF stringEqual(t.string, "BLOCK") THEN                    blockDir;
                ELSE
                    error("Unknown directive");
                END;
        |   Label:
                handleLabel(t.string);
        |   Identifier:
                handleOpcode(t.string);
        |   Char:
                mem[ppc] := INTEGER(t.char); INC(ppc);
        |   Integer:
                mem[ppc] := t.int; INC(ppc);
        |   String:
                handleString(t.string);
        ELSE
            error("Illegal token");
        END;
        getToken(t);
    END;
END assemble;

        (* directives *)

PROCEDURE blockDir;
VAR i:CARDINAL;
    t:token;
BEGIN
    getToken(t);
    IF t.class <> Integer THEN
        error("integer expected");
    ELSIF t.int < 0 THEN
        error("argument to BLOCK must be positive");
    ELSE
        FOR i := ppc TO ppc + CARDINAL(t.int) DO
            mem[i] := 0;
        END;
        INC(ppc, t.int);
    END;
END blockDir;


                     (*** labels ***)

PROCEDURE handleLabel(VAR label:ARRAY OF CHAR);
VAR a:address;
    d:defType;
BEGIN
   IF lookup(label, a, d) THEN  (* label found *)
        IF d = Defined THEN
            error("Doubly defined label");
        ELSIF NOT insert(label, ppc, Defined) THEN
            fatal("label table full");
        ELSE
            backpatch(a);   (* label found but undefined, so backpatch *)        END;
    ELSIF NOT insert(label, ppc, Defined) THEN
        fatal("label table full");
    END;
END handleLabel;

PROCEDURE backpatch(start:address);
(* We have found the definition of a label.  Go through the linked list of
   references to that label, inserting the value of that label (which is
   ppc). *)
VAR a, temp:address;
BEGIN
    a := start;
    WHILE a <> maxMem DO    (* maxMem marks end of list *)
        temp := mem[a];
        mem[a] := ppc;
        a := temp;
    END;
END backpatch;


                (*** opcodes and arguments ***)

PROCEDURE handleOpcode(VAR name:ARRAY OF CHAR);
VAR code:opCode;
    type:opType;
    t:token;
BEGIN
    type := nameToOpCode(name, code);
    IF type = NotFound THEN
        error("Unknown opcode");
    ELSE
        mem[ppc] := INTEGER(code);
        INC(ppc);
        IF (type = OneArg) OR (type = TwoArgs) THEN
            handleArg;
            IF type = TwoArgs THEN
                getToken(t);
                IF t.class <> Comma THEN
                    error("Comma expected");
                END;
                handleArg;
            END;
        END;
    END;
END handleOpcode;

PROCEDURE handleArg;
VAR a:address;
    i:INTEGER;
    d:defType;
    dummy:BOOLEAN;
    t:token;
BEGIN    getToken(t);
    CASE t.class OF
        EndOfLine:
            error("Missing argument"); (* argument must appear on same line *)
    |   EndOfFile:
            fatal("Unexpected end of file");
    |   Char:
            i := INTEGER(t.char);
    |   Integer:
            i := INTEGER(t.int);
    |   Identifier:
            IF lookup(t.string, a, d) THEN
                i := INTEGER(a);
                IF d = Undefined THEN  (* add to linked list of forward refs *)
                    dummy := insert(t.string, ppc, Undefined);
                END;
            ELSE    (* begin linked list of forward references *)
                i := maxMem;
                dummy := insert(t.string, ppc, Undefined);
            END;
    ELSE
        error("Illegal token for argument");
    END;
    mem[ppc] := i;
    INC(ppc);
END handleArg;


                (*** miscellaneous procedures ***)

PROCEDURE handleString(VAR s:ARRAY OF CHAR);
(* Put the characters of the string into consecutive memory locations. *)
VAR i:CARDINAL;
BEGIN
    FOR i := 1 TO stringLen(s) DO
        mem[ppc] := INTEGER(s[i-1]);
        INC(ppc);
    END;
END handleString;


PROCEDURE outputMem;        (* final output *)
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO ppc-1 DO
        WriteInt(mem[i], 0);
        WriteLn;
    END;
END outputMem;


PROCEDURE error(msg:ARRAY OF CHAR);     (* handling error messages *)
BEGIN
    writeLine;    WriteLnString(msg);
    INC(nErrors);
END error;

(* procedure for catching undefined labels *)
PROCEDURE undefLabelCheck(VAR label:ARRAY OF CHAR; a:address; d:defType);
BEGIN
    IF d = Undefined THEN
        WriteLnString("Undefined label:");
        WriteLnString(label);
        INC(nErrors);
    END;
END undefLabelCheck;

PROCEDURE wrapup;       (* handle stuff after assembly *)
BEGIN
    CloseInput;
    map(undefLabelCheck);   (* check every label to see if it's undefined *)
    IF nErrors = 0 THEN     (* output the code if there are no errors *)
        outputMem;
    END;
    CloseOutput;
    IF nErrors = 0 THEN
        WriteString("No");
    ELSE
        WriteCard(nErrors, 0);
    END;
    WriteLnString(" errors detected.");
END wrapup;

BEGIN               (* main program *)
    ClearScreen;
    openSource;
    openBinary;
    assemble;
    wrapup;
    pause('Hit any key to exit:');
END Assembler.
