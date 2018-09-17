IMPLEMENTATION MODULE MyTerminal;

(* Some small but useful additions to the Terminal module. *)

IMPORT Terminal;

VAR powerOfTen: ARRAY[0..4] OF CARDINAL;


PROCEDURE WriteLnString(s:ARRAY OF CHAR);
BEGIN
    Terminal.WriteString(s);
    Terminal.WriteLn;
END WriteLnString;

PROCEDURE WriteInt(i:INTEGER; spaces:CARDINAL);
BEGIN
    IF i < 0 THEN
        IF spaces <> 0 THEN
            writeNum(CARDINAL(-i), spaces-1, TRUE);

        ELSE
            writeNum(CARDINAL(-i), 0, TRUE);
        END;
    ELSE
        writeNum(CARDINAL(i), spaces, FALSE);
    END;
END WriteInt;

PROCEDURE WriteCard(c, spaces:CARDINAL);
BEGIN
    writeNum(c, spaces, FALSE);
END WriteCard;

PROCEDURE writeNum(c, spaces:CARDINAL; neg:BOOLEAN);
VAR p:CARDINAL;
    i:INTEGER;
BEGIN
    p := places(c);
    FOR i := 1 TO INTEGER(spaces) - INTEGER(p) DO
        Terminal.Write(' ');
    END;
    IF neg THEN
        Terminal.Write('-');
    END;
    FOR i := p-1 TO 0 BY -1 DO
        Terminal.Write(CHR((c DIV powerOfTen[i]) + ORD('0')));
        c := c MOD powerOfTen[i];
    END;
END writeNum;

PROCEDURE places(c:CARDINAL):CARDINAL;
(* Returns the number of places c takes to print; i.e. trunc(1+log10(c)). *)
VAR i:CARDINAL;
BEGIN
    FOR i := 4 TO 0 BY -1 DO
        IF (c DIV powerOfTen[i]) > 0 THEN
            RETURN i+1;
        END;
    END;
    RETURN 1;
END places;


PROCEDURE pause(msg:ARRAY OF CHAR);
(* Prevents the screen from blanking and returning to the Finder until the
   user hits a key.  msg is typed out. *)
VAR ch:CHAR;
BEGIN
    Terminal.WriteString(msg);
    Terminal.Read(ch);
END pause;

PROCEDURE fatal(msg:ARRAY OF CHAR);
BEGIN

    WriteLnString(msg);
    pause('Hit any key to die--');
    HALT;
END fatal;

PROCEDURE spaces(n:INTEGER);
VAR i:INTEGER;
BEGIN
    FOR i := 1 TO n DO
        Terminal.Write(' ');
    END;
END spaces;

            (*** Copies of Terminal procedures ***)

PROCEDURE WriteString(s:ARRAY OF CHAR);
BEGIN
    Terminal.WriteString(s);
END WriteString;

PROCEDURE WriteLn;
BEGIN
    Terminal.WriteLn;
END WriteLn;

PROCEDURE Write(c:CHAR);
BEGIN
    Terminal.Write(c);
END Write;

PROCEDURE Read(VAR c:CHAR);
BEGIN
    Terminal.Read(c);
END Read;

PROCEDURE ClearScreen;
BEGIN
    Terminal.ClearScreen;
END ClearScreen;

PROCEDURE Beep;
BEGIN
    Terminal.Beep;
END Beep;

BEGIN
    powerOfTen[0] := 1;
    powerOfTen[1] := 10;
    powerOfTen[2] := 100;
    powerOfTen[3] := 1000;
    powerOfTen[4] := 10000;
END MyTerminal.
