IMPLEMENTATION MODULE CharStuff;

FROM InOut IMPORT Read, EOL, WriteString;

VAR ch:CHAR;
    ungotten:BOOLEAN;

PROCEDURE getChar():CHAR;
BEGIN
    IF ungotten THEN
        ungotten := FALSE;
    ELSE
        Read(ch);
    END;
    RETURN ch;
END getChar;

PROCEDURE ungetChar;
BEGIN
    IF ungotten THEN

        WriteString("ungetChar: can only unget one character at a time");
        HALT;
    ELSE
        ungotten := TRUE;
    END;
END ungetChar;


PROCEDURE isAlphaNum(c:CHAR):BOOLEAN;
BEGIN
    RETURN isLetter(c) OR isDigit(c);
END isAlphaNum;

PROCEDURE isLetter(c:CHAR):BOOLEAN;
BEGIN
    RETURN isUpper(c) OR isLower(c);
END isLetter;

PROCEDURE isUpper(c:CHAR):BOOLEAN;
BEGIN
    RETURN (c >= 'A') AND (c <= 'Z');
END isUpper;

PROCEDURE isLower(c:CHAR):BOOLEAN;
BEGIN
    RETURN (c >= 'a') AND (c <= 'z');
END isLower;

PROCEDURE isDigit(c:CHAR):BOOLEAN;
BEGIN
    RETURN (c >= '0') AND (c <= '9');
END isDigit;

PROCEDURE isWhite(c:CHAR):BOOLEAN;
BEGIN
    RETURN (c = ' ') OR (c = TAB) OR (c = EOL);
END isWhite;

PROCEDURE toUpper(c:CHAR):CHAR;
BEGIN
    IF isLower(c) THEN
        RETURN CAP(c);
    ELSE
        RETURN c;
    END;
END toUpper;

BEGIN
    ungotten := FALSE;
END CharStuff.
