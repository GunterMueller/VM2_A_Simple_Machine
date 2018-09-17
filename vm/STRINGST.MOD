IMPLEMENTATION MODULE StringStuff;


PROCEDURE charCap(ch:CHAR):CHAR;
BEGIN
    IF (ch >= 'a') AND (ch <= 'z') THEN
        RETURN CAP(ch);
    ELSE
        RETURN ch;
    END;
END charCap;

PROCEDURE stringCap(VAR s:ARRAY OF CHAR);
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO stringLen(s) DO
        s[i] := charCap(s[i]);
    END;

END stringCap;

PROCEDURE stringLen(VAR s:ARRAY OF CHAR):CARDINAL;
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(s) DO
        IF s[i] = 0C THEN
            RETURN i;
        END;
    END;
    RETURN HIGH(s)+1;
END stringLen;

PROCEDURE stringCopy(VAR s1:ARRAY OF CHAR; s2:ARRAY OF CHAR);
VAR i:CARDINAL;
BEGIN
    i := 0;
    LOOP
        IF i > HIGH(s1) THEN
            EXIT;
        ELSIF i > HIGH(s2) THEN
            s1[i] := 0C;
            EXIT;
        ELSE
            s1[i] := s2[i];
        END;
        INC(i);
    END;
END stringCopy;

PROCEDURE stringEqual(s1, s2:ARRAY OF CHAR):BOOLEAN;
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(s1) DO
        IF i > HIGH(s2) THEN
            RETURN s1[i] = 0C;
        ELSIF s1[i] <> s2[i] THEN
            RETURN FALSE;
        ELSIF s1[i] = 0C THEN
            RETURN TRUE;
        END;
    END;
    RETURN TRUE;
END stringEqual;


BEGIN
END StringStuff.
