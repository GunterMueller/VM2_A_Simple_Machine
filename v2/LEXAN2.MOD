IMPLEMENTATION MODULE LexAn;

(* Lexical analyzer for the SIMPL compiler.  Uses the routines in
LexAnStuff.*)

FROM InOut IMPORT EOL;
FROM Token IMPORT token, tokenClass;
FROM MyTerminal IMPORT fatal, WriteLnString;
FROM StringStuff IMPORT stringLen;
FROM SymbolTable IMPORT enterKeyword, findKeyword;
FROM LexAnStuff IMPORT dispatch, enterAll, enterChar, enterEndOfFile, ignore,
    enterAlphas, enterDigits, skipToChar, alphaNumString, posInteger, string,
    enterWhite, writeLine, getChar, ungetChar;

VAR tok: token;
    ungotten: BOOLEAN;

PROCEDURE getToken(VAR t:token);
BEGIN
    getTok;
    t := tok;
END getToken;

PROCEDURE getTok;
VAR c:CHAR;
BEGIN
    IF ungotten THEN
        ungotten := FALSE;
    ELSE
       dispatch;
    END;
END getTok;

PROCEDURE ungetToken;
BEGIN
    IF ungotten THEN
        fatal("ungetToken: can only unget one token at a time");
    ELSE
        ungotten := TRUE;
    END;
END ungetToken;

PROCEDURE getTokenClass():tokenClass;
BEGIN
    getTok;
    RETURN tok.class;
END getTokenClass;

PROCEDURE peekTokenClass():tokenClass;
BEGIN
    getTok;
    ungetToken;
    RETURN tok.class;
END peekTokenClass;

PROCEDURE tokenErrorCheck(tc:tokenClass; msg: ARRAY OF CHAR);
BEGIN
    getTok;
    IF tok.class <> tc THEN
        compError(msg);
        IF tok.class = EndOfInput THEN
            fatal("unexpected end of input");
        END;
        ungetToken;
    END;
END tokenErrorCheck;

PROCEDURE getTokenErrorCheck(VAR t:token; tc:tokenClass; msg: ARRAY OF CHAR);
BEGIN
    tokenErrorCheck(tc, msg);
    t := tok;
END getTokenErrorCheck;

                (*** reading procedures ***)

PROCEDURE illegalChar(c:CHAR);
VAR charstring:ARRAY[0..1] OF CHAR;
BEGIN
    charstring[0] := c;         (* fake a 1-char string *)
    charstring[1] := 0C;
    compError('illegal character');
    getTok;
END illegalChar;


PROCEDURE comment(c:CHAR);      (* Comments are ignored.  They are delimited
                                   by { and } *)
BEGIN
    skipToChar('}');
    getTok;
END comment;


PROCEDURE idOrKeyword(c:CHAR);
(* Get an alphanumeric string from the input. If we find it in the symbol
   table marked as a keyword, then it's a keyword; findKeyword will have taken
   care of setting tok.class to the right value.  Else, it's an identifier. *)
BEGIN
    IF NOT alphaNumString(c, tok.string) THEN
        compError('identifier too long');
    END;
    IF NOT findKeyword(tok.string, tok.class) THEN
        tok.class := Identifier;
    END;
END idOrKeyword;

PROCEDURE posInt(c:CHAR);
BEGIN
    tok.class := Int;
    tok.integer := posInteger(c);
END posInt;

PROCEDURE charProc(c:CHAR);
(* Read a character, delimited by delim, from the input. Can use
   backslash: \n = newline, \t = tab, anything else literal. *)
BEGIN
    tok.class := Character;
    IF (NOT string(c, tok.string)) OR (stringLen(tok.string) > 1) THEN
        compError('illegal character constant');
    END;
    tok.ch := tok.string[0];
END charProc;

PROCEDURE stringProc(c:CHAR);
(* Read a string from the input.  If too long, skip to the next delim.  *)
BEGIN
    tok.class := String;
    IF NOT string(c, tok.string) THEN
        compError('string too long');
        skipToChar(c);
        c := getChar();     (* get the delimiter *)
    END;
END stringProc;

                (*** Reading special characters ***)

PROCEDURE period(c:CHAR);   BEGIN tok.class := Period;      END period;
PROCEDURE semicolon(c:CHAR);BEGIN tok.class := Semicolon;   END semicolon;
PROCEDURE equal(c:CHAR);    BEGIN tok.class := Equal;       END equal;
PROCEDURE comma(c:CHAR);    BEGIN tok.class := Comma;       END comma;
PROCEDURE plus(c:CHAR);     BEGIN tok.class := Plus;        END plus;
PROCEDURE minus(c:CHAR);    BEGIN tok.class := Minus;       END minus;
PROCEDURE times(c:CHAR);    BEGIN tok.class := Times;       END times;
PROCEDURE divide(c:CHAR);   BEGIN tok.class := Divide;      END divide;
PROCEDURE lparen(c:CHAR);   BEGIN tok.class := Lparen;      END lparen;
PROCEDURE rparen(c:CHAR);   BEGIN tok.class := Rparen;      END rparen;

PROCEDURE greater(c:CHAR);
BEGIN
    IF getChar() = '=' THEN
        tok.class := GreaterEqual;
    ELSE
        ungetChar;
        tok.class := Greater;
    END;
END greater;

PROCEDURE less(c:CHAR);
BEGIN
    c := getChar();
    IF c = '=' THEN
        tok.class := LessEqual;
    ELSIF c = '>' THEN
        tok.class := NotEqual;
    ELSE
        ungetChar;
        tok.class := Less;
    END;
END less;

PROCEDURE colon(c:CHAR);
BEGIN
    IF getChar() = '=' THEN
        tok.class := Assignment;
    ELSE
        ungetChar;
        tok.class := Colon;
    END;
END colon;

PROCEDURE endOfInput(c:CHAR);
BEGIN
    tok.class := EndOfInput;
END endOfInput;

                (*** initialization of charTable ***)

PROCEDURE initCharTable;
BEGIN
    enterAll(illegalChar);
    enterWhite(ignore);
    enterAlphas(idOrKeyword);
    enterDigits(posInt);
    enterChar('.', period);
    enterChar(':', colon);
    enterChar(';', semicolon);
    enterChar('(', lparen);
    enterChar(')', rparen);
    enterChar(',', comma);
    enterChar('=', equal);
    enterChar('>', greater);
    enterChar('<', less);
    enterChar('+', plus);
    enterChar('-', minus);
    enterChar('*', times);
    enterChar('/', divide);
    enterChar('{', comment);
    enterChar('"', stringProc);
    enterChar("'", charProc);
    enterEndOfFile(endOfInput);
END initCharTable;


PROCEDURE enterKeywords;
BEGIN
    enterKeyword('AND', And);
    enterKeyword('BEGIN', Begin);
    enterKeyword('BOOLEAN', Boolean);
    enterKeyword('CHAR', Char);
    enterKeyword('DO', Do);
    enterKeyword('ELSE', Else);
    enterKeyword('ELSIF', Elsif);
    enterKeyword('END', End);
    enterKeyword('FALSE', False);
    enterKeyword('FUNCTION', Function);
    enterKeyword('IF', If);
    enterKeyword('INTEGER', Integer);
    enterKeyword('NOT', Not);
    enterKeyword('OR', Or);
    enterKeyword('PROCEDURE', Procedure);
    enterKeyword('PROGRAM', Program);
    enterKeyword('READ', Read);
    enterKeyword('RETURN', Return);
    enterKeyword('THEN', Then);
    enterKeyword('TRUE', True);
    enterKeyword('VAR', Var);
    enterKeyword('WHILE', While);
    enterKeyword('WRITE', Write);
END enterKeywords;

                (*** errors ***)

PROCEDURE compError(msg:ARRAY OF CHAR);
BEGIN
    writeLine;
    WriteLnString(msg);
    errorFlag := TRUE;
END compError;

BEGIN
    ungotten := FALSE;
    errorFlag := FALSE;
    initCharTable;
    enterKeywords;
END LexAn.
