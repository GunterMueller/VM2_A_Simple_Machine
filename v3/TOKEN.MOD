IMPLEMENTATION MODULE Token;

(* Tokens and token lists for the SIMPL compiler. *)

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Terminal IMPORT WriteString;

TYPE tokenList = POINTER TO tokenListRec;   (* token lists are linked lists *)
     tokenListRec = RECORD
                        tok: token;
                        next: tokenList;
                    END;

PROCEDURE isRelation(tc:tokenClass):BOOLEAN;
BEGIN
    RETURN (tc = Equal) OR (tc = NotEqual) OR (tc = Greater) OR
        (tc = GreaterEqual) OR (tc = Less) OR (tc = LessEqual);
END isRelation;

PROCEDURE tlToken(tl:tokenList; VAR t:token);
BEGIN
    IF tlEmpty(tl) THEN
        WriteString("tlToken: empty tokenList");
    ELSE
        t := tl^.tok;
    END;
END tlToken;

PROCEDURE tlNext(tl:tokenList):tokenList;
BEGIN
    RETURN tl^.next;
END tlNext;

PROCEDURE addToTokenList(VAR t:token; tl:tokenList):tokenList;
(* Create a token list record for the new token and splice it on to the
   front of the token list.  Return ( a pointer to) the new record. *)
VAR newtl: tokenList;
BEGIN
    NEW(newtl);
    newtl^.tok := t;
    newtl^.next := tl;
    RETURN newtl;
END addToTokenList;

PROCEDURE freeTokenList(tl:tokenList);
BEGIN
    IF NOT tlEmpty(tl) THEN
        freeTokenList(tlNext(tl));
        DISPOSE(tl);
    END;
END freeTokenList;

PROCEDURE tlEmpty(tl:tokenList):BOOLEAN;
BEGIN
    RETURN tl = emptyTokenList;
END tlEmpty;


BEGIN
    emptyTokenList := NIL;
END Token.
