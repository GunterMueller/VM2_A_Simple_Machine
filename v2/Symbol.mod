IMPLEMENTATION MODULE Symbol;

(* Symbol and symbol list data structures. *)

FROM Token IMPORT stringType, tokenClass, typeType, tokenClassToType;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM MyTerminal IMPORT fatal;

TYPE
    symbol = POINTER TO symbolRec;
    symbolList = POINTER TO slRec;

    symbolRec = RECORD
                    string: stringType;
                    lexLevel: CARDINAL;
                    type: typeType;
                    next, prev: symbol;
                    offset:INTEGER;
                    CASE class: SymbolClass OF
                        Proc, Func: formals, locals: symbolList;
                        |   Keyword:    tokClass: tokenClass;
                    END;
                END;

    slRec = RECORD
                sym: symbol;
                next: symbolList;
            END;

                (*** getting fields ***)

PROCEDURE symbolClass(s:symbol):SymbolClass;
BEGIN
    RETURN s^.class;
END symbolClass;

PROCEDURE symbolString(s:symbol; VAR str:stringType);
BEGIN
    str := s^.string;
END symbolString;

PROCEDURE symbolType(s:symbol):typeType;
BEGIN
    RETURN s^.type;
END symbolType;

PROCEDURE symbolLexLevel(s:symbol):CARDINAL;
BEGIN
    RETURN s^.lexLevel;
END symbolLexLevel;

PROCEDURE symbolOffset(s:symbol):INTEGER;
BEGIN
   RETURN s^.offset;
END symbolOffset;

PROCEDURE symbolFormals(s:symbol):symbolList;
BEGIN
    IF (s^.class = Proc) OR (s^.class = Func) THEN
        RETURN s^.formals;
    ELSE
        fatal('symbolFormals: not a proc or func');
    END;
END symbolFormals;

PROCEDURE symbolLocals(s:symbol):symbolList;
BEGIN
    IF (s^.class = Proc) OR (s^.class = Func) THEN
        RETURN s^.locals;
    ELSE
        fatal('symbolLocals: not a proc or func');
    END;
END symbolLocals;

PROCEDURE symbolNext(s:symbol):symbol;
BEGIN
    IF s = emptySymbol THEN
        fatal('symbolNext: empty symbol given');
    ELSE
        RETURN s^.next;
    END;
END symbolNext;

PROCEDURE symbolPrev(s:symbol):symbol;
BEGIN
    RETURN s^.prev;
END symbolPrev;

PROCEDURE symbolTokClass(s:symbol):tokenClass;
BEGIN
    IF s^.class = Keyword THEN
        RETURN s^.tokClass;
    ELSE
        fatal('symbolTokClass: not a keyword');
    END;
END symbolTokClass;

                (*** setting fields ***)

PROCEDURE setSymbolFormals(s:symbol; sl:symbolList);
BEGIN
    IF (s^.class = Proc) OR (s^.class = Func) THEN
        s^.formals := sl;
    ELSE
        fatal('setSymbolFormals: not a proc or func');
    END;
END setSymbolFormals;

PROCEDURE setSymbolLocals(s:symbol; sl:symbolList);
BEGIN
    IF (s^.class = Proc) OR (s^.class = Func) THEN
        s^.locals := sl;
    ELSE
        fatal('setSymbolLocals: not a proc or func');
    END;
END setSymbolLocals;

PROCEDURE setSymbolType(s:symbol; tt:typeType);
BEGIN
    s^.type := tt;
END setSymbolType;

PROCEDURE setSymbolNext(s1, s2:symbol);
BEGIN
    s1^.next := s2;
END setSymbolNext;

PROCEDURE setSymbolPrev(s1, s2:symbol);
BEGIN
    s1^.prev := s2;
END setSymbolPrev;

PROCEDURE setSymbolOffset(s:symbol; o:INTEGER);
BEGIN
    s^.offset := o;
END setSymbolOffset;

PROCEDURE setSymbolTokClass(s:symbol; tc:tokenClass);
BEGIN
    IF s^.class = Keyword THEN
        s^.tokClass := tc;
    ELSE
        fatal('setSymbolTokClass: not a keyword');
    END;
END setSymbolTokClass;

(*** other symbol procedures ***)


PROCEDURE symbolClassEqual(s:symbol; sc:SymbolClass):BOOLEAN;
BEGIN
    RETURN (s^.class = Undeclared) OR (s^.class = sc);
END symbolClassEqual;

PROCEDURE symbolEqual(s1, s2:symbol):BOOLEAN;
BEGIN
    RETURN s1 = s2;
END symbolEqual;

PROCEDURE symbolEmpty(s:symbol):BOOLEAN;
BEGIN
    RETURN s = emptySymbol;
END symbolEmpty;

PROCEDURE newSymbol(VAR str:stringType; sc:SymbolClass; ll:CARDINAL;
                tt:typeType):symbol;
VAR s:symbol;
BEGIN
    NEW(s); (* should be: NEW(s, sc); *)
    WITH s^ DO
        string := str;
        lexLevel := ll;
        type := tt;
        next := emptySymbol;
        prev := emptySymbol;
        class := sc;
        CASE class OF
            Proc, Func:
                formals := emptySymbolList;
                locals := emptySymbolList;
        |   Variable: offset := 0;
        ELSE (* do nothing *)
        END;
    END;
    RETURN s;
END newSymbol;

PROCEDURE freeSymbol(s:symbol);
BEGIN
    DISPOSE(s); (* should be: DISPOSE(s, s^.class); *)
END freeSymbol;

PROCEDURE numFormals(s:symbol):CARDINAL;
VAR formList:symbolList;
    count:CARDINAL;
BEGIN
    count := 0;
    formList := symbolFormals(s);
    WHILE NOT slEmpty(formList) DO
        INC(count);
        formList := slNext(formList);
    END;
    RETURN count;
END numFormals;

PROCEDURE numLocals(s:symbol):CARDINAL;
VAR locList:symbolList;
    count:CARDINAL;
BEGIN
    count := 0;
    locList := symbolLocals(s);
    WHILE NOT slEmpty(locList) DO
        INC(count);
        locList := slNext(locList);
    END;
    RETURN count;
END numLocals;

                (*** symbolList ***)

PROCEDURE slEmpty(sl:symbolList):BOOLEAN;
BEGIN
    RETURN sl = emptySymbolList;
END slEmpty;

PROCEDURE slNext(sl:symbolList):symbolList;
BEGIN
    RETURN sl^.next;
END slNext;

PROCEDURE slSymbol(sl:symbolList):symbol;
BEGIN
    RETURN sl^.sym;
END slSymbol;

PROCEDURE addToSymbolList(s:symbol; sl:symbolList):symbolList;
VAR newsl: symbolList;
BEGIN
    NEW(newsl);
    newsl^.sym := s;
    newsl^.next := sl;
    RETURN newsl;
END addToSymbolList;

PROCEDURE freeSymbolList(sl:symbolList);
BEGIN
    IF NOT slEmpty(sl) THEN
        freeSymbolList(slNext(sl));
        DISPOSE(sl);
    END;
END freeSymbolList;

BEGIN
    emptySymbol := NIL;
    emptySymbolList := NIL;
END Symbol.
