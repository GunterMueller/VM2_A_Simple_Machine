IMPLEMENTATION MODULE Symbol;

(* Symbol and symbol list data structures.

  Changes made for part 3:
    1. The symbolRec data structure has been reorganized.
    2. Procedures have been added to access the new fields in a symbol,
       and to create array type objects.
    3. roffset for routines: used only for the hack (in
       SymbolTable.beginRoutine) that assigns a unique number to non-global
       routines.
*)

FROM Token IMPORT stringType, tokenClass;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM MyTerminal IMPORT fatal;
FROM SymbolTable IMPORT tUnknown;
FROM StringStuff IMPORT stringCopy;

TYPE
    symbol = POINTER TO symbolRec;
    symbolList = POINTER TO slRec;

    symbolRec = RECORD
                    string: stringType;
                    lexLevel: CARDINAL;
                    type: symbol;
                    next, prev: symbol;
                    CASE class: Class OF
                        Proc, Func: formals, locals: symbolList;
                                    roffset:INTEGER;
                    |   Local: loffset:INTEGER;
                    |   Formal: foffset:INTEGER; mode: modeType;
                    |   ScalarType: size: CARDINAL;
                    |   ArrayType: asize:CARDINAL; lowBound, highBound:INTEGER;
                                   open:BOOLEAN;
                    |   Keyword:    tokClass: tokenClass;
                    END;
                END;

    slRec = RECORD
                sym: symbol;
                next: symbolList;
            END;
    
    Cset = SET OF Class;

                (*** getting fields ***)

PROCEDURE class(s:symbol):Class;
BEGIN
    RETURN s^.class;
END class;

PROCEDURE string(s:symbol; VAR str:stringType);
BEGIN
    str := s^.string;
END string;

PROCEDURE type(s:symbol):symbol;
BEGIN
    RETURN s^.type;
END type;

PROCEDURE isType(s:symbol):BOOLEAN;
BEGIN
    RETURN (s^.class IN Cset{ArrayType, ScalarType}) OR (s = tUnknown);
END isType;

PROCEDURE lexLevel(s:symbol):CARDINAL;
BEGIN
    RETURN s^.lexLevel;
END lexLevel;

PROCEDURE offset(s:symbol):INTEGER;
BEGIN
    IF s^.class = Formal THEN
        RETURN s^.foffset;
    ELSIF s^.class = Local THEN
        RETURN s^.loffset;
    ELSIF s^.class IN Cset{Proc, Func} THEN
        RETURN s^.roffset;
    ELSE
        fatal('Symbol.offset: not a Formal, Local or routine');
    END;
END offset;

PROCEDURE mode(s:symbol):modeType;
BEGIN
    IF s^.class = Formal THEN
        RETURN s^.mode;
    ELSE
        fatal('Symbol.mode: not a formal');
    END;
END mode;

PROCEDURE size(s:symbol):CARDINAL;
BEGIN
    IF s^.class = ArrayType THEN
        RETURN s^.asize;
    ELSIF s^.class = ScalarType THEN
        RETURN s^.size;
    ELSE
        fatal('Symbol.size: not a type object');
    END;
END size;

PROCEDURE highBound(s:symbol):INTEGER;
BEGIN
    IF s^.class = ArrayType THEN
        RETURN s^.highBound;
    ELSE
        fatal('Symbol.highBound: not an array type');
    END;
END highBound;

PROCEDURE lowBound(s:symbol):INTEGER;
BEGIN
    IF s^.class = ArrayType THEN
        RETURN s^.lowBound;
    ELSE
        fatal('Symbol.lowBound: not an array type');
    END;
END lowBound;

PROCEDURE open(s:symbol):BOOLEAN;
BEGIN
    IF s^.class = ArrayType THEN
        RETURN s^.open;
    ELSE
        fatal('Symbol.open: not an array type');
    END;
END open;

PROCEDURE formals(s:symbol):symbolList;
BEGIN
    IF s^.class IN Cset{Proc, Func} THEN
        RETURN s^.formals;
    ELSE
        fatal('Symbol.formals: not a proc or func');
    END;
END formals;

PROCEDURE locals(s:symbol):symbolList;
BEGIN
    IF s^.class IN Cset{Proc, Func} THEN
        RETURN s^.locals;
    ELSE
        fatal('locals: not a proc or func');
    END;
END locals;

PROCEDURE next(s:symbol):symbol;
BEGIN
    IF s = emptySymbol THEN
        fatal('Symbol.next: empty symbol given');
    ELSE
        RETURN s^.next;
    END;
END next;

PROCEDURE prev(s:symbol):symbol;
BEGIN
    RETURN s^.prev;
END prev;

PROCEDURE tokClass(s:symbol):tokenClass;
BEGIN
    IF s^.class = Keyword THEN
        RETURN s^.tokClass;
    ELSE
        fatal('Symbol.tokClass: not a keyword');
    END;
END tokClass;

                (*** setting fields ***)

PROCEDURE setString(s:symbol; str:ARRAY OF CHAR);
VAR i:CARDINAL;
BEGIN
    stringCopy(s^.string, str);
END setString;

PROCEDURE setFormals(s:symbol; sl:symbolList);
BEGIN
    IF s^.class IN Cset{Proc, Func} THEN
        s^.formals := sl;
    ELSE
        fatal('setFormals: not a proc or func');
    END;
END setFormals;

PROCEDURE setLocals(s:symbol; sl:symbolList);
BEGIN
    IF s^.class IN Cset{Proc, Func} THEN
        s^.locals := sl;
    ELSE
        fatal('setLocals: not a proc or func');
    END;
END setLocals;

PROCEDURE setType(s1, t:symbol);
BEGIN
    s1^.type := t;
END setType;

PROCEDURE setNext(s1, s2:symbol);
BEGIN
    s1^.next := s2;
END setNext;

PROCEDURE setPrev(s1, s2:symbol);
BEGIN
    s1^.prev := s2;
END setPrev;

PROCEDURE setOffset(s:symbol; o:INTEGER);
BEGIN
    IF s^.class = Formal THEN
        s^.foffset := o;
    ELSIF s^.class = Local THEN
        s^.loffset := o;
    ELSIF s^.class IN Cset{Proc, Func} THEN
        s^.roffset := o;
    ELSE
        fatal('Symbol.setOffset: not a formal, local, or routine');
    END;
END setOffset;

PROCEDURE setMode(s:symbol; m:modeType);
BEGIN
    IF s^.class = Formal THEN
        s^.mode := m;
    ELSE
        fatal('Symbol.setMode: not a Formal');
    END;
END setMode;

PROCEDURE setSize(s:symbol; c:CARDINAL);
BEGIN
    IF s^.class = ScalarType THEN
        s^.size := c;
    ELSIF s^.class = ArrayType THEN
        s^.asize := c;
    ELSE
        fatal('Symbol.setSize: not a type object');
    END;
END setSize;

PROCEDURE setBounds(s:symbol; low, high:INTEGER);
BEGIN
    IF s^.class = ArrayType THEN
        s^.lowBound := low;
        s^.highBound := high;
    ELSE
        fatal('Symbol.setBounds: not an array type');
    END;
END setBounds;


PROCEDURE setOpen(s:symbol; b:BOOLEAN);
BEGIN
    IF s^.class = ArrayType THEN
        s^.open := b;
    ELSE
        fatal('Symbol.setOpen: not an array type');
    END;
END setOpen;

PROCEDURE setTokenClass(s:symbol; tc:tokenClass);
BEGIN
    IF s^.class = Keyword THEN
        s^.tokClass := tc;
    ELSE
        fatal('setTokenClass: not a keyword');
    END;
END setTokenClass;

(*** other symbol procedures ***)

PROCEDURE anonymous(s:symbol):BOOLEAN;
(* TRUE iff the array is anonymous *)
BEGIN
    RETURN s^.string[0] = 0C;
END anonymous;

PROCEDURE classEqual(s:symbol; sc:Class):BOOLEAN;
BEGIN
    RETURN (s^.class = Undeclared) OR (s^.class = sc);
END classEqual;

PROCEDURE equal(s1, s2:symbol):BOOLEAN;
BEGIN
    RETURN s1 = s2;
END equal;

PROCEDURE empty(s:symbol):BOOLEAN;
BEGIN
    RETURN s = emptySymbol;
END empty;

PROCEDURE new(str:stringType; sc:Class; ll:CARDINAL;
                typ:symbol):symbol;
VAR s:symbol;
BEGIN
    NEW(s); (* should be: NEW(s, sc); *)
    WITH s^ DO
        string := str;
        lexLevel := ll;
        type := typ;
        next := emptySymbol;
        prev := emptySymbol;
        class := sc;
        CASE class OF
            Proc, Func:
                formals := emptySymbolList;
                locals := emptySymbolList;
        |   Formal: foffset := 0; mode := mIn;
        |   Local: loffset := 0;
        |   ArrayType: open := FALSE;
        |   ScalarType: size := 1;
        ELSE (* do nothing *)
        END;
    END;
    RETURN s;
END new;

PROCEDURE newArrayType(baseType:symbol; lowBound, highBound:INTEGER;
                       open:BOOLEAN; lexLev:CARDINAL):symbol;
VAR ato:symbol;
BEGIN
    ato := new("", ArrayType, lexLev, baseType);
    setBounds(ato, lowBound, highBound);
    setOpen(ato, open);
    setSize(ato, size(baseType) * CARDINAL(highBound-lowBound+1));
    RETURN ato;
END newArrayType;

PROCEDURE copyArrayType(s:symbol):symbol;
BEGIN
    RETURN newArrayType(type(s), lowBound(s), highBound(s), open(s),
                         lexLevel(s));
END copyArrayType;

PROCEDURE free(s:symbol);
BEGIN
    DISPOSE(s); (* should be: DISPOSE(s, s^.class); *)
END free;

PROCEDURE numFormals(s:symbol):CARDINAL;
VAR formList:symbolList;
    count:CARDINAL;
BEGIN
    count := 0;
    formList := formals(s);
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
    locList := locals(s);
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