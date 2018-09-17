IMPLEMENTATION MODULE TypeChecker;

(* Handles type-checking of SIMPL expressions. *)

FROM Node IMPORT node, nodeType, nodeFirst, nodeRest, nodeEmpty, nodeClass,
    NodeClass, nodeSymbol;
FROM Token IMPORT tokenClass, stringType, typeType;
FROM Symbol IMPORT symbol, symbolType, symbolFormals, symbolList, symbolString,
        slNext, slSymbol, slEmpty, symbolClassEqual, SymbolClass, symbolClass,
        numFormals;
FROM MyTerminal IMPORT fatal;
FROM LexAn IMPORT compError;

PROCEDURE opAppropriate(op:tokenClass; arg:node):BOOLEAN;
BEGIN
    CASE op OF
        Plus, Minus, UMinus, Times, Divide:
            RETURN typeCompatible(nodeType(arg), tInteger);
    |   Greater, GreaterEqual, Less, LessEqual:
            RETURN typeCompatible(nodeType(arg), tInteger) OR
                   typeCompatible(nodeType(arg), tChar);
    |   And, Or, Not:
            RETURN typeCompatible(nodeType(arg), tBoolean);
    |   Equal, NotEqual:
            RETURN TRUE;
    ELSE
        fatal("opAppropriate: unknown op type");
    END;
END opAppropriate;

PROCEDURE typeCompatible(t1, t2:typeType):BOOLEAN;
BEGIN
    IF (t1 = tUnknown) OR (t2 = tUnknown) THEN
        RETURN TRUE;
    ELSE
        RETURN t1 = t2;
    END;
END typeCompatible;

PROCEDURE callCheck(routine:symbol; args:node);
(* Tricky because formals are stored backwards in symbol, but forwards
   in the call to the routine.  We do nothing if the symbol is not a procedure
   or function; that check is handled in the parser. *)
VAR nFormals, nActuals:CARDINAL;
    dummy:node;
BEGIN
    IF (symbolClass(routine) = Proc) OR (symbolClass(routine) = Func) THEN
        nFormals := numFormals(routine);
        nActuals := numActuals(args);
        IF nActuals < nFormals THEN
            compError('too few arguments to routine');
        ELSIF nActuals > nFormals THEN
            compError('too many arguments to routine');
        END;
        dummy := argsMatch(symbolFormals(routine), args);
    END;
END callCheck;

PROCEDURE argsMatch(flist:symbolList; alist:node):node;
(* This procedure matches two lists, one of which is backwards.  It does
   it by recursing down one list all the way, then iterating down the other
   list while unrecursing. *)
BEGIN
    IF slEmpty(flist) THEN
        RETURN alist;
    ELSE
        alist := argsMatch(slNext(flist), alist);
        IF nodeEmpty(alist) THEN
            RETURN alist;
        ELSE
            argCheck(slSymbol(flist), nodeFirst(alist));
            RETURN nodeRest(alist);
        END;
    END;
END argsMatch;

PROCEDURE argCheck(formal:symbol; actual:node);
VAR s:stringType;
BEGIN
    IF NOT typeCompatible(symbolType(formal), nodeType(actual)) THEN
        compError('type of formal does not match type of actual');
    END;
END argCheck;

PROCEDURE numActuals(actuals:node):CARDINAL;
VAR count:CARDINAL;
BEGIN
    count := 0;
    WHILE NOT nodeEmpty(actuals) DO
        INC(count);
        actuals := nodeRest(actuals);
    END;
    RETURN count;
END numActuals;

PROCEDURE readCheck(actuals:node);
VAR arg:node;
BEGIN
    IF nodeEmpty(actuals) THEN
        compError('READ requires an argument');
    ELSE
        REPEAT
            arg := nodeFirst(actuals);
            IF nodeClass(arg) <> nSymbol THEN
                compError('READ must have a variable as an argument');
            ELSIF NOT symbolClassEqual(nodeSymbol(arg), Variable) THEN
                compError('READ must have a variable as an argument');
            ELSIF NOT (typeCompatible(nodeType(arg), tInteger) OR
                       typeCompatible(nodeType(arg), tChar)) THEN
                compError('READ can only read integers or characters');
            END;
            actuals := nodeRest(actuals);
        UNTIL nodeEmpty(actuals);
    END;
END readCheck;

PROCEDURE writeCheck(actuals:node);
VAR arg:node;
BEGIN
    IF nodeEmpty(actuals) THEN
        compError('WRITE requires an argument');
    ELSE
        REPEAT
            arg := nodeFirst(actuals);
            IF NOT (typeCompatible(nodeType(arg), tInteger) OR
                       typeCompatible(nodeType(arg), tChar)) THEN
                compError('WRITE can only write integers or characters');
            END;
            actuals := nodeRest(actuals);
        UNTIL nodeEmpty(actuals);
    END;
END writeCheck;

PROCEDURE binopCheck(op:tokenClass; leftarg, rightarg:node):BOOLEAN;
BEGIN
    IF NOT opAppropriate(op, leftarg) THEN
        compError('inappropriate arg type: left arg');
        RETURN FALSE;
    END;
    IF NOT opAppropriate(op, rightarg) THEN
        compError('inappropriate arg type: right arg');
        RETURN FALSE;
    END;
    IF NOT typeCompatible(nodeType(leftarg), nodeType(rightarg)) THEN
        compError('argument types not compatible');
        RETURN FALSE;
    ELSE
        RETURN TRUE;
    END;
END binopCheck;

PROCEDURE unopCheck(op:tokenClass; arg:node):BOOLEAN;
BEGIN
    IF NOT opAppropriate(op, arg) THEN
        compError('inappropriate arg type');
        RETURN FALSE;
    ELSE
        RETURN TRUE;
    END;
END unopCheck;

PROCEDURE assignCheck(var:symbol; expr:node);
BEGIN
    IF NOT typeCompatible(symbolType(var), nodeType(expr)) THEN
        compError('types not assignment compatible');
    END;
END assignCheck;

PROCEDURE boolCheck(n:node);
BEGIN
    IF NOT typeCompatible(nodeType(n), tBoolean) THEN
        compError('Boolean expression expected');
    END;
END boolCheck;

PROCEDURE returnCheck(routine:symbol; expr:node):BOOLEAN;
BEGIN
    IF (NOT nodeEmpty(expr)) AND (symbolClass(routine) <> Func) THEN
        compError('only functions can return values');
        RETURN FALSE;
    ELSIF nodeEmpty(expr) AND (symbolClass(routine) <> Proc) THEN
        compError('function must return a value');
        RETURN FALSE;
    ELSIF (NOT nodeEmpty(expr)) AND
          (NOT typeCompatible(symbolType(routine), nodeType(expr))) THEN
        compError('return type not compatible with function type');
        RETURN FALSE;
    ELSE
        RETURN TRUE;
    END;
END returnCheck;

BEGIN
END TypeChecker.
