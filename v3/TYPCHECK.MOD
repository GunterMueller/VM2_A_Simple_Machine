IMPLEMENTATION MODULE TypeChecker;

(* Handles type-checking of SIMPL expressions. *)

FROM Node IMPORT node, nodeType, nodeFirst, nodeRest, nodeEmpty, nodeClass,
    NodeClass, nodeSymbol, nodeString, nodeArray, nodeIndex;
FROM Token IMPORT tokenClass, stringType;
FROM Symbol IMPORT symbol, symbolList, slNext, slSymbol, slEmpty,  Class, 
        numFormals, emptySymbol, modeType;
IMPORT Symbol;
FROM SymbolTable IMPORT tUnknown, tInteger, tChar, tBoolean, tString,
        lowFunc, highFunc;
FROM MyTerminal IMPORT fatal;
FROM LexAn IMPORT compError;
FROM StringStuff IMPORT stringLen;

TYPE Cset = SET OF Class;
     Nset = SET OF NodeClass;

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
            RETURN typeCompatible(nodeType(arg), tInteger) OR
                   typeCompatible(nodeType(arg), tChar) OR
                   typeCompatible(nodeType(arg), tBoolean);
    ELSE
        fatal("opAppropriate: unknown op type");
    END;
END opAppropriate;

PROCEDURE typeCompatible(t1, t2:symbol):BOOLEAN;
(* Two types are compatible if they have the same base type. *)
BEGIN
    RETURN typeIdentical(baseType(t1), baseType(t2));
END typeCompatible;

PROCEDURE typeIdentical(t1, t2:symbol):BOOLEAN;
(* Two types are identical iff they are the SAME type object, or if one is
   tUnknown. *)
BEGIN
    RETURN  Symbol.equal(t1, tUnknown) OR
            Symbol.equal(t2, tUnknown) OR
            Symbol.equal(t1, t2);
END typeIdentical;

PROCEDURE baseType(typeObject:symbol):symbol;
(* Compute the base type of typeObject. *)
BEGIN
    IF Symbol.empty(typeObject) THEN
        RETURN typeObject;
    END;
    WHILE NOT (Symbol.empty(Symbol.type(typeObject)) OR
      (Symbol.class(typeObject) = ArrayType)) DO
        typeObject := Symbol.type(typeObject);
    END;
    RETURN typeObject;
END baseType; 

PROCEDURE callCheck(routine:symbol; args:node);
(* Tricky because formals are stored backwards in symbol, but forwards
   in the call to the routine.  We do nothing if the symbol is not a procedure
   or function; that check is handled in the parser.
   For part 3: special check for lowFunc and highFunc. *)
VAR nFormals, nActuals:CARDINAL;
    dummy:node;
BEGIN
    IF Symbol.class(routine) IN Cset{Proc, Func} THEN
        nFormals := numFormals(routine);
        nActuals := numActuals(args);
        IF nActuals < nFormals THEN
            compError('too few arguments to routine');
        ELSIF nActuals > nFormals THEN
            compError('too many arguments to routine');
        END;
        dummy := argsMatch(Symbol.formals(routine), args,
                 Symbol.equal(routine, lowFunc) OR Symbol.equal(routine, highFunc));
    END;
END callCheck;

PROCEDURE argsMatch(flist:symbolList; alist:node; lowOrHigh:BOOLEAN):node;
(* This procedure matches two lists, one of which is backwards.  It does
   it by recursing down one list all the way, then iterating down the other
   list while unrecursing. *)
BEGIN
    IF slEmpty(flist) THEN
        RETURN alist;
    ELSE
        alist := argsMatch(slNext(flist), alist, lowOrHigh);
        IF nodeEmpty(alist) THEN
            RETURN alist;
        ELSE
            argCheck(slSymbol(flist), nodeFirst(alist), lowOrHigh);
            RETURN nodeRest(alist);
        END;
    END;
END argsMatch;

PROCEDURE argCheck(formal:symbol; actual:node; lowOrHigh:BOOLEAN);
(* An argument matches a formal if:
    The modes are compatible (see modeCompatible, below) AND
        1. The types are IDENTICAL (not compatible) OR
        2. The formal has an open array param as a type, and the
           actual is an array of the identical base type (incl. string) OR
        3. The formal is an array of CHAR and the actual is a string
           constant of size <= the array.
  For LOW and HIGH, all that is required is that the arg be an array.
*)
VAR ftype, atype:symbol;
BEGIN
  IF NOT nodeEmpty(actual) THEN
    IF lowOrHigh THEN
        IF NOT Symbol.classEqual(baseType(nodeType(actual)), ArrayType) THEN
            compError("LOW and HIGH take only arrays");
        END;
    ELSIF modeCompatible(formal, actual) THEN
        ftype := Symbol.type(formal);
        atype := nodeType(actual);
        IF NOT (typeIdentical(ftype, atype) OR 
               openArray(ftype, atype)  OR
               stringConst(ftype, actual)) THEN
            compError('type of formal does not match type of actual');
        END;
    ELSE
        compError("mode incompatibility");
    END;
  END;
END argCheck;

PROCEDURE modeCompatible(formal:symbol; actual:node):BOOLEAN;
(* TRUE iff:
    1. formal has mode IN and actual does not have mode OUT; OR
    2. formal has mode OUT and actual
        2a. is a variable or index; and
        2b. does not have mode IN; OR
    3. formal has mode IN OUT and actual
        3a. is a variable or index; and
        3b. does not have modes IN or OUT *)
BEGIN
    IF Symbol.mode(formal) = mIn THEN
        RETURN NOT hasMode(actual, mOut);
    ELSIF NOT (nodeClass(actual) IN Nset{nSymbol, nIndex}) THEN
        RETURN FALSE;
    ELSIF Symbol.mode(formal) = mOut THEN
        RETURN NOT hasMode(actual, mIn);
    ELSIF Symbol.mode(formal) = mInOut THEN
        RETURN (NOT hasMode(actual, mIn)) AND (NOT hasMode(actual, mOut));
    ELSE
        fatal('modeCompatible: unknown mode');
    END;
END modeCompatible;

PROCEDURE hasMode(n:node; m:modeType):BOOLEAN;
(* A node has a particular mode iff:
    1. The node is an index and the array being indexed has mode m; OR
    2. The node is a symbol, the symbol is a formal and it has mode m.
*)
BEGIN
    IF nodeClass(n) = nIndex THEN
        RETURN hasMode(nodeArray(n), m);
    ELSE
        RETURN  (nodeClass(n) = nSymbol) AND
                (Symbol.class(nodeSymbol(n)) = Formal) AND 
                (Symbol.mode(nodeSymbol(n)) = m);
    END;
END hasMode;

PROCEDURE openArray(ftype, atype:symbol):BOOLEAN;
(* TRUE iff ftype is an open array and atype is an array of the identical
   base type. *)
BEGIN
    RETURN (Symbol.class(ftype) = ArrayType) AND
           Symbol.open(ftype) AND
           (Symbol.class(atype) = ArrayType) AND
           typeIdentical(Symbol.type(ftype), Symbol.type(atype));
END openArray;

PROCEDURE stringConst(ftype:symbol; actual:node):BOOLEAN;
(* TRUE iff ftype is an array of char (possibly open) and actual is tString,
   and the string const is shorter than the array. *)
VAR s:stringType;
BEGIN
    IF (Symbol.class(ftype) = ArrayType)       AND
       Symbol.equal(Symbol.type(ftype), tChar) AND
       Symbol.equal(nodeType(actual), tString) THEN
        nodeString(actual, s);
        RETURN Symbol.open(ftype) OR (stringLen(s) <= Symbol.size(ftype));
    ELSE
        RETURN FALSE;
    END;
END stringConst;

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
          IF NOT nodeEmpty(arg) THEN
            IF NOT assignable(arg) THEN
                compError('READ must be able to assign to its arguments');
            END;
            IF  NOT charOrInt(nodeType(arg)) THEN
                compError('READ can only read integers or characters');
            END;
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
          IF NOT nodeEmpty(arg) THEN
            IF NOT charOrInt(nodeType(arg)) THEN
                compError('WRITE can only write integers or characters');
            END;
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
    (* The two operands must be THE SAME, not just compatible. *)
    IF NOT typeIdentical(nodeType(leftarg), nodeType(rightarg)) THEN
        compError('argument types not identical');
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

PROCEDURE assignCheck(var, expr:node);
(* For assignment, the types must be identical, unless one is an open 
   array, in which case only the base types need be identical. If the
   expr is a string constant, the var need only be an array of char. *)
VAR t1,t2:symbol;
BEGIN
    t1 := nodeType(var);
    t2 := nodeType(expr);
    IF NOT (openArray(t1, t2) OR
            openArray(t2, t1) OR
            typeIdentical(t1, t2) OR
            stringConst(t1, expr)) THEN
        compError('types not assignment-compatible');
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
    IF (NOT nodeEmpty(expr)) AND (NOT Symbol.classEqual(routine, Func)) THEN
        compError('only functions can return values');
        RETURN FALSE;
    ELSIF nodeEmpty(expr) AND Symbol.classEqual(routine, Func) THEN
        compError('function must return a value');
        RETURN FALSE;
    ELSIF (NOT nodeEmpty(expr)) AND
          (NOT typeIdentical(Symbol.type(routine), nodeType(expr))) THEN
        compError('return type not identical to function type');
        RETURN FALSE;
    ELSE
        RETURN TRUE;
    END;
END returnCheck;


PROCEDURE assignable(n:node):BOOLEAN;
(* Something can be assigned to if: it is a variable (including array
   indices), and it does not have mode IN. *)
BEGIN
    RETURN variable(n) AND (NOT hasMode(n, mIn));
END assignable;


PROCEDURE indexCheck(index:node);
BEGIN
  IF NOT nodeEmpty(index) THEN
      IF hasMode(index, mOut) THEN
          compError("can't use an OUT formal to index an array");
      ELSIF NOT typeCompatible(nodeType(index), tInteger) THEN
          compError("array index must be compatible with type INTEGER");
      END;
  END;
END indexCheck;

PROCEDURE variable(n:node):BOOLEAN;
(* TRUE iff n is a symbol of class Global, Local, or Formal; or
   if n is an index. *)
BEGIN
RETURN (NOT nodeEmpty(n)) AND
       ((nodeClass(n) = nIndex) OR
        ((nodeClass(n) = nSymbol) AND 
        (Symbol.class(nodeSymbol(n)) IN Cset{Global, Local, Formal})));
END variable;

PROCEDURE charOrInt(t:symbol):BOOLEAN;
(* TRUE iff t is a type object compatible with CHAR or INTEGER *)
BEGIN
    RETURN typeCompatible(t, tChar) OR typeCompatible(t, tInteger);
END charOrInt;


BEGIN
END TypeChecker.
