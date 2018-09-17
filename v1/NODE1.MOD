IMPLEMENTATION MODULE Node;

(* Procedures for constructing and manipulating the nodes of the parse tree.
   Most type-checking is done here. *)
FROM Token IMPORT tokenClass, typeType, stringType, isRelation;
FROM Symbol IMPORT symbol, SymbolClass, symbolType, emptySymbol, numFormals;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM TypeChecker IMPORT typeCompatible, opAppropriate, callCheck, unopCheck,
    readCheck, writeCheck, binopCheck, returnCheck, assignCheck;
FROM MyTerminal IMPORT WriteString, fatal;
FROM StringStuff IMPORT stringCopy;

TYPE
    node = POINTER TO nodeRec;
    nodeRec = RECORD
                type: typeType;
                CASE class:NodeClass OF
                    nOp: op: tokenClass; leftArg, rightArg: node;
                |   nUnop: unop: tokenClass; arg: node;
                |   nBool: bool: BOOLEAN;
                |   nInt: int: INTEGER;
                |   nChar: ch: CHAR;
                |   nString: string:stringType;
                |   nSymbol: sym: symbol;
                |   nIf: test, then, else: node;
                |   nWhile: wtest, stmts: node;
                |   nAssignment: LHS: symbol; RHS:node;
                |   nCall, nWrite, nRead: routine:symbol; args:node;
                |   nReturn: nFormals:CARDINAL; expr:node;
                |   nList: first, rest:node;  
                END;
              END;
  

PROCEDURE nodeClass(n:node):NodeClass;
BEGIN
    RETURN n^.class;
END nodeClass;

PROCEDURE nodeEmpty(n:node):BOOLEAN;
BEGIN
    RETURN n = emptyNode;
END nodeEmpty;

PROCEDURE freeNode(n:node);
BEGIN
    IF n <> emptyNode THEN
        WITH n^ DO CASE class OF
            nInt, nBool, nSymbol, nString, nChar: (* do nothing *);
        |   nOp:    freeNode(leftArg);
                    freeNode(rightArg);
        |   nUnop:  freeNode(arg);
        |   nIf:    freeNode(test);
                    freeNode(then);
                    freeNode(else);
        |   nWhile: freeNode(wtest);
                    freeNode(stmts);
        |   nAssignment: freeNode(RHS);
        |   nCall, nRead, nWrite:  freeNode(args);
        |   nReturn:freeNode(expr);
        |   nList:  freeNode(first);
                    freeNode(rest);
        ELSE
            WriteString("freeNode: unknown node type");
        END; END;
        DISPOSE(n);  (* , n^.class); *)
    END;
END freeNode;
    

                (*** node creation ***)

PROCEDURE makeStmtsNode(first, rest:node):node;
VAR n:node;
BEGIN
    n := newNode(nList);
    n^.first := first;
    n^.rest := rest;
    RETURN n;
END makeStmtsNode;

PROCEDURE makeExprListNode(first, rest:node):node;
VAR n:node;
BEGIN
    n := newNode(nList);
    n^.first := first;
    n^.rest := rest;
    RETURN n;
END makeExprListNode;
   
PROCEDURE makeIfNode(test, then, else:node):node;
VAR n:node;
BEGIN
    n := newNode(nIf);
    n^.test := test;
    n^.then := then;
    n^.else := else;
    RETURN n;
END makeIfNode;

PROCEDURE makeWhileNode(test, stmts:node):node;
VAR n:node;
BEGIN
    n := newNode(nWhile);
    n^.wtest := test;
    n^.stmts := stmts;
    RETURN n;
END makeWhileNode;

PROCEDURE makeReturnNode(routine:symbol; returnExpr:node):node;
VAR n:node;
BEGIN
    n := newNode(nReturn);
    n^.expr := returnExpr;
    IF returnCheck(routine, returnExpr) THEN
        n^.nFormals := numFormals(routine);
    END;
    RETURN n;
END makeReturnNode;

PROCEDURE makeAssignmentNode(var:symbol; expr:node):node;
VAR n:node;
BEGIN
    n := newNode(nAssignment);
    n^.LHS := var;
    n^.RHS := expr;
    assignCheck(var, expr);
    RETURN n;
END makeAssignmentNode;

PROCEDURE makeOpNode(op:tokenClass; leftarg, rightarg:node):node;
VAR n:node;
    typeOK:BOOLEAN;
BEGIN
    n := newNode(nOp);
    n^.op := op;
    n^.leftArg := leftarg;
    n^.rightArg := rightarg;
    typeOK := binopCheck(op, leftarg, rightarg);
    IF isRelation(op) THEN
        n^.type := tBoolean;
    ELSIF typeOK THEN
        n^.type := leftarg^.type;
    ELSE 
        n^.type := tUnknown;
    END;
    RETURN n;
END makeOpNode;

PROCEDURE makeUnopNode(op:tokenClass; arg:node):node;
VAR n:node;
BEGIN
    n := newNode(nUnop);
    n^.unop := op;
    n^.arg := arg;
    IF unopCheck(op, arg) THEN
        n^.type := arg^.type;
    ELSE
        n^.type := tUnknown;
    END;
    RETURN n;
END makeUnopNode;

PROCEDURE makeIntegerNode(i:INTEGER):node;
VAR n:node;
BEGIN
    n := newNode(nInt);
    n^.type := tInteger;
    n^.int := i;
    RETURN n;
END makeIntegerNode;

PROCEDURE makeBooleanNode(b:BOOLEAN):node;
VAR n:node;
BEGIN
    n := newNode(nBool);
    n^.type := tBoolean;
    n^.bool := b;
    RETURN n;
END makeBooleanNode;

PROCEDURE makeCharNode(c:CHAR):node;
VAR n:node;
BEGIN
    n := newNode(nChar);
    n^.type := tChar;
    n^.ch := c;
    RETURN n;
END makeCharNode;

PROCEDURE makeSymbolNode(s:symbol):node;
VAR n:node;
BEGIN
    n := newNode(nSymbol);
    n^.type := symbolType(s);
    n^.sym := s;
    RETURN n;
END makeSymbolNode;

PROCEDURE makeCallNode(name:symbol; actuals:node):node;
VAR n:node;
BEGIN
    n := newNode(nCall);
    WITH n^ DO
        routine := name;
        args := actuals;
        type := symbolType(name);
        callCheck(routine, args);
    END;        
    RETURN n;
END makeCallNode;

PROCEDURE makeWriteNode(actuals:node):node;
VAR n:node;
BEGIN
    writeCheck(actuals);
    n := newNode(nWrite);
    n^.routine := emptySymbol;
    n^.args := actuals;
    RETURN n;
END makeWriteNode;

PROCEDURE makeReadNode(actuals:node):node;
VAR n:node;
BEGIN
    readCheck(actuals);
    n := newNode(nRead);
    n^.routine := emptySymbol;
    n^.args := actuals;
    RETURN n;
END makeReadNode;

PROCEDURE makeStringNode(s:ARRAY OF CHAR):node;
VAR n:node;
BEGIN
    n := newNode(nString);
    stringCopy(n^.string, s);
    RETURN n;
END makeStringNode;

PROCEDURE newNode(nc:NodeClass):node;
VAR n:node;
BEGIN
    NEW(n);  (*  nc); *)
    n^.class := nc;
    n^.type := tUnknown;
    RETURN n;
END newNode;

                        (*** node access ***)

PROCEDURE nodeInt(n:node):INTEGER;
BEGIN
    nodeClassCheck('nodeInt', n, nInt);
    RETURN n^.int;
END nodeInt;

PROCEDURE nodeBool(n:node):BOOLEAN;
BEGIN
    nodeClassCheck('nodeBool', n, nBool);
    RETURN n^.bool;
END nodeBool;

PROCEDURE nodeChar(n:node):CHAR;
BEGIN
    nodeClassCheck('nodeChar', n, nChar);
    RETURN n^.ch;
END nodeChar;

PROCEDURE nodeString(n:node; VAR s:ARRAY OF CHAR);
BEGIN
    nodeClassCheck('nodeString', n, nString);
    stringCopy(s, n^.string);
END nodeString;

PROCEDURE nodeFirst(n:node):node;
BEGIN
    nodeClassCheck('nodeFirst', n, nList);
    RETURN n^.first;
END nodeFirst;

PROCEDURE nodeRest(n:node):node;
BEGIN
    nodeClassCheck('nodeRest', n, nList);
    RETURN n^.rest;
END nodeRest;

PROCEDURE nodeTest(n:node):node;
BEGIN
    IF n^.class = nIf THEN
        RETURN n^.test;
    ELSIF n^.class = nWhile THEN
        RETURN n^.wtest;
    ELSE
        nodeClassError('nodeTest');
        RETURN emptyNode;
    END;
END nodeTest;

PROCEDURE nodeThen(n:node):node;
BEGIN
    nodeClassCheck('nodeThen', n, nIf);
    RETURN n^.then;
END nodeThen;

PROCEDURE nodeElse(n:node):node;
BEGIN
    nodeClassCheck('nodeElse', n, nIf);
    RETURN n^.else;
END nodeElse;

PROCEDURE nodeStmts(n:node):node;
BEGIN
    nodeClassCheck('nodeStmts', n, nWhile);
    RETURN n^.stmts;
END nodeStmts;

PROCEDURE nodeRHS(n:node):node;
BEGIN
    nodeClassCheck('nodeRHS', n, nAssignment);
    RETURN n^.RHS;
END nodeRHS;

PROCEDURE nodeLHS(n:node):symbol;
BEGIN
    nodeClassCheck('nodeLHS', n, nAssignment);
    RETURN n^.LHS;
END nodeLHS;

PROCEDURE nodeArgs(n:node):node;
BEGIN
    WITH n^ DO
        IF (class = nCall) OR (class = nRead) OR (class = nWrite) THEN
            RETURN args;
        ELSE
            nodeClassError('nodeArgs');
        END;
    END;
END nodeArgs;

PROCEDURE nodeRoutine(n:node):symbol;
BEGIN
    nodeClassCheck('nodeRoutine', n, nCall);
    RETURN n^.routine;
END nodeRoutine;

PROCEDURE nodeExpr(n:node):node;
BEGIN
    nodeClassCheck('nodeExpr', n, nReturn);
    RETURN n^.expr;
END nodeExpr;

PROCEDURE nodeArg(n:node):node;
BEGIN
    nodeClassCheck('nodeArg', n, nUnop);
    RETURN n^.arg;
END nodeArg;

PROCEDURE nodeLeftArg(n:node):node;
BEGIN
    nodeClassCheck('nodeLeftArg', n, nOp);
    RETURN n^.leftArg;
END nodeLeftArg;

PROCEDURE nodeRightArg(n:node):node;
BEGIN
    nodeClassCheck('nodeRightArg', n, nOp);
    RETURN n^.rightArg;
END nodeRightArg;

PROCEDURE nodeOp(n:node):tokenClass;
BEGIN
    IF n^.class = nOp THEN
        RETURN n^.op;
    ELSIF n^.class = nUnop THEN
        RETURN n^.unop;
    ELSE
        nodeClassError('nodeOp');
        RETURN Plus;
    END;
END nodeOp;

PROCEDURE nodeSymbol(n:node):symbol;
BEGIN
    nodeClassCheck('nodeSymbol', n, nSymbol);
    RETURN n^.sym;
END nodeSymbol;

PROCEDURE nodeNumFormals(n:node):CARDINAL;
BEGIN
    nodeClassCheck('nodeNumFormals', n, nReturn);
    RETURN n^.nFormals;
END nodeNumFormals;

PROCEDURE nodeType(n:node):typeType;
BEGIN
    RETURN n^.type;
END nodeType;

(*** other ***)

PROCEDURE nodeClassCheck(s:ARRAY OF CHAR; n:node; nc:NodeClass);
BEGIN
    IF n^.class <> nc THEN
        nodeClassError(s);
    END;
END nodeClassCheck;

PROCEDURE nodeClassError(s:ARRAY OF CHAR);
BEGIN
    WriteString(s);
    fatal(": node of wrong type");
END nodeClassError;

BEGIN
    emptyNode := NIL;
END Node.
