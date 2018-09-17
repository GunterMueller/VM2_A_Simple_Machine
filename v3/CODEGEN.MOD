IMPLEMENTATION MODULE CodeGen;

(* Code Generator for the SIMPL compiler.

   Changes for part 3:
    1. Arrays handled.  No arrays are initialized.  Now, also,
        locals of any type are not initialized.
     2. IN/OUT handled.
     3. Array and string const assignment handled.
     4. String constant assignment handled.
*)
 
FROM MyTerminal IMPORT fatal;
IMPORT MyTerminal;
FROM InOut IMPORT WriteString, WriteLn, WriteCard;
FROM Node IMPORT node, nodeClass, NodeClass, nodeEmpty, nodeFirst, nodeRest,
        nodeTest, nodeThen, nodeElse, nodeStmts, nodeRHS, nodeLHS, nodeArgs,
        nodeRoutine, nodeExpr, nodeArg, nodeLeftArg, nodeRightArg, nodeOp,
        nodeSymbol, nodeInt, nodeBool, nodeNumFormals, nodeChar,
        nodeType, freeNode, nodeArray, nodeIndex, nodeString;
FROM CodeWrite IMPORT writeLabel, writeStringLabel, writeStringBranch,
        writeCondBranch, writeBranch, writeSymPop, writeCall, writeChar,
        writeWriteInt, writeInt, writeReadInt, writeReturn, writeFReturn,
        writeOp, writeBool, writeSymbol, writeWriteChar, writeReadChar,
        writeSetSP, writePop, writeLow, writeHigh,  writeCopy, writeAddr,
        writeMin, writeContents, writeAref, recordString;
FROM Token IMPORT tokenClass, isRelation, stringType;
FROM LexAn IMPORT errorFlag;
FROM Symbol IMPORT symbol, numLocals, Class, modeType,
        symbolList, slSymbol, slNext, slEmpty;
IMPORT Symbol;
FROM SymbolTable IMPORT tString, lowFunc, highFunc, tInteger, tChar, tBoolean;
FROM StringStuff IMPORT stringLen;
FROM TypeChecker IMPORT baseType;


                (*** label generation ***)

(* The code generator needs a supply of unique label names. *)

MODULE LabelGenerator;
EXPORT newLabel, label;

    TYPE label = CARDINAL;

    VAR labelCount:CARDINAL;

    PROCEDURE newLabel():label;
    BEGIN
        INC(labelCount);
        RETURN label(labelCount);
    END newLabel;

BEGIN
    labelCount := 0;
END LabelGenerator;


PROCEDURE genBlock(n:node);
(* This is the interface to generating statements.  We don't waste our time
   doing generation if there has been an error.  Also, it's possible for this
   routine to get an empty node (legally); in that case, we do nothing. *)
BEGIN
    IF (NOT errorFlag) AND (NOT nodeEmpty(n)) THEN
        IF nodeClass(n) <> nList THEN
            MyTerminal.fatal('genBlock: node class must be nList');
        ELSE
            genStmts(n);
            freeNode(n);
        END;
    END;
END genBlock;

PROCEDURE genGlobal(s:symbol);
(* Output the global symbol as a label.  Initialize integers to 0, booleans
   to FALSE (which is also zero), chars to NUL (which is again zero). 
			For arrays, just write a .BLOCK. No initialization. *)
VAR name:stringType;
    size:CARDINAL;
BEGIN
    IF NOT errorFlag THEN
        IF Symbol.class(s) = Global THEN
            Symbol.string(s, name);
            size := Symbol.size(Symbol.type(s));
            writeStringLabel(name);
            IF size = 1 THEN
                WriteString("  0");
            ELSE
                WriteString("  .BLOCK ");
                WriteCard(size, 0);
            END;
            WriteLn;
        ELSE
        MyTerminal.WriteString("genGlobal: not a global: ");
        fatal(name);
        END;
    END;
END genGlobal;

PROCEDURE genLocals(routine:symbol);
(* save space for locals on stack *)
VAR n:CARDINAL;
BEGIN
    n := sizeLocals(routine);
    IF n <> 0 THEN
        writeSetSP(INTEGER(n));
    END;
END genLocals;

PROCEDURE sizeLocals(routine:symbol):CARDINAL;
VAR locals:symbolList;
    sum:CARDINAL;
BEGIN
    locals := Symbol.locals(routine);
    sum := 0;
    WHILE NOT slEmpty(locals) DO
        INC(sum, Symbol.size(Symbol.type(slSymbol(locals))));
        locals := slNext(locals);
    END;
    RETURN sum;
END sizeLocals;


PROCEDURE genStmts(n:node);
(* Generate a list of statements, if the node isn't empty. *)
BEGIN
    IF NOT nodeEmpty(n) THEN
        genStmt(nodeFirst(n));
        genStmts(nodeRest(n));
    END;
END genStmts;

                (*** Statements ***)

PROCEDURE genStmt(n:node);
BEGIN
    CASE nodeClass(n) OF
        nIf: genIfStmt(n);
    |   nWhile: genWhileStmt(n);
    |   nAssignment: genAssignStmt(n);
    |   nCall: genCallStmt(n);
    |   nWrite: genWriteStmt(n);
    |   nRead: genReadStmt(n);
    |   nReturn: genReturnStmt(n);
    ELSE
        MyTerminal.fatal("genStmt: unknown statement type");
    END;
END genStmt;

        
PROCEDURE genIfStmt(n:node);
VAR label1, label2:label;
BEGIN
    label1 := newLabel();
    genExpr(nodeTest(n));               (* generate test *)
    writeCondBranch(Equal, label1);     (* branch to else part if test false *)
    genBlock(nodeThen(n));              (* generate then part *)
    IF nodeEmpty(nodeElse(n)) THEN 
        writeLabel(label1);             (* no else part *)
    ELSE
        label2 := newLabel();
        writeBranch(label2);            (* branch around els part *)
        writeLabel(label1);             (* label for else part *)
        genBlock(nodeElse(n));          (* generate else part *)
        writeLabel(label2);             (* final label *)
    END;
END genIfStmt;

PROCEDURE genWhileStmt(n:node);
VAR testLabel, endLabel:label;
BEGIN
    testLabel := newLabel();
    endLabel := newLabel();
    writeLabel(testLabel);              (* label for top of loop *)
    genExpr(nodeTest(n));               (* generate test *)
    writeCondBranch(Equal, endLabel);   (* if false, branch to end of loop *)
    genBlock(nodeStmts(n));             (* generate loop body *)
    writeBranch(testLabel);             (* branch back to test *)
    writeLabel(endLabel);               (* end label *)
END genWhileStmt;

PROCEDURE genAssignStmt(n:node);
BEGIN
    IF Symbol.class(baseType(nodeType(nodeLHS(n)))) = ArrayType THEN
       (* RHS had better be an array type too, or a string const *)
        genIndex(nodeRHS(n));
        genIndex(nodeLHS(n));
        computeSize(nodeRHS(n), nodeLHS(n));
        writeCopy;
    ELSE
        genExpr(nodeRHS(n));             (* generate the expression *)
        genScalarAssign(nodeLHS(n));
    END;
END genAssignStmt;

PROCEDURE genScalarAssign(n:node);
(* generate code to assign top of stack to n *)
BEGIN
    IF nodeClass(n) = nIndex THEN
        genIndex(n);
        writePop;
    ELSIF Symbol.class(baseType(nodeType(n))) = ArrayType THEN  (* error *)
        fatal('genScalarAssign: array type given');
    ELSE  (* a scalar variable *)
        writeSymPop(nodeSymbol(n));      (* pop result into the variable *)
    END;
END genScalarAssign;

PROCEDURE computeSize(source, dest:node);
(* Size for an array copy is tricky.  The rules are:
    1. If the size of source and dest are known (i.e. they are not open
       arrays), then take the min.
    2. If the size of one or both isn't known, compute the min at runtime. *)
VAR stype, dtype:symbol;
BEGIN
    stype := baseType(nodeType(source));
    dtype := baseType(nodeType(dest));
    IF Symbol.open(stype) OR Symbol.open(dtype) THEN
        genSize(source, stype);
        genSize(dest, dtype);
        writeMin;  (* MIN instruction added for this purpose *)
    ELSIF nodeClass(source) = nString THEN
        IF nstringLen(source) + 1 < Symbol.size(dtype) THEN
            genSize(source, stype);
        ELSE
            genSize(dest, dtype);
        END;
    ELSIF NOT Symbol.equal(stype, dtype) THEN
        fatal('computeSize: not same type');
    ELSE
        genSize(source, stype);
    END;
END computeSize;

PROCEDURE genSize(n:node; ntype:symbol);
(* generate code to put size of n on stack; n must be of type array. *)
BEGIN
    IF Symbol.open(ntype) THEN (* size = highBound - lowBound + 1 *)
        genHighBound(n);
        genLowBound(n);
        writeOp(Minus);
        writeInt(1);
        writeOp(Plus);
    ELSIF nodeClass(n) = nString THEN (* string const *)
        writeInt(INTEGER(nstringLen(n) + 1));
         (* Important: +1 for null at end *)
    ELSE (* a non-open array variable *)
        writeInt(INTEGER(Symbol.size(ntype)));
    END;
END genSize;

PROCEDURE genCallStmt(n:node);
BEGIN
    (* Here we do the special checks for the pseudofunctions LOW and HIGH *)
    IF Symbol.equal(nodeRoutine(n), lowFunc) THEN
        genLowBound(nodeFirst(nodeArgs(n)));
    ELSIF Symbol.equal(nodeRoutine(n), highFunc) THEN
        genHighBound(nodeFirst(nodeArgs(n)));
    ELSE  (* a "real" function *)
        genArgs(nodeRoutine(n), nodeArgs(n)); (* generate the arguments *)
        writeCall(nodeRoutine(n));       (* generate a call instruction *)
    END;
END genCallStmt;

PROCEDURE genArgs(routine:symbol; arglist:node);
(* Iterate down the arglist and the list of the routine's formals,
   and generate code for each argument *)
VAR formlist:symbolList;
BEGIN
    formlist := Symbol.formals(routine);
       (* assume number of formals = number of args *)
    WHILE NOT nodeEmpty(arglist) DO
        genArg(nodeFirst(arglist), slSymbol(formlist));
        arglist := nodeRest(arglist);
        formlist := slNext(formlist);
    END;
END genArgs;

PROCEDURE genArg(arg:node; formal:symbol);
(* Generate code to push the argument on the stack, as follows:
   1. Argument is an array:
     1a. If formal is open array, push high bound, low bound;
     1b. Push starting address, regardless
   2. Argument is a scalar:
     2a. If formal has mode IN, do the usual thing: genExpr.
     2b. Otherwise, push the address of the scalar.
*)
VAR argType:symbol;
BEGIN
    argType := baseType(nodeType(arg));
    IF Symbol.class(argType) = ArrayType THEN
        IF Symbol.open(Symbol.type(formal)) THEN
            genHighBound(arg);
            genLowBound(arg);
        END;
        genIndex(arg);
    ELSIF Symbol.class(argType) = ScalarType THEN
        IF Symbol.mode(formal) = mIn THEN
            genExpr(arg);
        ELSE
            genIndex(arg);
        END;
    ELSE
        fatal('genArg: argtype not a type object');
    END;
END genArg;


PROCEDURE genWriteStmt(n:node);
(* Generate code to write the arguments to the screen.  WRITE can take any
   number of arguments. *)
VAR arglist:node;
    argType:symbol;
BEGIN
    arglist := nodeArgs(n);
    WHILE NOT nodeEmpty(arglist) DO
        genExpr(nodeFirst(arglist));
        argType := baseType(nodeType(nodeFirst(arglist)));
        IF Symbol.equal(argType, tInteger) THEN
            writeWriteInt;
        ELSE (* it's a char *)
            writeWriteChar;
        END;
        arglist := nodeRest(arglist);
    END;
END genWriteStmt;

PROCEDURE genReadStmt(n:node);
(* Generate code to read from the terminal.  READ can take any number of
   arguments. *)
VAR arglist:node;
    argType:symbol;
BEGIN
    arglist := nodeArgs(n);
    WHILE NOT nodeEmpty(arglist) DO
        argType := baseType(nodeType(nodeFirst(arglist)));
        IF Symbol.equal(argType, tInteger) THEN
            writeReadInt;
            genScalarAssign(nodeFirst(arglist));
        ELSE (* a char *)
            writeReadChar;
            genScalarAssign(nodeFirst(arglist));
        END;
        arglist := nodeRest(arglist);
    END;
END genReadStmt;
    
PROCEDURE genReturnStmt(n:node);
BEGIN
    IF nodeEmpty(nodeExpr(n)) THEN  (* a procedure return *)
        writeReturn(nodeNumFormals(n));
    ELSE                            (* a function return *)
        genExpr(nodeExpr(n));
        writeFReturn(nodeNumFormals(n));
    END;
END genReturnStmt;

                (*** expressions ***)

PROCEDURE genExpr(n:node);
BEGIN
    CASE nodeClass(n) OF
        nUnop:  genExpr(nodeArg(n));
                writeOp(nodeOp(n));
    |   nOp:    IF (nodeOp(n) = And) OR (nodeOp(n) = Or) THEN
                    genLogicalOp(n);
                ELSE
                    genExpr(nodeLeftArg(n));
                    genExpr(nodeRightArg(n));
                    writeOp(nodeOp(n));
                END;
    |   nInt:   writeInt(nodeInt(n));
    |   nBool:  writeBool(nodeBool(n));
    |   nChar:  writeChar(nodeChar(n));
    |   nSymbol:writeSymbol(nodeSymbol(n));
    |   nCall:  genCallStmt(n);
    |   nIndex: genIndex(n);
                writeContents;
    ELSE       MyTerminal.fatal("genExpr: unknown expression type");
    END;
END genExpr;

PROCEDURE genLogicalOp(n:node);
(* AND's and OR's end up here.  We generate code to evaluate only the first
   if possible.  *)
VAR label1, label2:label;
BEGIN
    label1 := newLabel();
    label2 := newLabel();
    genExpr(nodeLeftArg(n));
    IF nodeOp(n) = And THEN (* we branch to FALSE if the first was FALSE *)
        writeCondBranch(Equal, label1);
    ELSE (* it's OR; we branch to TRUE if the first was TRUE *)
        writeCondBranch(Greater, label1);
    END;
    genExpr(nodeRightArg(n)); (* if the first one failed to decide, the value
                                 of the 2nd is the value of the whole thing. *)
    writeBranch(label2);
    writeLabel(label1);
    writeBool(nodeOp(n) = Or);  (* write TRUE if OR, FALSE if AND *)
    writeLabel(label2);
END genLogicalOp;

PROCEDURE genIndex(n:node);
(* Generates an array index.  Ends up with element address on top of stack. 
   Works fine for whole arrays, too--puts the starting address on the stack.
   In fact, works fine for scalar symbols too!  And for strings. *)
VAR arrayVar:node;
BEGIN
    IF nodeClass(n) = nSymbol THEN
        writeAddr(nodeSymbol(n));
    ELSIF nodeClass(n) = nString THEN
        genString(n);
    ELSIF nodeClass(n) = nIndex THEN
        arrayVar := nodeArray(n);
        genIndex(arrayVar);
        genLowBound(arrayVar);
        genHighBound(arrayVar);
        genExpr(nodeIndex(n));
        writeAref(Symbol.size(baseType(nodeType(n))));
    ELSE
        fatal('genIndex: node not symbol, string or index');
    END;
END genIndex;
 
PROCEDURE genLowBound(arrayVar:node);
VAR arrayType:symbol;
BEGIN
    arrayType := baseType(nodeType(arrayVar));
    IF nodeClass(arrayVar) = nSymbol THEN
        IF Symbol.open(arrayType) THEN
            writeLow(nodeSymbol(arrayVar));
        ELSE
            writeInt(Symbol.lowBound(arrayType));
        END;
    ELSIF nodeClass(arrayVar) = nString THEN
        writeInt(0);
    ELSIF nodeClass(arrayVar) = nIndex THEN
        writeInt(Symbol.lowBound(arrayType));
    ELSE
        fatal('genLowBound: not symbol, string or index');
    END;
END genLowBound;

PROCEDURE genHighBound(arrayVar:node);
VAR arrayType:symbol;
BEGIN
    arrayType := baseType(nodeType(arrayVar));
    IF nodeClass(arrayVar) = nSymbol THEN
        IF Symbol.open(arrayType) THEN
            writeHigh(nodeSymbol(arrayVar));
        ELSE
            writeInt(Symbol.highBound(arrayType));
        END;
    ELSIF nodeClass(arrayVar) = nString THEN
        writeInt(nstringLen(arrayVar)-1);  (* does not include 0 at end *)
    ELSIF nodeClass(arrayVar) = nIndex THEN
        writeInt(Symbol.highBound(arrayType));
    ELSE
        fatal('genHighBound: not symbol or index');
    END;
END genHighBound;
 
PROCEDURE genString(n:node);
VAR lab:label;
    s:stringType;
BEGIN
    lab := newLabel();
    nodeString(n, s);
    recordString(lab, s);
END genString;

PROCEDURE nstringLen(n:node):CARDINAL;
VAR s:stringType;
BEGIN
    nodeString(n, s);
    RETURN stringLen(s);
END nstringLen;

BEGIN
END CodeGen.
