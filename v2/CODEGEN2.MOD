IMPLEMENTATION MODULE CodeGen;

(* Code Generator for the SIMPL compiler. *)

IMPORT MyTerminal;
FROM InOut IMPORT WriteString, WriteLn;
FROM Node IMPORT node, nodeClass, NodeClass, nodeEmpty, nodeFirst, nodeRest,
        nodeTest, nodeThen, nodeElse, nodeStmts, nodeRHS, nodeLHS, nodeArgs,
        nodeRoutine, nodeExpr, nodeArg, nodeLeftArg, nodeRightArg, nodeOp,
        nodeSymbol, nodeInt, nodeBool, nodeNumFormals, nodeChar,
        nodeType, freeNode;
FROM CodeWrite IMPORT writeLabel, writeStringLabel, writeStringBranch,
        writeCondBranch, writeBranch, writePop, writeCall, writeChar,
        writeWriteInt, writeInt, writeReadInt, writeReturn, writeFReturn,
        writeOp, writeBool, writeSymbol, writeWriteChar, writeReadChar;
FROM Token IMPORT tokenClass, isRelation, stringType, typeType;
FROM LexAn IMPORT errorFlag;
FROM Symbol IMPORT symbol, symbolLexLevel, symbolString, numLocals;

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
   to FALSE (which is also zero), chars to NUL (which is again zero). *)
VAR name:stringType;
BEGIN
    symbolString(s, name);
    IF symbolLexLevel(s) = 0 THEN
        writeStringLabel(name);
        WriteString("  0");
        WriteLn;
    ELSE
        MyTerminal.WriteString("genGlobal: not a global: ");
        MyTerminal.fatal(name);
    END;
END genGlobal;

PROCEDURE genLocals(routine:symbol);
(* put locals on stack, initialized to 0 (or FALSE, or NUL) *)
VAR i:CARDINAL;
BEGIN
    FOR i := 1 TO numLocals(routine) DO
        writeInt(0);
    END;
END genLocals;

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
    writeCondBranch(Equal, label1);     (* branch to else part if test false
*)
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
    genExpr(nodeRHS(n));                (* generate the expression *)
    writePop(nodeLHS(n));               (* pop the result into the variable *)
END genAssignStmt;

PROCEDURE genCallStmt(n:node);
BEGIN
    genExprList(nodeArgs(n));           (* generate the arguments *)
    writeCall(nodeRoutine(n));          (* generate a call instruction *)
END genCallStmt;

PROCEDURE genWriteStmt(n:node);
(* Generate code to write the arguments to the screen.  WRITE can take any
   number of arguments. *)
VAR arglist:node;
BEGIN
    arglist := nodeArgs(n);
    WHILE NOT nodeEmpty(arglist) DO
        genExpr(nodeFirst(arglist));
        CASE nodeType(nodeFirst(arglist)) OF
            tInteger: writeWriteInt;
        |   tChar:    writeWriteChar;
        ELSE
            MyTerminal.fatal("genWriteStmt: illegal type");
        END;
        arglist := nodeRest(arglist);
    END;
END genWriteStmt;

PROCEDURE genReadStmt(n:node);
(* Generate code to read from the terminal.  READ can take any number of
   arguments. *)
VAR arglist:node;
BEGIN
    arglist := nodeArgs(n);
    WHILE NOT nodeEmpty(arglist) DO
        CASE nodeType(nodeFirst(arglist)) OF
            tInteger: writeReadInt;
        |   tChar:    writeReadChar;
        ELSE
            MyTerminal.fatal("genReadStmt: illegal type");
        END;
        writePop(nodeSymbol(nodeFirst(arglist)));
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

PROCEDURE genExprList(n:node);
VAR el:node;
BEGIN
    el := n;
    WHILE NOT nodeEmpty(el) DO
        genExpr(nodeFirst(el));
        el := nodeRest(el);
    END;
END genExprList;

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
                                 of the 2nd is the value of the whole thing.
*)
    writeBranch(label2);
    writeLabel(label1);
    writeBool(nodeOp(n) = Or);  (* write TRUE if OR, FALSE if AND *)
    writeLabel(label2);
END genLogicalOp;


BEGIN
END CodeGen.
