IMPLEMENTATION MODULE Parser;

(* Most of the parser for the SIMPL compiler.  It is a top-down, recursive
   descent parser. *)

FROM Token IMPORT token, tokenClass, isType, emptyTokenList,
    tlToken, tlNext, tlEmpty, addToTokenList, tokenList, freeTokenList,
    typeType, tokenClassToType;
FROM LexAn IMPORT getToken, getTokenClass, peekTokenClass, compError,
    ungetToken, tokenErrorCheck, getTokenErrorCheck;
FROM Symbol IMPORT symbol, scopeType, emptySymbol, SymbolClass, symbolEmpty,
    symbolClassEqual, symbolEqual;
FROM SymbolTable IMPORT enterGlobal, enterLocal, enterFormal, findSymbol;
FROM Node IMPORT node, emptyNode, makeStmtsNode, makeIfNode, makeWhileNode,
    makeReturnNode, makeAssignmentNode, makeExprListNode,
    makeCallNode, makeReadNode, makeWriteNode, nodeType;
FROM CodeGen IMPORT genBlock, genGlobal;
FROM CodeWrite IMPORT writeRoutineLabel, writeStringBranch, writeHalt;
FROM TypeChecker IMPORT boolCheck;
FROM ExprParser IMPORT expr;
FROM Routines IMPORT routines;
FROM MyTerminal IMPORT fatal;

VAR programName:symbol;


(* <program> ::= PROGRAM <id>; <vars> <routines> <block> . *)
PROCEDURE program;
VAR t:token;
    n:node;
BEGIN
    tokenErrorCheck(Program, 'keyword "PROGRAM" expected');
    getTokenErrorCheck(t, Identifier, 'name of program expected');
    IF t.class <> Identifier THEN
        t.string := "???";  (* if the program name isn't given, make one up *)
    END;
    programName := enterGlobal(t.string, Proc, tUnknown);
    writeStringBranch(t.string);
    tokenErrorCheck(Semicolon, 'semicolon expected');
    vars(Global, emptySymbol);
    routines;
    writeRoutineLabel(t.string);
    genBlock(block(programName));
    tokenErrorCheck(Period, 'period expected');
    tokenErrorCheck(EndOfInput, 'end of input expected');
    writeHalt;
END program;

                (*** variable declarations ***)

(* <vars> ::= <empty> | VAR <varlist> *)
PROCEDURE vars(scope:scopeType; routineName:symbol);
BEGIN
    IF getTokenClass() = Var THEN
        varlist(scope, routineName);
    ELSE
        ungetToken;
    END;
END vars;

(* <varlist> ::= <decl> | <decl> <varlist>
   We can recognize the end of a varlist by seeing if the next token is an
   identifier.  An Id indicates the varlist continues.  If it didn't we'd
   see a keyword: either Begin, Procedure or Function. *)
PROCEDURE varlist(scope:scopeType; routineName:symbol);
BEGIN
    decl(scope, routineName);
    IF peekTokenClass() = Identifier THEN
        varlist(scope, routineName);
    END;
END varlist;

(* <decl> ::= <idlist> : <type> ; 
   Declarations.  All the work of putting information about the variables into
   the symbol table is done here. *)
PROCEDURE decl(scope:scopeType; routineName:symbol);
VAR tl, tokenp:tokenList;
    t, id:token;
    tt:typeType;
BEGIN
    tl := idlist();
    tokenErrorCheck(Colon, 'colon expected');
    getToken(t);
    IF NOT isType(t.class) THEN
        compError('type name expected');
        tt := tUnknown;
        ungetToken;
    ELSE
        tt := tokenClassToType(t.class);
    END;
    tokenErrorCheck(Semicolon, 'semicolon expected');
    tokenp := tl;
    (* Enter the variables into the symbol table.  For globals, also generate
       the variables. *)
    WHILE NOT tlEmpty(tokenp) DO
        tlToken(tokenp, id);
        CASE scope OF
            Global: genGlobal(enterGlobal(id.string, Variable, tt));
        |   Local:  enterLocal(id.string, tt, routineName);
        ELSE
            fatal("decl: unknown scope type");
        END;
        tokenp := tlNext(tokenp);
    END;
    freeTokenList(tl);
END decl;

(* <idlist> ::= <id> | <id> , <idlist> *)
PROCEDURE idlist():tokenList;
VAR t: token;
BEGIN
    getTokenErrorCheck(t, Identifier, 'identifier expected');
    IF getTokenClass() <> Comma THEN (* this is the end of the idlist *)
        ungetToken;
        IF t.class = Identifier THEN
            RETURN addToTokenList(t, emptyTokenList);
        ELSE
            RETURN emptyTokenList;
        END;
    (* we saw a comma, so there's more *)
    ELSIF t.class = Identifier THEN
        RETURN addToTokenList(t, idlist());
    ELSE
        RETURN idlist();
    END;
END idlist;


                (*** blocks and statements ***)

(* <block> ::= BEGIN <stmts> END *)
PROCEDURE block(routine:symbol):node;
VAR n:node;
BEGIN
    tokenErrorCheck(Begin, 'BEGIN expected');
    n := stmts(routine);
    tokenErrorCheck(End, '"END" expected');
    RETURN n;
END block;

(* <stmts> ::= <empty> | <stmt> ; <stmts>
   We can recognize an empty <stmts> by seeing if the next token is ELSE,
   ELSIF or END. *)
PROCEDURE stmts(routine:symbol):node;
VAR n:node;
    tc:tokenClass;
BEGIN
    tc := peekTokenClass();
    IF (tc = Else) OR (tc = Elsif) OR (tc = End) THEN
        RETURN emptyNode;
    ELSE
        n := stmt(routine);
        tokenErrorCheck(Semicolon, 'a semicolon must end a statement');
        RETURN makeStmtsNode(n, stmts(routine));
    END;
END stmts;

(* <stmt> ::= <while> | <if> | <return> | <assign> | <call> *)
PROCEDURE stmt(routine:symbol):node;
VAR t:token;
BEGIN
    getToken(t);
    CASE t.class OF
        If: RETURN ifStmt(routine);
    |   While: RETURN whileStmt(routine);
    |   Return: RETURN returnStmt(routine);
            IF symbolEqual(routine, programName) THEN
                compError("can't return from main program");
            ELSE
                RETURN makeReturnNode(routine, expr());
            END;
    |   Write: RETURN makeWriteNode(actuals());
    |   Read: RETURN makeReadNode(actuals());
    |   Identifier: RETURN assignOrCallStmt(t);
    ELSE
        compError('illegal statement type');
        RETURN emptyNode;
    END;
END stmt;
  

(* <if> ::= IF <elsif> END *)
PROCEDURE ifStmt(routine:symbol):node;
VAR n:node;
BEGIN
    n := elsif(routine);
    tokenErrorCheck(End, 'END expected');
    RETURN n;
END ifStmt;
          
(* <elsif> ::= <expr> THEN <stmts> <else> *)
PROCEDURE elsif(routine:symbol):node;
VAR n1, n2:node;
BEGIN
    n1 := expr();
    boolCheck(n1);
    tokenErrorCheck(Then, 'THEN expected');
    n2 := stmts(routine);
    RETURN makeIfNode(n1, n2, else(routine));        
END elsif;

(* <else> ::= <empty> | ELSIF <elsif> | ELSE <stmts> 
   We can tell an <else> is empty by seeing if the next token is END. *)
PROCEDURE else(routine:symbol):node;
BEGIN
    CASE getTokenClass() OF
        End:   ungetToken;
               RETURN emptyNode;
    |   Elsif: RETURN makeStmtsNode(elsif(routine), emptyNode);
    |   Else:  RETURN stmts(routine);
    ELSE
        compError('END, ELSIF or ELSE expected');
        ungetToken;
        RETURN emptyNode;
    END;
END else;

(* <while> ::= WHILE <expr> DO <stmts> END *)
PROCEDURE whileStmt(routine:symbol):node;
VAR n:node;
BEGIN
    n := expr();
    boolCheck(n);
    tokenErrorCheck(Do, 'DO expected');
    n := makeWhileNode(n, stmts(routine));
    tokenErrorCheck(End, 'END expected');
    RETURN n;
END whileStmt;

(* <return> ::= RETURN | RETURN <expr> *)
PROCEDURE returnStmt(routine:symbol):node;
BEGIN
    IF symbolEqual(routine, programName) THEN
        compError("can't return from main program");
    END;
    IF peekTokenClass() = Semicolon THEN
        RETURN makeReturnNode(routine, emptyNode);
    ELSE
        RETURN makeReturnNode(routine, expr());
    END;
END returnStmt;
 
(* We can't distinguish an assignment from a call based on the first token
   of the statement, since in both cases it's an identifier.  The next token,
   though, will distinguish: it's an assignment sign for an assignment. *)
PROCEDURE assignOrCallStmt(t:token):node;
BEGIN
    IF getTokenClass() = Assignment THEN
        RETURN assignStmt(t);
    ELSE
        ungetToken;
        RETURN callStmt(t);
    END;
END assignOrCallStmt;

(* <assign> ::= <id> := <expr> *)
PROCEDURE assignStmt(varName:token):node;
VAR s:symbol;
BEGIN
    s := findSymbol(varName.string);
    IF NOT symbolClassEqual(s, Variable) THEN
        compError('only variables can be assigned to');
        RETURN expr();   (* consume the expression anyway *)
    ELSE
        RETURN makeAssignmentNode(s, expr());
    END;
END assignStmt;

(* <call> ::= <id> <actuals> *)
PROCEDURE callStmt(routineName:token):node;
VAR proc:symbol;
BEGIN
    proc := findSymbol(routineName.string);
    IF NOT symbolClassEqual(proc, Proc) THEN
        compError('only procedures can be used in a call statement');
        RETURN actuals();
    ELSE
        RETURN makeCallNode(proc, actuals());
    END;
END callStmt;

(* <actuals> ::= <empty> | ( <exprlist> )  
   We can recognize an empty <actuals> by seeing if the next character is a
   left parenthesis. *)
PROCEDURE actuals():node;
VAR n:node;
BEGIN
    IF getTokenClass() = Lparen THEN
        n := exprlist();
        tokenErrorCheck(Rparen, 'right paren expected');
        RETURN n;
    ELSE
        ungetToken;
        RETURN emptyNode;
    END;
END actuals;

(* <exprlist> ::= <expr> | <expr> , <exprlist> 
   Exprlist always returns an nList node, even if there's only one expr. *)
PROCEDURE exprlist():node;
VAR n:node;
BEGIN
    n := expr();
    IF getTokenClass() = Comma THEN
        RETURN makeExprListNode(n, exprlist());
    ELSE 
        ungetToken;
        RETURN makeExprListNode(n, emptyNode);
    END;
END exprlist;

BEGIN
END Parser.
