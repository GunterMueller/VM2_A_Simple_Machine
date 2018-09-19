IMPLEMENTATION MODULE Parser;

(* Most of the parser for the SIMPL compiler.  It is a top-down, recursive
   descent parser.

  Changes for part 3:
    1. Type declarations and arrays are parsed.
    2. Type objects used instead of the old typeType.
	
*)

FROM Token IMPORT token, tokenClass, emptyTokenList, stringType,
    tlToken, tlNext, tlEmpty, addToTokenList, tokenList, freeTokenList;
FROM LexAn IMPORT getToken, getTokenClass, peekTokenClass, compError,
    ungetToken, tokenErrorCheck, getTokenErrorCheck;
FROM Symbol IMPORT symbol, emptySymbol, isType, Class;
IMPORT Symbol;
FROM SymbolTable IMPORT enterSymbol, enterLocal, enterFormal, findSymbol,
    currentLexLevel, tUnknown, enterArrayType;
FROM Node IMPORT node, emptyNode, makeStmtsNode, makeIfNode, makeWhileNode,
    makeReturnNode, makeAssignmentNode, makeExprListNode,
    makeCallNode, makeReadNode, makeWriteNode, nodeType, makeSymbolNode;
FROM CodeGen IMPORT genBlock, genGlobal;
FROM CodeWrite IMPORT writeStringBranch, writeHalt, writeRoutineLabel,
    writeStrings;
FROM TypeChecker IMPORT boolCheck, assignable;
FROM ExprParser IMPORT expr, idOrIndex;
FROM Routines IMPORT routines;
FROM MyTerminal IMPORT fatal;

VAR programName:symbol;


(* <program> ::= PROGRAM <id>; <types> <vars> <routines> <block> . *)
PROCEDURE program;
VAR t:token;
    n:node;
BEGIN
    tokenErrorCheck(Program, 'keyword "PROGRAM" expected');
    getTokenErrorCheck(t, Identifier, 'name of program expected');
    IF t.class <> Identifier THEN
        t.string := "???";  (* if the program name isn't given, make one up *)
    END;
    programName := enterSymbol(t.string, Proc, tUnknown);
    writeStringBranch(t.string);
    tokenErrorCheck(Semicolon, 'semicolon expected');
    types(emptySymbol);
    vars(emptySymbol);
    routines;
    writeRoutineLabel(programName);
    genBlock(block(programName));
    tokenErrorCheck(Period, 'period expected');
    tokenErrorCheck(EndOfInput, 'end of input expected');
    writeHalt;
    writeStrings;
END program;

		(*** type declarations ***)
		
(* <types> ::= <empty> | TYPE <typelist> *)
PROCEDURE types(routineName:symbol);
BEGIN
    IF getTokenClass() = Type THEN
        typelist(routineName);
    ELSE
        ungetToken;
    END;
END types;

(* <typelist> ::= <typedecl> | <typedecl> <typelist> *)
PROCEDURE typelist(routineName:symbol);
BEGIN
    typedecl(routineName);
    IF peekTokenClass() = Identifier THEN
        typelist(routineName);
    END;
END typelist;

(* <typedecl> ::= <id> = <type> ; *)
PROCEDURE typedecl(routineName:symbol);
VAR typeToken:token;
    typeObject:symbol;
BEGIN
    getToken(typeToken);
    IF typeToken.class <> Identifier THEN
        compError('identifier expected');
        ungetToken;
        typeToken.string := '???';  (* make up a type name *)
    END;
    tokenErrorCheck(Equal, 'equal sign expected');
    typeObject := type();
    IF NOT Symbol.empty(typeObject) THEN
        IF Symbol.class(typeObject) = ArrayType THEN
            handleArrayType(typeToken.string, typeObject);
        ELSIF Symbol.class(typeObject) = ScalarType THEN
            typeObject := enterSymbol(typeToken.string, ScalarType,typeObject);
        ELSE
            (* do nothing--error caught in type() *)
        END;
    END;
    tokenErrorCheck(Semicolon, "semicolon expected");
END typedecl;

PROCEDURE handleArrayType(VAR s:stringType; typeObject:symbol);
(* If the array type hasn't been named, name it and insert it; else,
   copy it and give the copy the new name. *)
VAR typeName:stringType;
BEGIN
    IF Symbol.anonymous(typeObject) THEN
        enterArrayType(s, typeObject);
    ELSE
        enterArrayType(s, Symbol.copyArrayType(typeObject));
    END;
END handleArrayType;

(* <type> ::= <id> | ARRAY [ Int .. Int ] OF <type> *)
PROCEDURE type():symbol;
VAR t:token;
    typeSymbol:symbol;
BEGIN
    getToken(t);
    IF t.class = Identifier THEN
        typeSymbol := findSymbol(t.string);
        IF isType(typeSymbol) THEN
            RETURN typeSymbol;
        ELSE
            compError('not a type');
            RETURN tUnknown;
        END;
    ELSIF t.class = Array THEN
        tokenErrorCheck(Lbracket, "left bracket expected");
        RETURN type1();
    ELSE
        compError("Identifier or ARRAY expected");
        ungetToken;
        RETURN tUnknown;
    END;
END type;

PROCEDURE type1():symbol;
VAR t:token;
    lowBound, highBound, temp:INTEGER;
BEGIN
    lowBound := getBound();
    tokenErrorCheck(DotDot, 'two dots expected');
    highBound := getBound();
    IF lowBound > highBound THEN
        compError("lowBound > highBound");
        temp := lowBound;
        lowBound := highBound;
        highBound := temp;
    END;
    getToken(t);
    IF t.class = Comma THEN
        RETURN Symbol.newArrayType(type1(), lowBound, highBound, FALSE,
                                   currentLexLevel());
    ELSIF t.class = Rbracket THEN
        tokenErrorCheck(Of, 'OF expected');
        RETURN Symbol.newArrayType(type(), lowBound, highBound, FALSE,
		         currentLexLevel());
    ELSE
        compError("Comma or right bracket expected");                  
		      ungetToken;
        RETURN tUnknown;
    END;
END type1;

PROCEDURE getBound():INTEGER;
(* Reads in an integer, possibly negative. *)
VAR t:token;
BEGIN
    getToken(t);
    IF t.class = Int THEN
        RETURN t.integer;
    ELSIF t.class = Minus THEN
        getTokenErrorCheck(t, Int, "integer expected");
        RETURN -t.integer;
    ELSE
        ungetToken;
        compError("integer expected");
        RETURN 0;
    END;
END getBound;
	
                (*** variable declarations ***)

(* <vars> ::= <empty> | VAR <varlist> *)
PROCEDURE vars(routineName:symbol);
BEGIN
    IF getTokenClass() = Var THEN
        varlist(routineName);
    ELSE
        ungetToken;
    END;
END vars;

(* <varlist> ::= <decl> | <decl> <varlist>
   We can recognize the end of a varlist by seeing if the next token is an
   identifier.  An Id indicates the varlist continues.  If it didn't we'd
   see a keyword: either Begin, Procedure or Function. *)
PROCEDURE varlist(routineName:symbol);
BEGIN
    decl(routineName);
    IF peekTokenClass() = Identifier THEN
        varlist(routineName);
    END;
END varlist;

(* <decl> ::= <idlist> : <type> ; 
   Declarations.  All the work of putting information about the variables into
   the symbol table is done here. *)
PROCEDURE decl(routineName:symbol);
VAR tl, tokenp:tokenList;
    t, id:token;
	typeSymbol:symbol;
BEGIN
    tl := idlist();
    tokenErrorCheck(Colon, 'colon expected');
    typeSymbol := type();
    tokenErrorCheck(Semicolon, 'semicolon expected');
    tokenp := tl;
    (* Enter the variables into the symbol table.  For globals, also generate
       the variables. *)
    WHILE NOT tlEmpty(tokenp) DO
        tlToken(tokenp, id);
        IF currentLexLevel() = 0 THEN
            genGlobal(enterSymbol(id.string, Global, typeSymbol));
        ELSE
            enterLocal(id.string, typeSymbol, routineName);
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

(* <stmt> ::= <while> | <if> | <return> | <assign> | <call> |
              <write> | <read> *)
PROCEDURE stmt(routine:symbol):node;
VAR t:token;
BEGIN
    getToken(t);
    CASE t.class OF
        If: RETURN ifStmt(routine);
    |   While: RETURN whileStmt(routine);
    |   Return: RETURN returnStmt(routine);
            IF Symbol.equal(routine, programName) THEN
                compError("can't return from main program");
            ELSE
                RETURN makeReturnNode(routine, expr());
            END;
    |   Write: RETURN makeWriteNode(actuals());
    |   Read: RETURN makeReadNode(readActuals());
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
    IF Symbol.equal(routine, programName) THEN
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
   though, will distinguish: it's an assignment sign or a left bracket
			for an assignment. *)
PROCEDURE assignOrCallStmt(t:token):node;
VAR tc:tokenClass;
BEGIN
    c := peekTokenClass();
    IF (tc = Assignment) OR (tc = Lbracket) THEN
        RETURN assignStmt(t);
    ELSE
        RETURN callStmt(t);
    END;
END assignOrCallStmt;

(* <assign> ::= <idOrIndex> := <expr> *)
PROCEDURE assignStmt(varName:token):node;
VAR n:node;
BEGIN
    n := idOrIndex(findSymbol(varName.string));
    IF NOT assignable(n) THEN
        compError('cannot assign to this');
        tokenErrorCheck(Assignment, ":= expected");
        RETURN expr();   (* consume the expression anyway *)
    ELSE
        tokenErrorCheck(Assignment, ":= expected");
        RETURN makeAssignmentNode(n, expr());
    END;
END assignStmt;

(* <call> ::= <id> <actuals> *)
PROCEDURE callStmt(routineName:token):node;
VAR proc:symbol;
BEGIN
    proc := findSymbol(routineName.string);
    IF NOT Symbol.classEqual(proc, Proc) THEN
        compError('only procedures can be used in a call statement');
        RETURN actuals();  (* consume the actuals anyway *)
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

(* These two procedures are for the args to READ *)
PROCEDURE readActuals():node;
VAR n:node;
BEGIN
    IF getTokenClass() = Lparen THEN
        n := readExprlist();
        tokenErrorCheck(Rparen, 'right paren expected');
        RETURN n;
    ELSE
        ungetToken;
        RETURN emptyNode;
    END;
END readActuals;


PROCEDURE readExprlist():node;
VAR t:token;
    n:node;
BEGIN
    getTokenErrorCheck(t, Identifier, "identifier expected");
    n := idOrIndex(findSymbol(t.string));
    IF getTokenClass() = Comma THEN
        RETURN makeExprListNode(n, readExprlist());
    ELSE 
        ungetToken;
        RETURN makeExprListNode(n, emptyNode);
    END;
END readExprlist;

BEGIN
END Parser.
