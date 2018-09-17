IMPLEMENTATION MODULE ExprParser;

(* Handles parsing of expressions, which are tricky because we have to
   make the operators left-associative, whereas the normal recursive descent
   grammar would have them be right-associative. 
      The problem is that the trees are build from the right.  To make
   them get built from the left, we pass to expr, relexpr, intexpr and term
   the partial tree constructed from the left, and each of these procedures
   hooks that tree on to the one it parses in the appropriate way.

 Changes for part 3:
    1.  Type coercion functions are handled.
    2.  Strings handled.
    3.  Array indexing handled.
 *)

FROM Token IMPORT token, tokenClass, isRelation;
FROM LexAn IMPORT getToken, ungetToken, tokenErrorCheck, compError,
        getTokenClass, peekTokenClass;
FROM Symbol IMPORT symbol, Class, modeType;
IMPORT Symbol; 
FROM SymbolTable IMPORT findSymbol, tInteger;
FROM Node IMPORT node, emptyNode, makeOpNode, makeUnopNode, makeIntegerNode,
    makeBooleanNode, makeSymbolNode, makeCallNode, makeStringNode, nodeEmpty,
    makeCharNode, nodeFirst, nodeRest, makeIndexNode, setNodeType, nodeType;
FROM Parser IMPORT actuals;
FROM TypeChecker IMPORT indexCheck;

CONST dummy = Period;

(* <expr>::= <expr>|<relexpr> | <relexpr> OR <expr> | <relexpr> AND <expr> *)
PROCEDURE expr():node;
BEGIN
    RETURN expr1(emptyNode, dummy);
END expr;

PROCEDURE expr1(left:node; op:tokenClass):node;
VAR n:node;
    t:token;
BEGIN
    n := relexpr();
    getToken(t);
    IF (t.class = And) OR (t.class = Or) THEN
        RETURN expr1(buildTree(op, left, n), t.class);
    ELSE
        ungetToken;
        IF nodeEmpty(left) THEN
            RETURN n;
        ELSE
            RETURN makeOpNode(op, left, n);
        END;
    END;
END expr1;

(* <relexpr> ::= <intexpr> | <intexpr> <relation> <intexpr> *)
PROCEDURE relexpr():node;
(* Here we don't have to worry about associativity since relations aren't
   associative!  *)
VAR n:node;
    t:token;
BEGIN
    n := intexpr(emptyNode, dummy);
    getToken(t);
    IF isRelation(t.class) THEN
        RETURN makeOpNode(t.class, n, intexpr(emptyNode, dummy));
    ELSE
        ungetToken;
        RETURN n;
    END;
END relexpr;

(* <intexpr> ::= <term> | <term> + <intexpr> | <term> - <intexpr> *)
PROCEDURE intexpr(left:node; op:tokenClass):node;
VAR n:node;
    t:token;
BEGIN
    n := term(emptyNode, dummy);
    getToken(t);
    IF (t.class = Plus) OR (t.class = Minus) THEN
        RETURN intexpr(buildTree(op, left, n), t.class);
    ELSE
        ungetToken;
        IF nodeEmpty(left) THEN
            RETURN n;
        ELSE
            RETURN makeOpNode(op, left, n);
        END;
    END;
END intexpr;

(* <term> ::= <factor> | <factor> * <term> | <factor> / <term> *)
PROCEDURE term(left:node; op:tokenClass):node;
VAR n:node;
    t:token;
BEGIN
    n := factor();
    getToken(t);
    IF (t.class = Times) OR (t.class = Divide) THEN
        RETURN term(buildTree(op, left, n), t.class);
    ELSE
        ungetToken;
        IF nodeEmpty(left) THEN
            RETURN n;
        ELSE
            RETURN makeOpNode(op, left, n);
        END;
    END;
END term;

(* <factor> ::= <id> | <number> | <call> | <char> | - <factor> | NOT <factor> |
                ( <expr> ) | <string> | <index> *)
PROCEDURE factor():node;
VAR n:node;
    t:token;
BEGIN
    getToken(t);
    CASE t.class OF
        Int: RETURN makeIntegerNode(t.integer);
    |   Character: RETURN makeCharNode(t.ch);
    |   String: RETURN makeStringNode(t.string);
    |   Minus: RETURN makeUnopNode(UMinus, factor());
    |   Not: RETURN makeUnopNode(Not, factor());
    |   True: RETURN makeBooleanNode(TRUE);
    |   False: RETURN makeBooleanNode(FALSE);
    |   Lparen: 
            n := expr();
            tokenErrorCheck(Rparen, 'Right paren expeced');
            RETURN n;
    |   Identifier: RETURN Id(t);
    ELSE
        compError('bad factor');
        RETURN emptyNode;
    END;
END factor;

PROCEDURE Id(t:token):node;
VAR s:symbol;
BEGIN
    s := findSymbol(t.string);
    CASE Symbol.class(s) OF
        Func:   RETURN makeCallNode(s, actuals());
    |   Proc:   compError('procedures cannot be used in an expression');
                RETURN emptyNode;
    |   ArrayType, ScalarType: RETURN typeCoerce(s, actuals());
    |   Global, Local, Formal: (* it's a variable or index*)
            IF (Symbol.class(s) = Formal) AND (Symbol.mode(s) = mOut) THEN
                compError("can't use an OUT formal in an expression");
            END;
            RETURN idOrIndex(s);
    ELSE (* ignore *)
        RETURN emptyNode;
    END;
END Id;

(* <idOrIndex> ::= <id> | <idOrIndex> [ <expr> ]
   That's the "official" syntax.  It's easier to treat it as:
    <idOrIndex> ::= <id> <indices>
    <indices> ::= <empty> | [ <exprOrExprlist> ] <indices>
   The beginning <id> has already been read and looked up. *)
PROCEDURE idOrIndex(s:symbol):node;
BEGIN
    IF (peekTokenClass() = Lbracket) AND
       (Symbol.class(Symbol.type(s)) <> ArrayType) THEN
        compError("array variable expected");
    END;
    RETURN indices(makeSymbolNode(s));
END idOrIndex;

PROCEDURE indices(n:node):node;
BEGIN
    IF getTokenClass() = Lbracket THEN (* array index *)
								RETURN indices1(n);
    ELSE (* no index *)
        ungetToken;
        RETURN n;
    END;
END indices;

PROCEDURE indices1(n:node):node;
(* Like indices, except it starts with expression instead of Lbracket *)
VAR exp:node;
    tc:tokenClass;
BEGIN
    IF Symbol.class(nodeType(n)) <> ArrayType THEN
        compError("too many indices for array");
    END;
        exp := expr();
        indexCheck(exp);
        tc := getTokenClass();
        IF tc = Rbracket THEN
            RETURN indices(makeIndexNode(n, exp));
        ELSIF tc = Comma THEN
            RETURN indices1(makeIndexNode(n, exp));
        ELSE
            compError("right bracket or comma expected");
        RETURN emptyNode;
    END;
END indices1;

        
PROCEDURE buildTree(op:tokenClass; n1, n2:node):node;
(* This is the key hack that builds trees up from the left if necessary. *)
BEGIN
    IF nodeEmpty(n1) THEN
        RETURN n2;
    ELSE
        RETURN makeOpNode(op, n1, n2);
    END;
END buildTree;

PROCEDURE typeCoerce(typeObject:symbol; actual:node):node;
(* Changes the type of the actual to typeObject.  actual should be a list
   of one thing. The actual's type should have the same size as the 
   typeObject. Note that you can do some pretty evil things with this
   ability, like transforming an array[1..10][1..2] into an 
   array[1..2][1..10]. *)
BEGIN
    IF nodeEmpty(actual) THEN
        compError('type coercion functions must take an argument');
        RETURN actual;
    ELSE
        IF NOT nodeEmpty(nodeRest(actual)) THEN
            compError('type coercion functions take only one argument');
            (* ...but we'll set the first argument anyway *)
        END;
        IF Symbol.size(typeObject) <> Symbol.size(nodeType(nodeFirst(actual)))
            THEN compError('types not of same size');
        END;
        setNodeType(nodeFirst(actual), typeObject);
        RETURN nodeFirst(actual);
    END;
END typeCoerce;

BEGIN
END ExprParser.
