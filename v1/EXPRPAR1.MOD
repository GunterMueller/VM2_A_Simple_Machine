IMPLEMENTATION MODULE ExprParser;

(* Handles parsing of expressions, which are tricky because we have to
   make the operators left-associative, whereas the normal recursive descent
   grammar would have them be right-associative. 
      The problem is that the trees are build from the right.  To make
   them get built from the left, we pass to expr, relexpr, intexpr and term
   the partial tree constructed from the left, and each of these procedures
   hooks that tree on to the one it parses in the appropriate way. *)

FROM Token IMPORT token, tokenClass, isRelation;
FROM LexAn IMPORT getToken, ungetToken, tokenErrorCheck, compError;
FROM Symbol IMPORT symbol, SymbolClass, symbolClassEqual; 
FROM SymbolTable IMPORT findSymbol;
FROM Node IMPORT node, emptyNode, makeOpNode, makeUnopNode, makeIntegerNode,
    makeBooleanNode, makeSymbolNode, makeCallNode, makeStringNode, nodeEmpty,
    makeCharNode;
FROM Parser IMPORT actuals;

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
                ( <expr> )  *)
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
    |   Identifier: RETURN callOrId(t);
    ELSE
        compError('bad factor');
        RETURN emptyNode;
    END;
END factor;

PROCEDURE callOrId(t:token):node;
VAR s:symbol;
BEGIN
    s := findSymbol(t.string);
    IF symbolClassEqual(s, Func) THEN
        RETURN makeCallNode(s, actuals());
    ELSIF symbolClassEqual(s, Proc) THEN
        compError('procedures cannot be used in an expression');
        RETURN emptyNode;
    ELSE (* it's a variable *)
        RETURN makeSymbolNode(s);
    END;
END callOrId;

PROCEDURE buildTree(op:tokenClass; n1, n2:node):node;
(* This is the key hack that builds trees up from the left if necessary. *)
BEGIN
    IF nodeEmpty(n1) THEN
        RETURN n2;
    ELSE
        RETURN makeOpNode(op, n1, n2);
    END;
END buildTree;

BEGIN
END ExprParser.
