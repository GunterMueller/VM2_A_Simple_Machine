IMPLEMENTATION MODULE Debug;

FROM Token IMPORT token, tokenList, tokenClass, typeType, tlNext, tlToken,
    stringType;
FROM Symbol IMPORT symbol, SymbolClass, symbolClass, symbolType, symbolList,
    symbolString;
FROM Node IMPORT node, NodeClass, nodeClass, nodeEmpty, nodeType, nodeFirst,
    nodeRest, nodeArg, nodeArgs, nodeLeftArg, nodeRightArg, nodeInt,
    nodeBool, nodeSymbol, nodeRoutine, nodeTest, nodeStmts, nodeThen,
    nodeElse, nodeOp, nodeExpr, nodeLHS, nodeRHS, nodeNumFormals,
    nodeString, nodeChar;
FROM MyTerminal IMPORT WriteLnString, WriteCard, WriteInt, WriteString,
    WriteLn;
IMPORT InOut;

PROCEDURE printHeader(s:ARRAY OF CHAR);
BEGIN
    WriteString(s);
    WriteString(': ');
END printHeader;

PROCEDURE printType(tt:typeType);
BEGIN
    CASE tt OF
        tInteger: WriteString('tInteger');
    |   tBoolean: WriteString('tBoolean');
    |   tChar:    WriteString('tChar');
    |   tUnknown: WriteString('tUnknown');
    ELSE
        WriteString('*unknown type*');
    END;
END printType;

(*** tokens ***)

PROCEDURE printToken(VAR t:token);
BEGIN
    printHeader('TOKEN');
    printTokenClass(t.class);
    IF (t.class = Identifier) OR (t.class = String) THEN
        WriteString(': ');
        WriteString(t.string);
    ELSIF t.class = Int THEN
        WriteString(': ');
        WriteInt(t.integer, 0);
    END;
    WriteLn;
END printToken;

PROCEDURE printTokenClass(tc:tokenClass);
BEGIN
    CASE tc OF
        And: WriteString('And');
    |   Assignment:WriteString('Assignment');
    |   Begin: WriteString('Begin');
    |   Boolean: WriteString('Boolean');
    |   Char: WriteString("Char");
    |   Character: WriteString("Character");
    |   Colon: WriteString('Colon');
    |   Comma: WriteString('Comma');
    |   Divide: WriteString('Divide');
    |   Do: WriteString('Do');
    |   Else: WriteString('Else');
    |   Elsif: WriteString('Elsif');
    |   End: WriteString('End');
    |   EndOfInput: WriteString('EndOfInput');
    |   Equal: WriteString('Equal');
    |   False: WriteString('False');
    |   Function: WriteString('Function');
    |   Greater: WriteString('Greater');
    |   GreaterEqual: WriteString('GreaterEqual');
    |   Identifier: WriteString('Identifier');
    |   If: WriteString('If');
    |   Int: WriteString('Int');
    |   Integer: WriteString('Integer');
    |   Less: WriteString('Less');
    |   LessEqual: WriteString('LessEqual');
    |   Lparen: WriteString('Lparen');
    |   Minus: WriteString('Minus');
    |   Not: WriteString('Not');
    |   NotEqual: WriteString('NotEqual');
    |   Or: WriteString('Or');
    |   Period: WriteString('Period');
    |   Plus: WriteString('Plus');
    |   Procedure: WriteString('Procedure');
    |   Program: WriteString('Program');
    |   Read: WriteString('Read');
    |   Return: WriteString('Return');
    |   Rparen: WriteString('Rparen');
    |   Semicolon: WriteString('Semicolon');
    |   String: WriteString('String');
    |   Then: WriteString('Then');
    |   Times: WriteString('Times');
    |   True: WriteString('True');
    |   UMinus: WriteString('UMinus');
    |   Var: WriteString('Var');
    |   While: WriteString('While');
    |   Write: WriteString('Write');
    ELSE
        WriteString('*unknown class*');
    END;
END printTokenClass;


(*** token lists ***)

PROCEDURE printTokenList(tl:tokenList);
BEGIN
    printHeader('TOKEN LIST');
    WriteLn;
END printTokenList;


                (*** symbols ***)

PROCEDURE printSymbol(s:symbol);
BEGIN
    pSymbol(s, 0);
    WriteLn;
END printSymbol;

PROCEDURE pSymbol(s:symbol; depth:CARDINAL);
VAR i:CARDINAL;
    str:stringType;
BEGIN
    FOR i := 1 TO depth DO
        InOut.Write(' ');
    END;
    printHeader('SYMBOL');
    symbolString(s, str);
    WriteString(str);
    WriteString(": ");
    printSymbolClass(symbolClass(s));
    WriteString(", ");
    printType(symbolType(s));
END pSymbol;

PROCEDURE printSymbolClass(sc:SymbolClass);
BEGIN
    CASE sc OF
        Proc: WriteString('Proc');
    |   Func: WriteString('Func');
    |   Variable: WriteString('Variable');
    |   Keyword: WriteString('Keyword');
    |   Undeclared: WriteString('Undeclared');
    ELSE
        WriteString('*unknown class*');
    END;
END printSymbolClass;

(*** symbol lists ***)

PROCEDURE printSymbolList(sl:symbolList);
BEGIN
    printHeader('SYMBOL LIST');
    WriteLn;
END printSymbolList;


            (*** nodes ***)


PROCEDURE printNode(n:node);
BEGIN
    pNode(n, 0);
    WriteLn;
END printNode;

PROCEDURE pNode(n:node; depth:CARDINAL);
VAR i:CARDINAL;
    str:stringType;
BEGIN
    FOR i := 1 TO depth DO
       InOut.Write(' ');
    END;
    printHeader('NODE');
    IF nodeEmpty(n) THEN
        WriteString("empty");
        RETURN;
    END;
    printNodeClass(nodeClass(n));
    WriteString(": ");
    printType(nodeType(n));
    WriteString(": ");
    CASE nodeClass(n) OF
        nOp:
            printTokenClass(nodeOp(n));
            WriteLn;
            pNode(nodeLeftArg(n), depth+2);
            WriteLn;
            pNode(nodeRightArg(n), depth+2);        
    |   nUnop:
            printTokenClass(nodeOp(n));
            WriteLn;
            pNode(nodeArg(n), depth+2);
    |   nBool: 
            IF nodeBool(n) THEN
                WriteString("TRUE");
            ELSE
                WriteString("FALSE");
            END;
    |   nInt: WriteInt(nodeInt(n), 0);
    |   nChar: InOut.Write(nodeChar(n));
    |   nString: nodeString(n, str); WriteString(str);
    |   nSymbol: pSymbol(nodeSymbol(n), 0);
    |   nIf:
            WriteLn;
            pNode(nodeTest(n), depth+2);
            WriteLn;
            pNode(nodeThen(n), depth+2);
            WriteLn;
            pNode(nodeElse(n), depth+2);
    |   nWhile:
            WriteLn;
            pNode(nodeTest(n), depth+2);
            WriteLn;
            pNode(nodeStmts(n), depth+2);
    |   nReturn:
            WriteCard(nodeNumFormals(n), 0);
            WriteLn;
            pNode(nodeExpr(n), depth+2);
    |   nCall:
            WriteLn;
            pSymbol(nodeRoutine(n), depth+2);
            WriteLn;
            pNode(nodeArgs(n), depth+2);
    |   nAssignment:
            WriteLn;
            pSymbol(nodeLHS(n), depth+2);
            WriteLn;
            pNode(nodeRHS(n), depth+2);
    |   nWrite:
            WriteLn;
            pNode(nodeArgs(n), depth+2);
    |   nRead:
            WriteLn;
            pNode(nodeArgs(n), depth+2);
    |   nList:
            WriteLn;
            pNode(nodeFirst(n), depth+2);
            WriteLn;
            pNode(nodeRest(n), depth+2);
    ELSE
       (* do nothing *)
    END;
END pNode;

PROCEDURE printNodeClass(nc:NodeClass);
BEGIN
    CASE nc OF
        nOp: WriteString("Op");
    |   nUnop: WriteString("Unop");
    |   nList: WriteString("List");
    |   nRead: WriteString("Read");
    |   nWrite: WriteString("Write");
    |   nBool: WriteString("Bool");
    |   nInt: WriteString("Int");
    |   nChar: WriteString("Char");
    |   nString: WriteString("String");
    |   nSymbol: WriteString("Symbol");
    |   nIf: WriteString("If");
    |   nWhile: WriteString("While");
    |   nAssignment: WriteString("Assignment");
    |   nCall: WriteString("Call");
    |   nReturn: WriteString("Return");
    ELSE
        WriteString("*unknown class*");
    END;
END printNodeClass;
            
        
BEGIN
END Debug.
