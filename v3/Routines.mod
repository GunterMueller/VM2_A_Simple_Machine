IMPLEMENTATION MODULE Routines;

(* The part of the parser that handles procedures and functions.
     There are basically two things that have to be done: the routine
   declarations have to be processed to yield symbol table entries, and
   the code for the routine bodies has to be generated.  The lists of
   formal parameters (arguments) and locals variables are placed in the
   appropriate slots in the symbol table entry for the routine.  For functions,
   the return type of the function is put in the type slot of the symbol; for
   procedures, this slot is left undefined.  An offset from the stack pointer
   is given to each local and formal.  The initial offsets assume the
   following stack conventions:

    -------------
    |   arg 1   |
    |   arg 2   |       |
    |    ...    |       |   stack grows down
    |   old FP  |       |   towards low memory
    |     SP    |       |
FP->|   return  |       v
    |   loc 1   |
    |   loc 2   |
    |    ...    |

   The list of formals must be backwards to match the argument conventions.
   The order of the locals doesn't matter, but it's also backwards.
     We generate code as if for a block, with two exceptions: at the beginning,
   we have to push enough words to move the stack pointer past the local
   storage area; while we are at it, we initialize the words to 0.  At the
   end, we generate a return, in case the user didn't.  For procedures, it
   is okay to return by falling off the end.  For functions, something has to
   be returned explicitly; it is an error to fall through.
   
 Changes for part 3:
  1. Formal parameter modes handled.
  2. Arrays handled.  Always passed by reference; starting address passed.
  3. Open array params handled. The bounds get offsets just below the
     starting address. 
	
*)        

FROM Token IMPORT token, tokenClass, 
    tokenList, tlEmpty, tlNext, tlToken, freeTokenList;
FROM LexAn IMPORT getToken, getTokenClass, ungetToken, tokenErrorCheck, 
        getTokenErrorCheck, compError, peekTokenClass;
FROM Symbol IMPORT symbol, isType, symbolList, slNext, slSymbol, slEmpty,
    Class,  numLocals, modeType, newArrayType;
IMPORT Symbol;
FROM SymbolTable IMPORT enterSymbol, enterFormal, beginRoutine, endRoutine,
     tUnknown, findSymbol, currentLexLevel;                    
FROM CodeGen IMPORT genBlock, genLocals;
FROM CodeWrite IMPORT writeInt, writeFReturn, writeReturn, writeRoutineLabel,
    writeStrings; 
FROM Parser IMPORT vars, types, idlist, block;
FROM Node IMPORT node;


CONST
    initialLocalOffset = -1;        (* offsets, in words, from the FP *)
    initialFormalOffset = 3;


(* <routines> ::= <empty> | <proc> <routines> | <func> <routines> *)
PROCEDURE routines;
BEGIN
    LOOP
        CASE getTokenClass() OF
            Procedure: proc;
        |   Function:  func;
        ELSE ungetToken; EXIT;
        END;
    END;
END routines;
        
(* <proc> ::= procedure <id> <formals> ; <types> <vars> <routines> <block> ; *)
PROCEDURE proc; 
VAR t:token;
    s:symbol;
    i:CARDINAL;
BEGIN
    getTokenErrorCheck(t, Identifier, 'procedure name expected');
    s := enterSymbol(t.string, Proc, tUnknown);
    beginRoutine(s);
    formals(s);
    tokenErrorCheck(Semicolon, 'semicolon expected');
    types(s);
    locals(s);
    routines;
    writeRoutineLabel(s);
    genLocals(s);
    genBlock(block(s));
    writeReturn(sizeFormals(s));
    writeStrings;
    tokenErrorCheck(Semicolon, 'semicolon expected');
    endRoutine(s);
END proc;

(* <func> ::= function <id> <formals>:<id>; <vars> <routines> <block>; *)
PROCEDURE func; 
VAR fname, ftype:token;
    s, typeSymbol:symbol;
    i:CARDINAL;
BEGIN
    getTokenErrorCheck(fname, Identifier, 'function name expected');
    s := enterSymbol(fname.string, Func, tUnknown);
    beginRoutine(s);
    formals(s);
    tokenErrorCheck(Colon, 'colon expected');
    typeSymbol := typeName();
    IF Symbol.class(typeSymbol) <> ScalarType THEN
        compError("functions can only return scalar types");
    END;
    tokenErrorCheck(Semicolon, 'semicolon expected');
    Symbol.setType(s, typeSymbol);
    types(s);
    locals(s);
    routines;
    writeRoutineLabel(s);
    genLocals(s);
    genBlock(block(s));
    (* Here we should generate an error message in the code: value not
       returned from function.  But since we have no string manipulation,
       we can't. Instead we'll return either 0 (for an Integer function) or
       false (which is also 0) for a boolean function. *)
    writeInt(0);
    writeFReturn(sizeFormals(s));
    writeStrings;
    tokenErrorCheck(Semicolon, 'semicolon expected');
    endRoutine(s);
END func;

(* <formals> ::= <empty> | ( <formlist> ) *)
PROCEDURE formals(routine:symbol);
VAR formList:symbolList;
    offset:INTEGER;
BEGIN
    IF getTokenClass() <> Lparen THEN
        ungetToken;
    ELSE
        formlist(routine);
        tokenErrorCheck(Rparen, 'right paren expected');
        (* Set the offsets of the formals *)
        formList := Symbol.formals(routine);
        offset := initialFormalOffset;
        WHILE NOT slEmpty(formList) DO
            Symbol.setOffset(slSymbol(formList), offset);
            IF openArrayFormal(slSymbol(formList)) THEN
                INC(offset, 3); (* 2 extra: one for each bound *)
            ELSE
                INC(offset);
            END;
            formList := slNext(formList);
        END;
    END;
END formals;

PROCEDURE openArrayFormal(s:symbol):BOOLEAN;
BEGIN
    RETURN (Symbol.class(Symbol.type(s)) = ArrayType) AND
           Symbol.open(Symbol.type(s));
END openArrayFormal;

(* <formlist> ::= <formdecl> | <formdecl> ; <formlist> *)
PROCEDURE formlist(routine:symbol);
BEGIN
    formdecl(routine);
    IF getTokenClass() = Semicolon THEN
        formlist(routine);
    ELSE
        ungetToken;
    END;
END formlist;

(* <formdecl> ::= <idlist> : <mode> <typeId> *)
PROCEDURE formdecl(routine:symbol);
VAR tl, tokenp:tokenList;
    t:token;
    typeSymbol:symbol;
    m:modeType;
BEGIN
    tl := idlist();
    tokenErrorCheck(Colon, "colon expected");
    m := formalMode();
    getToken(t);
    IF t.class = Identifier THEN
        ungetToken;
        typeSymbol := typeName();
    ELSIF t.class = Array THEN
        tokenErrorCheck(Of, "OF expected");
        typeSymbol := newArrayType(typeName(), 0, 0, TRUE, 
        currentLexLevel());
        (* bounds are irrelevant; TRUE indicates open array  *)
    ELSE
        compError("type name or open array parameter expected");
        ungetToken;
        typeSymbol := tUnknown;
    END;
    (* create and enter the symbols *)
    tokenp := tl;
    WHILE NOT tlEmpty(tokenp) DO
        tlToken(tokenp, t);
        enterFormal(t.string, m, typeSymbol, routine);
        tokenp := tlNext(tokenp);
    END;
    freeTokenList(tl);
END formdecl;

(* <mode> ::= <empty> | IN | OUT | IN OUT.
   Note: OUT IN is not acceptable.  *)
PROCEDURE formalMode():modeType;
VAR t:token;
BEGIN
    getToken(t);
    IF t.class = In THEN
        IF getTokenClass() = Out THEN
            RETURN mInOut;
        ELSE
            ungetToken;
            RETURN mIn;
        END;
    ELSIF t.class = Out THEN
        RETURN mOut;
    ELSE
        ungetToken;
        RETURN mIn;
    END;
END formalMode;

PROCEDURE locals(routine:symbol);
(* Syntactically, locals look just like globals; but we have to put them
   into the locals list of the routine and give them offsets from the frame
   pointer. Note that since the array handling stuff expects the starting
   address to be the lowest in the array, we have to assign the offset
   of arrays so that the starting address is lowest. *)
VAR locList:symbolList;
    offset:INTEGER;
    size:CARDINAL;
BEGIN
    vars(routine);
    locList := Symbol.locals(routine);
    offset := initialLocalOffset;
    (* set the offsets of the locals *)
    WHILE NOT slEmpty(locList) DO
        size := Symbol.size(Symbol.type(slSymbol(locList)));
        Symbol.setOffset(slSymbol(locList), offset-INTEGER(size)+1);
        DEC(offset, INTEGER(size));
        locList := slNext(locList);
    END;
END locals;

PROCEDURE typeName():symbol;
VAR t:token;
    s:symbol;
BEGIN
    getTokenErrorCheck(t, Identifier, "type name expected");
    s := findSymbol(t.string);
    IF NOT isType(s) THEN
        compError('type name expected');
        s := tUnknown;
    END;
    RETURN s;
END typeName;

PROCEDURE sizeFormals(routine:symbol):CARDINAL;
(* Returns the number of words occupied by formals.  Before this was just
   the number of formals, but now each open array param adds 2 to the count.
   (For its bounds.) *)
VAR formlist:symbolList;
    sum:CARDINAL;
    t:symbol;
BEGIN
    formlist := Symbol.formals(routine);
    sum := 0;
    WHILE NOT slEmpty(formlist) DO
        t := Symbol.type(slSymbol(formlist));
        IF (Symbol.class(t) = ArrayType) AND Symbol.open(t) THEN
            INC(sum, 3); 
        ELSE
            INC(sum);
        END;
        formlist := slNext(formlist);
    END;
    RETURN sum;
END sizeFormals;

BEGIN
END Routines.
