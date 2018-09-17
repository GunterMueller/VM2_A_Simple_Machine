IMPLEMENTATION MODULE Routines;

(* The part of the parser that handles procedures and functions.
     There are basically two things that have to be done: the routine
   declarations have to be processed to yield symbol table entries, and
   the code for the routine bodies has to be generated.  The lists of
   formal parameters (arguments) and locals variables are placed in the
   appropriate slots in the symbol table entry for the routine.  For
functions,
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
     We generate code as if for a block, with two exceptions: at the
beginning,
   we have to push enough words to move the stack pointer past the local
   storage area; while we are at it, we initialize the words to 0.  At the
   end, we generate a return, in case the user didn't.  For procedures, it
   is okay to return by falling off the end.  For functions, something has to
   be returned explicitly; it is an error to fall through.
*)

FROM Token IMPORT token, tokenClass, isType, typeType, tokenClassToType,
    tokenList, tlEmpty, tlNext, tlToken, freeTokenList;
FROM LexAn IMPORT getToken, getTokenClass, ungetToken, tokenErrorCheck,
        getTokenErrorCheck, compError, peekTokenClass;
FROM Symbol IMPORT symbol, setSymbolType, setSymbolOffset,
    symbolFormals, symbolLocals, symbolList, slNext, slSymbol, slEmpty,
    SymbolClass, numFormals, numLocals;
FROM SymbolTable IMPORT enterSymbol, enterFormal, beginRoutine, endRoutine;
FROM CodeGen IMPORT genBlock, genLocals;
FROM CodeWrite IMPORT writeInt, writeFReturn, writeReturn, writeRoutineLabel;
FROM Parser IMPORT vars, idlist, block;
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

(* <proc> ::= procedure <id> <formals> ; <vars> <routines> <block> ; *)
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
    locals(s);
    routines;
    writeRoutineLabel(s);
    genLocals(s);
    genBlock(block(s));
    writeReturn(numFormals(s));
    tokenErrorCheck(Semicolon, 'semicolon expected');
    endRoutine(s);
END proc;

(* <func> ::= function <id> <formals>:<type>; <vars> <routines> <block>; *)
PROCEDURE func;
VAR fname, ftype:token;
    s:symbol;
    i:CARDINAL;
BEGIN
    getTokenErrorCheck(fname, Identifier, 'function name expected');
    s := enterSymbol(fname.string, Func, tUnknown);
    beginRoutine(s);
    formals(s);
    tokenErrorCheck(Colon, 'colon expected');
    getToken(ftype);
    IF NOT isType(ftype.class) THEN
        compError('function type expected');
    END;
    tokenErrorCheck(Semicolon, 'semicolon expected');
    setSymbolType(s, tokenClassToType(ftype.class));
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
    writeFReturn(numFormals(s));
    tokenErrorCheck(Semicolon, 'semicolon expected');
    endRoutine(s);
END func;

(* <formals> ::= <empty> | ( <formlist> ) *)
PROCEDURE formals(routine:symbol);
VAR formList:symbolList;
    offset:INTEGER;
BEGIN
    IF getTokenClass() = Lparen THEN
        formlist(routine);
        tokenErrorCheck(Rparen, 'right paren expected');
    ELSE
        ungetToken;
    END;
    (* Set the offsets of the formals *)
    formList := symbolFormals(routine);
    offset := initialFormalOffset;
    WHILE NOT slEmpty(formList) DO
        setSymbolOffset(slSymbol(formList), offset);
        INC(offset);
        formList := slNext(formList);
    END;
END formals;


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

(* <formdecl> ::= <idlist> : <typeId> *)
PROCEDURE formdecl(routine:symbol);
VAR tl, tokenp:tokenList;
    t:token;
    tt:typeType;
BEGIN
    tl := idlist();
    tokenErrorCheck(Colon, "colon expected");
    getToken(t);
    IF isType(t.class) THEN
        tt := tokenClassToType(t.class);
    ELSE
        compError("type name expected");
        tt := tUnknown;
        ungetToken;
    END;
    (* create and enter the symbols *)
    tokenp := tl;
    WHILE NOT tlEmpty(tokenp) DO
        tlToken(tokenp, t);
        enterFormal(t.string, tt, routine);
        tokenp := tlNext(tokenp);
    END;
    freeTokenList(tl);
END formdecl;

PROCEDURE locals(routine:symbol);
(* Syntactically, locals look just like globals; but we have to put them
   into the locals list of the routine and give them offsets from the frame
   pointer. *)
VAR locList:symbolList;
    offset:INTEGER;
BEGIN
    vars(routine);
    locList := symbolLocals(routine);
    offset := initialLocalOffset;
    (* set the offsets of the locals *)
    WHILE NOT slEmpty(locList) DO
        setSymbolOffset(slSymbol(locList), offset);
        DEC(offset);
        locList := slNext(locList);
    END;
END locals;


BEGIN
END Routines.
