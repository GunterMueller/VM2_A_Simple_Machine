IMPLEMENTATION MODULE SymbolTable;

(* The symbol table for the SIMPL compiler.  It is a hash table; each entry
   is a symbol,possibly linked through the NEXT field to other symbols. The
   list of symbols is doubly linked, to make it easy to delete from the middle.
   We still have to rehash to delete from the beginning, though.  This could
   be gotten around by hanging a dummy record off of every hashtable entry.
*)

FROM Symbol IMPORT symbol, emptySymbol, symbolList, addToSymbolList, modeType,
        slSymbol, slNext, slEmpty, Class, freeSymbolList, emptySymbolList;
IMPORT Symbol;
FROM Token IMPORT stringType, tokenClass;
FROM LexAn IMPORT compError;
FROM MyTerminal IMPORT fatal;
FROM StringStuff IMPORT stringEqual;

CONST symTabSize = 20;  
(* This is NOT an upper limit on the number of symbols,
   since we have linked lists coming off of the hashtable entries.  Still,
   the compiler may run faster (because the lists it searches are shorter)
   if this number is increased. *)

VAR symbolTable: ARRAY[0..symTabSize-1] OF symbol;
    lexicalLevel: CARDINAL;

PROCEDURE currentLexLevel():CARDINAL;
BEGIN
    RETURN lexicalLevel;
END currentLexLevel;

PROCEDURE enterLocal(VAR s:stringType; type, routine:symbol);
VAR sym:symbol;
BEGIN
    sym := enterSymbol(s, Local, type);
    IF NOT Symbol.empty(sym) THEN
        Symbol.setLocals(routine, 
        Symbol.addToSymbolList(sym, Symbol.locals(routine)));
    END;
END enterLocal;

PROCEDURE enterFormal(VAR s:stringType; mode:modeType; type, routine:symbol);
VAR sym:symbol;
BEGIN
    sym := enterSymbol(s, Formal, type);
    IF NOT Symbol.empty(sym) THEN
        Symbol.setMode(sym, mode);
        Symbol.setFormals(routine, 
        Symbol.addToSymbolList(sym, Symbol.formals(routine)));
    END;
END enterFormal;

PROCEDURE enterKeyword(s:stringType; tc:tokenClass);
VAR sym:symbol;
BEGIN
    sym := enterSymbol(s, Keyword, tUnknown);
    Symbol.setTokenClass(sym, tc);
END enterKeyword;

PROCEDURE enterArrayType(VAR s:stringType; typeObject:symbol);
(* Enter the array type object into the symbol table with the given name. 
   Does NOT create a new symbol. *)
BEGIN
    IF NOT isBuiltIn(s) THEN
        Symbol.setString(typeObject, s);
        typeObject (* dummy *) := insert(typeObject, hash(s));
    ELSE
        compError("cannot redefine a built-in name");
    END;
END enterArrayType;
    
            (*** symbol insertion ***)

PROCEDURE enterSymbol(VAR s:stringType; symc:Symbol.Class; type:symbol):symbol;
(* This does the real work of entering a symbol.  It signals an error
   if a symbol is redefined, or a built-in. *)
BEGIN
    IF NOT isBuiltIn(s) THEN
        RETURN enterSym(s, symc, type);
    ELSE
        compError("can't redefine a built-in symbol");
        RETURN emptySymbol;
    END;
END enterSymbol;

PROCEDURE enterSym(VAR s:stringType; symc:Symbol.Class; type:symbol):symbol;
(* enters a symbol without doing built-in checking *)
VAR sym:symbol;
    h:CARDINAL;
BEGIN
    sym := lookup(s, FALSE, h);
    IF Symbol.empty(sym) THEN
        RETURN insert(Symbol.new(s, symc, lexicalLevel, type), h);
    ELSE
        compError('redefined symbol');
        RETURN sym;
    END;
END enterSym; 
       

            (*** symbol lookup ***)

PROCEDURE findSymbol(VAR s:stringType):symbol;
VAR sym: symbol;
    h: CARDINAL;
BEGIN
    sym := lookup(s, TRUE,  h);
    IF Symbol.empty(sym) THEN
        compError('undefined symbol');
        RETURN insert(Symbol.new(s, Undeclared, 0, tUnknown), h);
    ELSE
        RETURN sym;
    END;
END findSymbol;

PROCEDURE findKeyword(VAR s:stringType; VAR tc:tokenClass):BOOLEAN;
(* This is used by the lexical analyzer to return the keyword's token class.
   Returns true if the keyword is found; tc will then contain the token
   class of the keyword. *)
VAR sym:symbol;
    h:CARDINAL;
BEGIN
    sym := lookup(s, TRUE, h);
    IF Symbol.empty(sym) OR (Symbol.class(sym) <> Keyword) THEN
        RETURN FALSE;
    ELSE
        tc := Symbol.tokClass(sym);
        RETURN TRUE;
    END;
END findKeyword;

PROCEDURE lookup(VAR s:stringType; anything:BOOLEAN; VAR h:CARDINAL):symbol;
(* Looks up the string in the symbol table.  Returns the empty symbol if
   the string isn't found; if it is, returns the symbol and, in h, the hash
   value. anything TRUE means: "match anything".
   This is what findSymbol uses.  We match lexical level on insertion, to
   check for redefined symbols.
*)
VAR sym: symbol;
    syms: stringType;
BEGIN
    h := hash(s);
    sym := symbolTable[h];
    WHILE NOT Symbol.empty(sym) DO
        Symbol.string(sym, syms);
        IF stringEqual(syms, s) AND
                  (anything OR (lexicalLevel = Symbol.lexLevel(sym))) THEN
            RETURN sym;
        END;
        sym := Symbol.next(sym);
    END;
    RETURN Symbol.emptySymbol;
END lookup;
    
           
PROCEDURE insert(s:symbol; h:CARDINAL):symbol;
(* Link the symbol into the h'th symbol table entry.  The symbol is put at
   the front of the list. *)
BEGIN
    Symbol.setNext(s, symbolTable[h]);
    Symbol.setPrev(s, Symbol.emptySymbol);
    symbolTable[h] := s;
    RETURN s;
END insert;

MODULE begRout; (* This needs to be a module because a variable needs
                   to be remembered across invocations. *)
IMPORT symbol, lexicalLevel;
IMPORT Symbol;
EXPORT beginRoutine;

    VAR num:INTEGER;

    PROCEDURE beginRoutine(rname:symbol);
    BEGIN
        IF Symbol.lexLevel(rname) <> 0 THEN  (* assign a unique number to *)
            Symbol.setOffset(rname, num);    (*  non-global procedures *)
            INC(num);
        END;
        INC(lexicalLevel);
    END beginRoutine;

BEGIN
    num := 0;
END begRout;

PROCEDURE endRoutine(rname:symbol);
(* This is the stuff we do at the end of compiling a procedure or function.
   The free's are just to reclaim storage.  The remove's remove the symbols
   from the symbol table, which is important if some local symbol is 
   shadowing a global symbol.  We remove both locals and formals, but we don't
   free the formals because we need them for type checking.
     We also remove the routines declared at this lexical level, and free their
   formals.  We find these routines by searching the entire symbol table--it
   would probably be better to keep a list of them.
     We also remove the types declared at this level, again by exhaustive
   search.
*)
BEGIN
    removeSymbolList(Symbol.locals(rname));
    freeSymbols(Symbol.locals(rname));
    Symbol.freeSymbolList(Symbol.locals(rname));
    Symbol.setLocals(rname, Symbol.emptySymbolList);
    removeSymbolList(Symbol.formals(rname));
    removeRoutinesAtThisLevel;
    DEC(lexicalLevel);
END endRoutine;


PROCEDURE removeSymbolList(symbolp:symbolList);
BEGIN
    WHILE NOT Symbol.slEmpty(symbolp) DO
        removeSymbol(Symbol.slSymbol(symbolp));
        symbolp := Symbol.slNext(symbolp);
    END; 
END removeSymbolList;

PROCEDURE removeRoutinesAtThisLevel;
(* Remove all routines defined at this lexical level. Free their formals.
   All the symbols at this lexical level will be at the beginning of their
   respective buckets in the symbol table, and they all will be routines.
   Now removes types too. *)
VAR i:CARDINAL;
    s, next:symbol;
BEGIN
    FOR i := 0 TO symTabSize-1 DO
        s := symbolTable[i];
        WHILE (NOT Symbol.empty(s)) AND (Symbol.lexLevel(s) = lexicalLevel) DO
            IF NOT Symbol.isType(s) THEN (* it's a routine *)
                freeSymbols(Symbol.formals(s));
                Symbol.freeSymbolList(Symbol.formals(s));
            END;
            (* remove this symbol from the table *)
            next := Symbol.next(s);
            symbolTable[i] := next;
            IF NOT Symbol.empty(next) THEN
                Symbol.setPrev(next, Symbol.emptySymbol);
            END;
            Symbol.free(s);
            s := next;
        END;
    END;
END removeRoutinesAtThisLevel;

PROCEDURE removeSymbol(s:symbol);
(* Splice the symbol out of the symbol table.  If the symbol is at the
   beginning of the list, we have to rehash to find the right entry.
   Otherwise, just remove it from the list. *)
VAR bucket:CARDINAL;
    syms: stringType;
BEGIN
    IF Symbol.empty(Symbol.prev(s)) THEN
        Symbol.string(s, syms);
        bucket := hash(syms);
        IF NOT Symbol.equal(symbolTable[bucket], s) THEN
            fatal('removeSymbol: error');
        ELSE
            symbolTable[bucket] := Symbol.next(s);
            IF NOT Symbol.empty(Symbol.next(s)) THEN
                Symbol.setPrev(Symbol.next(s), Symbol.emptySymbol);
            END;
        END;
    ELSE
        Symbol.setNext(Symbol.prev(s), Symbol.next(s));
        IF NOT Symbol.empty(Symbol.next(s)) THEN
            Symbol.setPrev(Symbol.next(s), Symbol.prev(s));
        END;
    END;
END removeSymbol;

PROCEDURE freeSymbols(symbolp:Symbol.symbolList);
VAR nextSymbol:Symbol.symbolList;
BEGIN
    WHILE NOT Symbol.slEmpty(symbolp) DO
        nextSymbol := Symbol.slNext(symbolp);
        Symbol.free(Symbol.slSymbol(symbolp));
        symbolp := nextSymbol;
    END; 
END freeSymbols;



            (*** low-level stuff ***)

PROCEDURE hash(VAR s:stringType):CARDINAL;
(* A simple hash function: just add up the ASCII values of the characters. *)
VAR i, sum:CARDINAL;
BEGIN
    i := 0;
    sum := 0;
    WHILE s[i] <> 0C DO 
        sum := sum + ORD(s[i]);
        INC(i);
    END;
    RETURN sum MOD symTabSize;
END hash;

MODULE BuiltIns;
    IMPORT stringType, stringEqual, enterSym, fatal, symbol, emptySymbol;
    IMPORT Symbol;
    EXPORT isBuiltIn, enterBuiltIn, enterBuiltInType;

    CONST maxBuiltIns = 10;

    VAR builtIns: ARRAY[1..maxBuiltIns] OF stringType;
        nBuiltIns: [0..maxBuiltIns];

    PROCEDURE isBuiltIn(VAR s:stringType):BOOLEAN;
    VAR i:CARDINAL;
    BEGIN
        FOR i := 1 TO nBuiltIns DO
            IF stringEqual(s, builtIns[i]) THEN
                RETURN TRUE;
            END;
        END;
        RETURN FALSE;
    END isBuiltIn;

    PROCEDURE enterBuiltIn(s:stringType; symc:Symbol.Class):symbol;
    BEGIN
        IF nBuiltIns = maxBuiltIns THEN
            fatal('too many built-ins');
        ELSE
            INC(nBuiltIns);
            builtIns[nBuiltIns] := s;
            RETURN enterSym(s, symc, emptySymbol);
        END;
    END enterBuiltIn;

    PROCEDURE enterBuiltInType(s:stringType):symbol;
    (* Assumes size =1 scalar types *)
    VAR sym:symbol;
    BEGIN
        sym := enterBuiltIn(s, Symbol.ScalarType);
        Symbol.setSize(sym, 1);
        RETURN sym;
    END enterBuiltInType;

BEGIN (* module BuiltIns *)
    nBuiltIns := 0;
END BuiltIns;


PROCEDURE initSymbolTable;
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO symTabSize-1 DO
        symbolTable[i] := emptySymbol;
    END;
END initSymbolTable;

PROCEDURE enterBuiltIns;
VAR oneArg:symbolList;
BEGIN
    tInteger := enterBuiltInType("INTEGER");
    tChar := enterBuiltInType("CHAR");
    tBoolean := enterBuiltInType("BOOLEAN");
    (* tString isn't really a built-in type, nor is STRING a reserved word.
       So just create a symbol for tString. Do be careful about
       the symbol class and the basetype: some might use that info in
       type-checking (e.g. writeCheck). Strings are arrays of CHAR. *)
    tString := Symbol.new("_tString", ArrayType, lexicalLevel, tChar);
     (* tUnknown is a total dummy, but it shouldn't be equal to the empty
        symbol nonetheless. *)
    tUnknown := Symbol.new("_tUnknown", ScalarType, lexicalLevel, emptySymbol); 
     (* LOW and HIGH built-in functions can be treated as ordinary
        functions by everything except the code generator.  Give them a dummy 
        formal for type-checking purposes. The formal will correctly default to
        mode IN. *)
    oneArg := addToSymbolList(Symbol.new("_dummy", Formal, lexicalLevel,
                              tUnknown), emptySymbolList);
    lowFunc := enterBuiltIn("LOW", Func);
    Symbol.setFormals(lowFunc, oneArg);
    Symbol.setType(lowFunc, tInteger);
    highFunc := enterBuiltIn("HIGH", Func);
    Symbol.setFormals(highFunc, oneArg);
    Symbol.setType(highFunc, tInteger);
END enterBuiltIns;


BEGIN
    lexicalLevel := 0;
    initSymbolTable;
    enterBuiltIns;
END SymbolTable.
