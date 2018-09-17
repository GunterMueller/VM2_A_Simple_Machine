IMPLEMENTATION MODULE SymbolTable;

(* The symbol table for the SIMPL compiler.  It is a hash table; each entry
   is a symbol,possibly linked through the NEXT field to other symbols. The
   list of symbols is doubly linked, to make it easy to delete from the middle.
   We still have to rehash to delete from the beginning, though.  This could
   be gotten around by hanging a dummy record off of every hashtable entry.
*)

FROM Symbol IMPORT symbol, SymbolClass, emptySymbol, scopeType, newSymbol,
    symbolFormals, symbolLocals, symbolEmpty, symbolString, symbolScope,
    symbolNext, symbolPrev, setSymbolNext, setSymbolPrev, setSymbolLocals,
    setSymbolFormals, freeSymbol, symbolClass, symbolEqual, symbolTokClass,
    setSymbolTokClass, emptySymbolList, numFormals,
    symbolList, slEmpty, slNext, slSymbol, addToSymbolList, freeSymbolList;
FROM Token IMPORT stringType, tokenClass, typeType;
FROM LexAn IMPORT compError;
FROM MyTerminal IMPORT fatal;
FROM StringStuff IMPORT stringEqual;

CONST symTabSize = 20;  
(* This is NOT an upper limit on the number of symbols,
   since we have linked lists coming off of the hashtable entries.  Still,
   the compiler may run faster (because the lists it searches are shorter)
   if this number is increased. *)

VAR symbolTable: ARRAY[0..symTabSize-1] OF symbol;

PROCEDURE enterGlobal(VAR s:stringType; symc:SymbolClass; tt:typeType):symbol;
BEGIN
    RETURN enterSymbol(s, symc, Global, tt);
END enterGlobal;

PROCEDURE enterLocal(VAR s:stringType; tt:typeType; routine:symbol);
VAR sym:symbol;
BEGIN
    sym := enterSymbol(s, Variable, Local, tt);
    setSymbolLocals(routine, addToSymbolList(sym, symbolLocals(routine)));
END enterLocal;

PROCEDURE enterFormal(VAR s:stringType; tt:typeType; routine:symbol);
VAR sym:symbol;
BEGIN
    sym := enterSymbol(s, Variable, Local, tt);
    setSymbolFormals(routine, addToSymbolList(sym, symbolFormals(routine)));
END enterFormal;

PROCEDURE enterKeyword(s:stringType; tc:tokenClass);
VAR sym:symbol;
BEGIN
    sym := enterSymbol(s, Keyword, Global, tUnknown);
    setSymbolTokClass(sym, tc);
END enterKeyword;
    
            (*** symbol insertion ***)

PROCEDURE enterSymbol(VAR s:stringType; symc:SymbolClass; st:scopeType;
                        tt: typeType):symbol;
(* This does the real work of entering a symbol.  It signals an error
   if a symbol is redefined. *)
VAR sym:symbol;
    h:CARDINAL;
BEGIN
    sym := lookup(s, st, h);
    IF symbolEmpty(sym) THEN
        RETURN insert(newSymbol(s, symc, st, tt), h);
    ELSE
        compError('redefined symbol');
        RETURN sym;
    END;
END enterSymbol; 
       
            (*** symbol lookup ***)

PROCEDURE findSymbol(VAR s:stringType):symbol;
VAR sym: symbol;
    h: CARDINAL;
BEGIN
    sym := lookup(s, Formal,  h);
    IF symbolEmpty(sym) THEN
        compError('undefined symbol');
        RETURN insert(newSymbol(s, Undeclared, Global, tUnknown), h);
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
    sym := lookup(s, Global, h);
    IF symbolEmpty(sym) OR (symbolClass(sym) <> Keyword) THEN
        RETURN FALSE;
    ELSE
        tc := symbolTokClass(sym);
        RETURN TRUE;
    END;
END findKeyword;

PROCEDURE lookup(VAR s:stringType; st:scopeType; VAR h:CARDINAL):symbol;
(* Looks up the string in the symbol table.  Returns the empty symbol if
   the string isn't found; if it is, returns the symbol and, in h, the hash
   value.  The scope type has a special interpretation: Global is for globals,
   Local for both locals and formals, and Formal means "match anything".
   The first two are used on insertion, to check for redefined symbols.
*)
VAR sym: symbol;
    syms: stringType;
BEGIN
    h := hash(s);
    sym := symbolTable[h];
    WHILE NOT symbolEmpty(sym) DO
        symbolString(sym, syms);
        IF stringEqual(syms, s) AND
                     ((st = Formal) OR (st = symbolScope(sym))) THEN
            RETURN sym;
        END;
        sym := symbolNext(sym);
    END;
    RETURN emptySymbol;
END lookup;
    
           
PROCEDURE insert(s:symbol; h:CARDINAL):symbol;
(* Link the symbol into the h'th symbol table entry.  The symbol is put at
   the front of the list. *)
BEGIN
    setSymbolNext(s, symbolTable[h]);
    setSymbolPrev(s, emptySymbol);
    symbolTable[h] := s;
    RETURN s;
END insert;

PROCEDURE endRoutine(rname:symbol);
(* This is the stuff we do at the end of compiling a procedure or function.
   The free's are just to reclaim storage.  The remove's remove the symbols
   from the symbol table, which is important if some local symbol is 
   shadowing a global symbol.  We remove both locals and formals, but we don't
   free the formals because we need them for type checking.
*)
BEGIN
    removeSymbolList(symbolLocals(rname));
    freeSymbols(symbolLocals(rname));
    freeSymbolList(symbolLocals(rname));
    setSymbolLocals(rname, emptySymbolList);
    removeSymbolList(symbolFormals(rname));
END endRoutine;


PROCEDURE removeSymbolList(symbolp:symbolList);
BEGIN
    WHILE NOT slEmpty(symbolp) DO
        removeSymbol(slSymbol(symbolp));
        symbolp := slNext(symbolp);
    END; 
END removeSymbolList;

PROCEDURE removeSymbol(s:symbol);
(* Splice the symbol out of the symbol table.  If the symbol is at the
   beginning of the list, we have to rehash to find the right entry.
   Otherwise, just remove it from the list. *)
VAR bucket:CARDINAL;
    syms: stringType;
BEGIN
    IF symbolEmpty(symbolPrev(s)) THEN
        symbolString(s, syms);
        bucket := hash(syms);
        IF NOT symbolEqual(symbolTable[bucket], s) THEN
            fatal('removeSymbol: error');
        ELSE
            symbolTable[bucket] := symbolNext(s);
            IF NOT symbolEmpty(symbolNext(s)) THEN
                setSymbolPrev(symbolNext(s), emptySymbol);
            END;
        END;
    ELSE
        setSymbolNext(symbolPrev(s), symbolNext(s));
        IF NOT symbolEmpty(symbolNext(s)) THEN
            setSymbolPrev(symbolNext(s), symbolPrev(s));
        END;
    END;
END removeSymbol;

PROCEDURE freeSymbols(symbolp:symbolList);
VAR nextSymbol:symbolList;
BEGIN
    WHILE NOT slEmpty(symbolp) DO
        nextSymbol := slNext(symbolp);
        freeSymbol(slSymbol(symbolp));
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


PROCEDURE initSymbolTable;
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO symTabSize-1 DO
        symbolTable[i] := emptySymbol;
    END;
END initSymbolTable;

BEGIN
    initSymbolTable;
END SymbolTable.
