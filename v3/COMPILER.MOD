MODULE Compiler;

(* A compiler for the SIMPL programming language. 
   Copyright 1985 by Jonathan Amsterdam. All Rights Reserved.
   See the BYTE article "A SIMPL Compiler" for more information.

   Module map, roughly in order of low to high level:

    CharStuff       Low-level character utilities           \
    StringStuff     Low-level string utilities              |  used in previous
    MyTerminal      Low-level terminal I/O utilities        |      projects
    LexAnStuff      Toolkit for building lexical analyzers  /

    Token           Token, tokenList and typeType data types
    Symbol          Symbol, symbolList and related data types
    Node            Node and related data types
    
    Init            Initialization of compiler
    TypeChecker     Procedures to do type-checking
    LexAn           Lexical analyzer for compiler
    SymbolTable     Compiler symbol table
    CodeWrite       Actual output of code
    CodeGen         Code generation
    
    ExprParser      Parses expressions
    Routines        Parses procedure and function declarations
    Parser          Main parser

   The module Debug, also supplied, is not used by the compiler, but contains
   routines useful in debugging the compiler.

   I would appreciate hearing about any bugs in the code.  My BIX name is
   jba.         --Jonathan Amsterdam
*)

FROM InOut IMPORT OpenInput, OpenOutput, CloseInput, CloseOutput;
FROM MyTerminal IMPORT ClearScreen, pause, WriteLnString;
FROM Parser IMPORT program;
IMPORT Init;

BEGIN
    ClearScreen;
    WriteLnString("SIMPL Compiler V2.0");
    OpenInput('SMP');
    OpenOutput('ASM');
    Init.enterKeywords;
    program;
    CloseInput;
    CloseOutput;
    pause('Done--');
END Compiler.
