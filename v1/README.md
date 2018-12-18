                    Guide to Part 1 of the SIMPL Compiler

This code accompanies the article "A SIMPL Compiler, Part I."  It implements
a compiler for the SIMPL programming language, minus much of the machinery
for subroutines--that will be discussed in Part 2.

The compiler is written in Modula-2 and consists of twelve modules:

CodeGen
CodeWrite
Compiler (MOD file only)
ExprParser
LexAn
Node
Parser
Routines
Symbol
SymbolTable
Token
TypeChecker

An additional module, Debug, is provided to help in debugging the compiler.

I would appreciate hearing about any bugs in this code.  My BIX nickname is
JBA.  I can also be reached at 1643 Cambridge St. #34, Cambridge MA 02138.
Happy compiling!

    Jonathan Amsterdam

[Editor's note: Jonathan wrote this modula source code on the Macintosh and
took advantage of the longer filenames allowed on the Mac.  Because of the 8
character limit on filenames on BYTEnet, we have combined the above files into
one and you should break out the individual definition and implementation
modules into their own files before compiling them.]
