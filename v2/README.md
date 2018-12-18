
Guide to Part 2 of the SIMPL Compiler

This code accompanies part 2 of "A SIMPL Compiler."  It includes Modula-2 source code for the entire SIMPL Compiler.  You will need the Monitor program from Building a Computer in Software (October '85) and the VM2 Assembler (November '85).  There are 12 modules to the compiler:

CodeGen
CodeWrite
Compiler  (MOD file only)
ExprParser
LexAn
Node
Parser
Routines
Symbol
SymbolTable
Token
TypeChecker

[Editor's note:  The above files were combined into one file when they were put on the bulletin board.  You will need to break each module into a separate file to compile them.]

You will also need to compile the following utility modules along with the 12 above.  These modules can be found with those downloaded with the VM2 Monitor:

CharStuff
LexAnStuff
MyTerminal
StringStuff


The programs were developed using MacModula-2 for the Macintosh, but conversion to other Modula-2 systems should be straightforward.  I would appreciate hearing about any conversion difficulties or bugs.  You can reach me on BIX as "jba" or by U.S. mail at 1643 Cambridge St. #34, Cambridge, MA  02138.  Happy Compiling!

    Jonathan Amsterdam


