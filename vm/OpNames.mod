IMPLEMENTATION MODULE OpNames;

FROM VM2Defs IMPORT opCode;
FROM StringStuff IMPORT stringEqual, stringCopy;

CONST maxNameLen = 10;  (* maximum length of mnemonics *)

TYPE
    opString = ARRAY[0..maxNameLen] OF CHAR;
    opRec = RECORD
                name: opString;
                type: opType;
             END;

VAR opTable: ARRAY opCode OF opRec;


PROCEDURE opCodeToName(oc:opCode; VAR s:ARRAY OF CHAR):opType;
BEGIN
    stringCopy(s, opTable[oc].name);
    RETURN opTable[oc].type;
END opCodeToName;

PROCEDURE nameToOpCode(s:ARRAY OF CHAR; VAR oc:opCode):opType;
BEGIN
    FOR oc := VAL(opCode, 0) TO VAL(opCode, HIGH(opTable)) DO
        IF stringEqual(s, opTable[oc].name) THEN
            RETURN opTable[oc].type;
        END;
    END;
    RETURN NotFound;
END nameToOpCode;


PROCEDURE OTset(op: opCode; s: opString; ot:opType);
BEGIN
    WITH opTable[op] DO
        name := s;
        type := ot;
    END;
END OTset;

PROCEDURE init;
BEGIN
    OTset(Add,          "ADD",      NoArgs);
    OTset(Sub,          "SUB",      NoArgs);
    OTset(Mul,          "MUL",      NoArgs);
    OTset(Div,          "DIV",      NoArgs);
    OTset(Neg,          "NEG",      NoArgs);
    OTset(Equal,        "EQUAL",    NoArgs);
    OTset(NotEqual,     "NOTEQL",   NoArgs);
    OTset(Greater,      "GREATER",  NoArgs);
    OTset(Less,         "LESS",     NoArgs);
    OTset(GreaterEqual, "GTREQL",   NoArgs);
    OTset(LessEqual,    "LSSEQL",   NoArgs);
    OTset(Not,          "NOT",      NoArgs);
    OTset(Push,         "PUSH",     OneArg);
    OTset(PushConst,    "PUSHC",    OneArg);
    OTset(Pop,          "POP",      NoArgs);
    OTset(PopConst,     "POPC",     OneArg);
    OTset(Branch,       "BRANCH",   OneArg);
    OTset(Jump,         "JUMP",     NoArgs);
    OTset(BranchEqual,  "BREQL",    OneArg);
    OTset(BranchLess,   "BRLSS",    OneArg);
    OTset(BranchGreater,"BRGTR",    OneArg);
    OTset(Contents,     "CONTENTS", NoArgs);
    OTset(Halt,         "HALT",     NoArgs);
    OTset(Writeint,     "WRINT",    NoArgs);
    OTset(Readint,      "RDINT",    NoArgs);
    OTset(WriteChar,    "WRCHAR",   NoArgs);
    OTset(ReadChar,     "RDCHAR",   NoArgs);

    OTset(Return,       "RETURN",   OneArg);
    OTset(FReturn,      "FRETURN",  OneArg);
    OTset(Call,         "CALL",     TwoArgs);
    OTset(PushL,        "PUSHL",    TwoArgs);
    OTset(PopL,         "POPL",     TwoArgs);
    OTset(Min,          "MIN",      NoArgs);
    OTset(Copy,         "COPY",     NoArgs);
    OTset(AddrL,        "ADDRL",    TwoArgs);
    OTset(SetSP,        "SETSP",    OneArg);
    OTset(Aref,         "AREF",     OneArg);
END init;

BEGIN
    init;
END OpNames.
