IMPLEMENTATION MODULE Init;

FROM SymbolTable IMPORT enterKeyword;
FROM Token IMPORT tokenClass;



PROCEDURE enterKeywords;
BEGIN
    enterKeyword('AND', And);
    enterKeyword('ARRAY', Array);
    enterKeyword('BEGIN', Begin);
    enterKeyword('DO', Do);
    enterKeyword('ELSE', Else);
    enterKeyword('ELSIF', Elsif);
    enterKeyword('END', End);
    enterKeyword('FALSE', False);
    enterKeyword('FUNCTION', Function);
    enterKeyword('IF', If);
    enterKeyword('IN', In);
    enterKeyword('NOT', Not);
    enterKeyword('OF', Of);
    enterKeyword('OR', Or);
    enterKeyword('OUT', Out);
    enterKeyword('PROCEDURE', Procedure);
    enterKeyword('PROGRAM', Program);
    enterKeyword('READ', Read);
    enterKeyword('RETURN', Return);
    enterKeyword('THEN', Then);
    enterKeyword('TRUE', True);
    enterKeyword('TYPE', Type);
    enterKeyword('VAR', Var);
    enterKeyword('WHILE', While);
    enterKeyword('WRITE', Write);
END enterKeywords;

BEGIN
END Init.
