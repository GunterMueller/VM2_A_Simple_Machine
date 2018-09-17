IMPLEMENTATION MODULE LabelTable;

(* The label table is implemented as a hash table with collision resolution
   by linear probing. *)

FROM VM2Defs IMPORT address;
FROM StringStuff IMPORT stringEqual, stringLen, stringCopy;
CONST
    labelSize = 20;     (* Labels longer than this are truncated *)
    htSize = 100;       (* Maximum number of labels in a program *)

TYPE
    labelString = ARRAY[1..labelSize] OF CHAR;
    htRec = RECORD
                lab:labelString;
                addr:address;
                empty:BOOLEAN;
                def: defType;
            END;

VAR hashTable: ARRAY[0..htSize-1] OF htRec;


PROCEDURE insert(label:ARRAY OF CHAR; a:address; d:defType):BOOLEAN;
(* Inserts a label into the table.  Returns TRUE if the table wasn't full.
   If the label is not present, it is inserted.
   If the label is present, we redefine it.
   Truncates the label to labelSize characters.  *)
VAR index:CARDINAL;
    label1:labelString;
BEGIN
    stringCopy(label1, label);
    IF NOT find(label1, index) AND NOT hashTable[index].empty THEN
        RETURN FALSE;
    END;
    WITH hashTable[index] DO
        lab := label1;
        addr := a;
        def := d;
        empty := FALSE;    END;
    RETURN TRUE;
END insert;

PROCEDURE lookup(label:ARRAY OF CHAR; VAR a:address; VAR d:defType):BOOLEAN;
(* Retrieves the address associated with the label from the table.  Returns
   TRUE if the label was found.  Truncates the label to labelSize chars. *)
VAR index:CARDINAL;
    label1:labelString;
BEGIN
    stringCopy(label1, label);
    IF find(label1, index) THEN
        a := hashTable[index].addr;
        d := hashTable[index].def;
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END;
END lookup;

PROCEDURE map(proc:mapProc);
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO htSize-1 DO
        WITH hashTable[i] DO
            IF NOT empty THEN
                proc(lab, addr, def);
            END;
        END;
    END;
END map;

                (*** internal procedures ***)

PROCEDURE find(VAR label:ARRAY OF CHAR; VAR probe:CARDINAL):BOOLEAN;
VAR start:CARDINAL;
BEGIN
    start := hash(label);
    probe := start;
    REPEAT
        WITH hashTable[probe] DO
            IF empty THEN
                RETURN FALSE;
            ELSIF stringEqual(label, lab) THEN
                RETURN TRUE;
            END;
        END;
        probe := (probe + 1) MOD htSize;
    UNTIL probe = start;
    RETURN FALSE;
END find;

PROCEDURE hash(VAR s:ARRAY OF CHAR):CARDINAL;
(* A standard hash function: add up the ASCII values of the characters and
   mod out by the size of the hashtable. Since the hashtable indices run from
   0 to htSize-1, the MODded value is correct. *)
VAR i, sum:CARDINAL;
BEGIN
    sum := 0;
    FOR i := 0 TO stringLen(s)-1 DO
        INC(sum, ORD(s[i]));
    END;
    RETURN sum MOD htSize;
END hash;

PROCEDURE initHashTable;
VAR i:CARDINAL;
BEGIN
    FOR i := 0 TO htSize-1 DO
        hashTable[i].empty := TRUE;
    END;
END initHashTable;

BEGIN
    initHashTable;
END LabelTable.
