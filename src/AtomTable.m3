MODULE AtomTable;

IMPORT SortedTextIntTbl, Stats, Text;

CONST
  Delta = 10;

TYPE
  TextArray = REF ARRAY OF TEXT;

  Private = Public OBJECT
    mapText : TextArray          := NIL;
    mapId   : SortedTextIntTbl.T := NIL;
    used    : CARDINAL           := 1;
  METHODS
    init(): T;
  END;

REVEAL
  T = Private BRANDED OBJECT
  OVERRIDES
    findId     := T_findId;
    findString := T_findString;
    init       := T_init;
    putString  := T_putString;
  END;

PROCEDURE new(): T =
BEGIN
  RETURN NEW(T).init();
END new;

PROCEDURE T_findId(self: T; id: CARDINAL): TEXT =
BEGIN
  IF (id < 1) OR (id > self.used) THEN
    RETURN NIL;
  END;
  RETURN self.mapText[id];
END T_findId;

PROCEDURE T_findString(self: T; string: TEXT): CARDINAL =
VAR id: INTEGER := 0;
BEGIN
  IF (string # NIL) AND (Text.Length(string) > 0) THEN
    IF (NOT self.mapId.get(string, id)) THEN
      id := 0;
    END;
  END;
  RETURN id;
END T_findString;

PROCEDURE T_init(self: T): T =
BEGIN
  self.used    := 1;
  self.mapText := NEW(TextArray, 1);
  self.mapId   := NEW(SortedTextIntTbl.Default).init();
  FOR i := 0 TO NUMBER(self.mapText^)-1 DO self.mapText[i] := NIL; END;
  RETURN self;
END T_init;

PROCEDURE T_putString(self: T; string: TEXT): CARDINAL =
VAR id: INTEGER := 0;
BEGIN
  IF (string # NIL) AND (Text.Length(string) > 0) THEN
    IF (NOT self.mapId.get(string, id)) THEN
      WITH limit = NUMBER(self.mapText^) DO
        IF (self.used >= limit) THEN
          WITH newLimit = limit + Delta, newText = NEW(TextArray, newLimit) DO
            SUBARRAY(newText^, 0, limit) := SUBARRAY(self.mapText^, 0, limit);
            FOR i := limit TO newLimit-1 DO newText[i] := NIL; END;
            self.mapText := newText;
          END;
        END;
        id := self.used;
        INC(self.used);
        Stats.log(Stats.Kind.Strings);
        self.mapText[id] := string;
      END;
      EVAL self.mapId.put(string, id);
    END;
  END;
  RETURN id;
END T_putString;

BEGIN
END AtomTable.
