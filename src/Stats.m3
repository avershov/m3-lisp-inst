MODULE Stats;

PROCEDURE log(what: Kind) =
BEGIN
  INC(data[what]);
END log;

PROCEDURE get(what: Kind): CARDINAL =
BEGIN
  RETURN data[what];
END get;

PROCEDURE name(what: Kind): TEXT =
BEGIN
  RETURN names[what];
END name;

TYPE
  Data = ARRAY Kind OF CARDINAL;
  Names = ARRAY Kind OF TEXT;

VAR
  data: Data := Data { 0, .. };
  names: Names := Names { "car", "cdr", "newCons", "newNumber", "newSymbol", "strings" };

BEGIN
END Stats.
