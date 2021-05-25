INTERFACE Stats;

TYPE
  Kind = {Car, Cdr, NewCons, NewNumber, NewSymbol, Strings};

PROCEDURE log(what: Kind);
PROCEDURE get(what: Kind): CARDINAL;
PROCEDURE name(what: Kind): TEXT;

END Stats.
