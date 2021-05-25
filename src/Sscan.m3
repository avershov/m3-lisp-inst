MODULE Sscan;

IMPORT Sexpr, Snode;
IMPORT ASCII, Rd, Text, Thread;

TYPE
  Type = { Number, Symbol, Char, End };

  Token = RECORD
    type   : Type    := Type.End;
    number : INTEGER := 0;
    symbol : TEXT    := NIL;
    char   : CHAR    := ' ';
  END;

CONST
  EOF = VAL(255, CHAR);

VAR
  reader: Rd.T := NIL;
  nextChar: CHAR := ' ';
  nextToken: Token;

PROCEDURE char() RAISES {Rd.Failure, Thread.Alerted} =
BEGIN
  TRY
    nextChar := Rd.GetChar(reader);
  EXCEPT
  | Rd.EndOfFile => nextChar := EOF;
  END;
END char;

PROCEDURE numberToken() RAISES {Error, Rd.Failure, Thread.Alerted} =
VAR negative: BOOLEAN := FALSE;
BEGIN
  IF (nextChar = '-') THEN negative := TRUE; char(); END;
  IF NOT (nextChar IN ASCII.Digits) THEN RAISE Error("[numberToken] not a digit character after '-' sign: '" & Text.FromChar(nextChar) & "'"); END;
  nextToken.type := Type.Number;
  nextToken.number := 0;
  REPEAT
    nextToken.number := (nextToken.number * 10) + (ORD(nextChar) - ORD('0'));
    char();
  UNTIL NOT (nextChar IN ASCII.Digits);
  IF negative THEN nextToken.number := -nextToken.number; END;
END numberToken;

PROCEDURE symbolToken() RAISES {Rd.Failure, Thread.Alerted} =
BEGIN
  nextToken.type := Type.Symbol;
  nextToken.symbol := Text.FromChar(nextChar);
  char();
  WHILE (nextChar IN ASCII.AlphaNumerics) DO
    nextToken.symbol := nextToken.symbol & Text.FromChar(nextChar);
    char();
  END;
END symbolToken;

PROCEDURE charToken() RAISES {Error, Rd.Failure, Thread.Alerted} =
CONST SpecialChars = ASCII.Set{'(', '.', ')'};
BEGIN
  IF NOT (nextChar IN SpecialChars) THEN RAISE Error("[charToken] only '(', '.', ')' allowed but character is: '" & Text.FromChar(nextChar) & "'"); END;
  nextToken.type := Type.Char;
  nextToken.char := nextChar;
  char();
END charToken;

PROCEDURE token() RAISES {Error, Rd.Failure, Thread.Alerted} =
CONST NumberStart = ASCII.Digits + ASCII.Set{'-'};
BEGIN
  WHILE (nextChar IN ASCII.Spaces) DO char(); END;
  IF (nextChar = EOF) THEN
    nextToken.type := Type.End;
  ELSIF (nextChar IN NumberStart) THEN
    numberToken();
  ELSIF (nextChar IN ASCII.Letters) THEN
    symbolToken();
  ELSE
    charToken();
  END;
END token;

PROCEDURE list(): Snode.T RAISES {Error, Rd.Failure, Thread.Alerted} =
VAR
  car: Snode.T := NIL;
  cdr: Snode.T := NIL;
BEGIN
  car := expr();
  IF (nextToken.type = Type.Char) AND (nextToken.char = '.') THEN
    token();
    cdr := expr();
  ELSIF (nextToken.type = Type.Char) AND (nextToken.char = ')') THEN
    (*token();*)
    cdr := Sexpr.nil();
  ELSE
    (*token();*)
    cdr := list();
  END;
  RETURN Sexpr.newCons(car, cdr);
END list;

PROCEDURE expr(): Snode.T RAISES {Error, Rd.Failure, Thread.Alerted} =
VAR e: Snode.T := NIL;
BEGIN
  IF (nextToken.type = Type.Char) AND (nextToken.char = '(') THEN
    token();
    IF (nextToken.type = Type.Char) AND (nextToken.char = ')') THEN
      e := Sexpr.nil();
      token();
    ELSE
      e := list();
      token();
    END;
  ELSIF (nextToken.type = Type.Number) THEN
    e := Sexpr.newNumber(nextToken.number);
    token();
  ELSIF (nextToken.type = Type.Symbol) THEN
    e := Sexpr.newSymbol(nextToken.symbol);
    token();
  ELSE
    RAISE Error("[expr] bad formed expression: only list, number or symbol allowed after '(' character");
  END;
  RETURN e;
END expr;

PROCEDURE fromRd(rd: Rd.T): Snode.T RAISES {Error, Rd.Failure, Thread.Alerted} = 
BEGIN
  reader := rd;
  char();
  token();
  RETURN expr();
END fromRd;

BEGIN
END Sscan.
