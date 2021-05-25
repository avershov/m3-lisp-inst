MODULE Sfmt;

IMPORT Fmt, Sexpr, Snode;

PROCEDURE toText(e: Snode.T): TEXT RAISES {Sexpr.Error} =

  PROCEDURE token(text: TEXT; newSpace: BOOLEAN) =
  BEGIN
    IF space THEN str := str & " "; END;
    str := str & text;
    space := newSpace;
  END token;

  PROCEDURE expr(e: Snode.T) RAISES {Sexpr.Error} =
  BEGIN
    IF (e = NIL) THEN
      token("<nil>", TRUE);
    ELSIF e.marked() THEN
      token("[]", TRUE);
    ELSE
(*
*)
      e.markNode(TRUE);
      IF Sexpr.isNumber(e) THEN
        token(Fmt.Int(Sexpr.number(e).value()), TRUE);
      ELSIF Sexpr.isSymbol(e) THEN
        token(Sexpr.symbol(e).value(), TRUE);
      ELSE
        token("(", FALSE);
        WHILE Sexpr.isCons(e) DO
          WITH c = Sexpr.cons(e) DO
(*            e.markNode(TRUE);*)
            expr(c.car());
(*            e.markNode(FALSE);*)
            e := c.cdr();
          END;
        END;
        IF (NOT Sexpr.isNil(e)) THEN
          token(".", TRUE);
(*          e.markNode(TRUE);*)
          expr(e);
(*          e.markNode(FALSE);*)
        END;
        space := FALSE;
        token(")", TRUE);
      END;
(*
*)
      e.markNode(FALSE);
    END;
  END expr;

VAR
  str := "";
  space := FALSE;

BEGIN (*toText*)
  space := FALSE;
  IF (e # NIL) THEN e.markTree(FALSE); END;
  expr(e);
  IF (e # NIL) THEN e.markTree(FALSE); END;
  RETURN str;
END toText;

BEGIN
END Sfmt.
