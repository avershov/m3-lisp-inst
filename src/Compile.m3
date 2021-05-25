MODULE Compile;

IMPORT Debug, Fmt, SECD, Sexpr, Snode;

VAR
  DEBUG := FALSE;

(*
PROCEDURE push_value(value: CARDINAL; VAR s: Snode.T) =
BEGIN
  Sexpr.push(Sexpr.newNumber(value), s);
END push_value;
*)

PROCEDURE push_opcode(opc: SECD.Opcode; VAR s: Snode.T) =
BEGIN
  Sexpr.push(Sexpr.newNumber(SECD.opcode(opc)), s);
END push_opcode;

(*
PROCEDURE push_symbol(string: TEXT; VAR s: Snode.T) =
BEGIN
  Sexpr.push(Sexpr.newSymbol(string), s);
END push_symbol;
*)

(*
  complist(e, n, c) =
  если равно(e, НИЛ) то
      cons(ВДК, cons(НИЛ, c))
  иначе
      complist(cdr(e), n, comp(саг(e), n, cons(CONS, c)))
*)

PROCEDURE complist(e, n, c: Snode.T): Snode.T RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.sep(); END;
  IF DEBUG THEN Debug.dump("[1] complist e", e); END;
  IF DEBUG THEN Debug.dump("[1] complist n", n); END;
  IF DEBUG THEN Debug.dump("[1] complist c", c); END;
  IF Sexpr.isSymbolEQ(e, Sexpr.nil()) THEN
    Sexpr.push(Sexpr.nil(), c);
    IF DEBUG THEN Debug.dump("[2] complist c", c); END;
    push_opcode(SECD.Opcode.opcLDC, c);
    IF DEBUG THEN Debug.dump("[3] complist c", c); END;
  ELSE
    IF DEBUG THEN Debug.dump("[4] complist c", c); END;
    WITH le = Sexpr.pop(e) DO
      IF DEBUG THEN Debug.dump("[5] complist le", le); END;
      IF DEBUG THEN Debug.dump("[5] complist e", e); END;
      push_opcode(SECD.Opcode.opcCONS, c);
      IF DEBUG THEN Debug.dump("[6] complist c", c); END;
      c := comp(le, n, c);
      IF DEBUG THEN Debug.dump("[7] complist c", c); END;
      c := complist(e, n, c);
      IF DEBUG THEN Debug.dump("[8] complist c", c); END;
    END;
  END;
  RETURN c;
END complist;

PROCEDURE location(e, n: Snode.T): Snode.T RAISES {Error, Sexpr.Error} =

  TYPE
    Pair = RECORD
      a, b: CARDINAL := 0;
    END;

  PROCEDURE loc(e, n: Snode.T): Pair RAISES {Error, Sexpr.Error} =

    (*
      member(e, n) =

      если равно(n, НИЛ) то Л
      иначе если равно(e, car(n)) то И
      иначе member(e, cdr(n))
    *)

    PROCEDURE member(e, n: Snode.T): BOOLEAN RAISES {Sexpr.Error} =
    BEGIN
      IF Sexpr.isSymbolEQ(n, Sexpr.nil()) THEN
        RETURN FALSE;
      ELSE
        WITH x = Sexpr.pop(n) DO
          IF Sexpr.isSymbolEQ(e, x) THEN
            RETURN TRUE;
          ELSE
            RETURN member(e, n);
          END;
        END;
      END;
    END member;

    (*
      position(e, n) =

      если равно(e, саг(n)) то 0
      иначе 1 + position(e, cdr(n))
    *)

    PROCEDURE position(e, n: Snode.T): CARDINAL RAISES {Sexpr.Error} =
    BEGIN
      WITH x = Sexpr.pop(n) DO
        IF Sexpr.isSymbolEQ(e, x) THEN
          RETURN 0;
        ELSE
          RETURN 1 + position(e, n);
        END;
      END;
    END position;

    PROCEDURE incar(p: Pair): Pair =
    BEGIN
      RETURN Pair{ a := p.a + 1, b := p.b };
    END incar;

  (*
    location(e, n) =

    если member(e, саг(n)) то
        cons(0, position(e, саг(n)))
    иначе
        incar(location(e, cdr(n)))
  *)

  BEGIN (*loc*)
    IF DEBUG THEN Debug.sep(); END;
    IF DEBUG THEN Debug.dump("[1] loc e", e); END;
    IF DEBUG THEN Debug.dump("[1] loc n", n); END;
    IF Sexpr.isSymbolEQ(e, Sexpr.nil()) THEN
      RAISE Error("[location] atom not found in namelist (variable name): " & Sexpr.symbol(e).value());
    END;
    WITH ls = Sexpr.pop(n) DO
      IF DEBUG THEN Debug.dump("[2] loc ls", ls); END;
      IF DEBUG THEN Debug.dump("[2] loc n", n); END;
      IF member(e, ls) THEN
        IF DEBUG THEN Debug.dump("[3] loc member(e, ls) = TRUE", NIL); END;
        RETURN Pair{ a := 0, b := position(e, ls) };
      ELSE
        IF DEBUG THEN Debug.dump("[4] loc member(e, ls) = FALSE", NIL); END;
        RETURN incar(loc(e, n));
      END;
    END;
  END loc;

BEGIN (*location*)
  IF DEBUG THEN Debug.sep(); END;
  IF DEBUG THEN Debug.dump("[1] location e", e); END;
  IF DEBUG THEN Debug.dump("[1] location n", n); END;
  WITH p = loc(e, n) DO
    IF DEBUG THEN Debug.dump("[2] location p = { " & Fmt.Int(p.a) & " . " & Fmt.Int(p.b) & " };", NIL); END;
    WITH res = Sexpr.newCons(Sexpr.newNumber(p.a), Sexpr.newNumber(p.b)) DO
      IF DEBUG THEN Debug.dump("[3] location res", res); END;
      RETURN res;
    END;
  END;
END location;

(*
  vars(d) = если равно(d,НИЛ) то НИЛ иначе cons(caar(d), vars(cdr(d)))
*)

PROCEDURE vars(d: Snode.T): Snode.T RAISES {Sexpr.Error} =
BEGIN
  IF Sexpr.isSymbolEQ(d, Sexpr.nil()) THEN
    RETURN Sexpr.nil();
  ELSE
    WITH s = Sexpr.pop(d) DO
      RETURN Sexpr.newCons(Sexpr.car(s), vars(d));
    END;
  END;
END vars;

(*
  exprs(d) = если равно(d,НИЛ) то НИЛ иначе cons(cdar(d), exprs(cdr(d)))
*)

PROCEDURE exprs(d: Snode.T): Snode.T RAISES {Sexpr.Error} =
BEGIN
  IF Sexpr.isSymbolEQ(d, Sexpr.nil()) THEN
    RETURN Sexpr.nil();
  ELSE
    WITH s = Sexpr.pop(d) DO
      RETURN Sexpr.newCons(Sexpr.cdr(s), exprs(d));
    END;
  END;
END exprs;

(*
  comp(e, n, c) =
  если атом(e) то
      cons(ВВОД, cons(location(e, n), c))
  иначе если равно(саг(e), КОД) то
      cons(ВДК, cons(cadr(e), c))
  иначе если равно(саг(e), ПЛЮС) то
      comp(cadr(e), n, comp(caddr(e), n, cons(ПЛЮС, c)))
  иначе если равно(саг(e), МИНУС) то
      comp(cadr(e), n, comp(caddr(e), n, cons(МИНУС, c)))
  иначе если равно(саг(e), УМН) то
      comp(cadr(e), n, comp(caddr(e), n, cons(УМН, c)))
  иначе если равно(саг(e), ДЕЛ) то
      comp(cadr(e), n, comp(caddr(e), n, cons(ДЕЛ, c)))
  иначе если равно(саг(e), ОСТ) то
      comp(cadr(e), n, comp(caddr(e), n, cons(ОСТ, c)))
  иначе если равно(саг(e), РАВНО) то
      comp(cadr(e), n, comp(caddr(e), n, cons(РАВНО, c)))
  иначе если равно(саг(e), МР) то
      comp(cadr(e), n, comp(caddr(e), n, cons(МР, c)))
  иначе если равно(саг(e), CAR) то
      comp(cadr(e), cons(CAR, c))
  иначе если равно(саг(e), CDR) то
      comp(cadr(e), cons(CDR, c))
  иначе если равно(саг(e), АТОМ) то
      comp(cadr(e), cons(АТОМ, c))
  иначе если равно(саг(e), CONS) то
      comp(caddr(e), n, comp(cadr(e), n, cons(CONS, c)))
  иначе если равно(саг(e), ЕСЛИ) то
      { comp(cadr(e), n, cons(ВЫБ, cons(то, cons(иначе, c))))
        где то = comp(caddr(e), n, (ВОССТ))
        и иначе = comp(cadddr(e), n, (ВОССТ)) }
  иначе если равно(саг(e), ЛЯМБДА) то
      { cons(ВДФ, cons(тело, c))
        где тело = comp(caddr(e), cons(cadr(e), n), (ВОЗВР)) }
  иначе если равно(саг(e), ПУСТЬ) то
      {{ complist(арг, n, cons(ВДФ, cons(тело, cons(ПРИМ, c))))
             где тело = comp(cadr(e), m, (B03BP)) }
         где m = cons(vars(cddr(e)), n)
         и арг = exprs(cddr(e)) }
  иначе если равно(саг(e), ПУСТЬРЕК) то
      {{ cons(ФОРМ, complist(арг, m, cons(ВДФ, cons(тело, cons(PEK, c)))))
             где тело = comp(cadr(e), m, (ВОЗВР)) }
         где m = cons(vars(cddr(e)), n)
         и арг = exprs(cddr(e)) }
  иначе
      complist(cdr(e), n, comp(саг(e), n, cons(ПРИМ, c)))
*)

PROCEDURE comp(e, n, c: Snode.T): Snode.T RAISES {Error, Sexpr.Error} =
BEGIN
  IF (NOT Sexpr.isCons(e)) THEN
    IF DEBUG THEN Debug.sep(); END;
    IF DEBUG THEN Debug.dump("[1] comp (load) e", e); END;
    IF DEBUG THEN Debug.dump("[1] comp n", n); END;
    IF DEBUG THEN Debug.dump("[1] comp c", c); END;
    IF (NOT Sexpr.isSymbol(e)) THEN
      RAISE Error("[comp] unknown atom where must be a symbol (variable name)");
    END;
    WITH loc = location(e, n) DO
      IF DEBUG THEN Debug.dump("[2] comp loc", loc); END;
      Sexpr.push(loc, c);
      IF DEBUG THEN Debug.dump("[3] comp c", c); END;
      push_opcode(SECD.Opcode.opcLD, c);
      IF DEBUG THEN Debug.dump("[4] comp c", c); END;
      RETURN c;
    END;
  ELSE
    WITH s = Sexpr.pop(e) DO
      IF Sexpr.isSymbolEQ(s, quoteSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        Sexpr.push(Sexpr.car(e), c);
        push_opcode(SECD.Opcode.opcLDC, c);
        RETURN c;
      ELSIF Sexpr.isSymbolEQ(s, addSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          IF DEBUG THEN Debug.dump("[2] comp a", a); END;
          IF DEBUG THEN Debug.dump("[2] comp b", b); END;
          IF DEBUG THEN Debug.dump("[2] comp e", e); END;
          push_opcode(SECD.Opcode.opcADD, c);
          IF DEBUG THEN Debug.dump("[3] comp c", c); END;
          c := comp(b, n, c);
          IF DEBUG THEN Debug.dump("[4] comp c", c); END;
          c := comp(a, n, c);
          IF DEBUG THEN Debug.dump("[5] comp c", c); END;
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, subSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcSUB, c);
          c := comp(b, n, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, mulSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcMUL, c);
          c := comp(b, n, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, divSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcDIV, c);
          c := comp(b, n, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, remSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcREM, c);
          c := comp(b, n, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, eqSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcEQ, c);
          c := comp(b, n, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, leqSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcLEQ, c);
          c := comp(b, n, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, carSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e) DO
          push_opcode(SECD.Opcode.opcCAR, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, cdrSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e) DO
          push_opcode(SECD.Opcode.opcCDR, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, atomSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e) DO
          push_opcode(SECD.Opcode.opcATOM, c);
          c := comp(a, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, consSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH a = Sexpr.pop(e), b = Sexpr.car(e) DO
          push_opcode(SECD.Opcode.opcCONS, c);
          c := comp(a, n, c);
          c := comp(b, n, c);
          RETURN c;
        END;
      ELSIF Sexpr.isSymbolEQ(s, ifSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH e1 = Sexpr.pop(e), e2 = Sexpr.pop(e), e3 = Sexpr.car(e) DO
          VAR then: Snode.T := Sexpr.nil();
              else: Snode.T := Sexpr.nil();
          BEGIN
            IF DEBUG THEN Debug.dump("[2] comp e1", e1); END;
            IF DEBUG THEN Debug.dump("[2] comp e2", e2); END;
            IF DEBUG THEN Debug.dump("[2] comp e3", e3); END;
            IF DEBUG THEN Debug.dump("[2] comp then", then); END;
            IF DEBUG THEN Debug.dump("[2] comp else", else); END;
            push_opcode(SECD.Opcode.opcJOIN, then);
            push_opcode(SECD.Opcode.opcJOIN, else);
            IF DEBUG THEN Debug.dump("[3] comp then", then); END;
            IF DEBUG THEN Debug.dump("[3] comp else", else); END;
            then := comp(e2, n, then);
            IF DEBUG THEN Debug.dump("[4] comp then", then); END;
            else := comp(e3, n, else);
            IF DEBUG THEN Debug.dump("[5] comp else", else); END;
            Sexpr.push(else, c);
            IF DEBUG THEN Debug.dump("[6] comp c", c); END;
            Sexpr.push(then, c);
            IF DEBUG THEN Debug.dump("[7] comp c", c); END;
            push_opcode(SECD.Opcode.opcSEL, c);
            IF DEBUG THEN Debug.dump("[8] comp c", c); END;
            c := comp(e1, n, c);
            IF DEBUG THEN Debug.dump("[9] comp c", c); END;
            RETURN c;
          END;
        END;
      ELSIF Sexpr.isSymbolEQ(s, lambdaSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH xlist = Sexpr.pop(e), fe = Sexpr.car(e), m = Sexpr.newCons(xlist, n) DO
          VAR body: Snode.T := Sexpr.nil();
          BEGIN
            IF DEBUG THEN Debug.dump("[2] comp xlist", xlist); END;
            IF DEBUG THEN Debug.dump("[2] comp fe", fe); END;
            IF DEBUG THEN Debug.dump("[2] comp m", m); END;
            IF DEBUG THEN Debug.dump("[2] comp body", body); END;
            push_opcode(SECD.Opcode.opcRET, body);
            IF DEBUG THEN Debug.dump("[3] comp body", body); END;
            body := comp(fe, m, body);
            IF DEBUG THEN Debug.dump("[4] comp body", body); END;
            Sexpr.push(body, c);
            IF DEBUG THEN Debug.dump("[5] comp c", c); END;
            push_opcode(SECD.Opcode.opcLDF, c);
            IF DEBUG THEN Debug.dump("[6] comp c", c); END;
            RETURN c;
          END;
        END;
      ELSIF Sexpr.isSymbolEQ(s, letSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH le = Sexpr.pop(e), m = Sexpr.newCons(vars(e), n), args = exprs(e) DO
          VAR body: Snode.T := Sexpr.nil();
          BEGIN
            push_opcode(SECD.Opcode.opcRET, body);
            body := comp(le, m, body);
            push_opcode(SECD.Opcode.opcAP, c);
            Sexpr.push(body, c);
            push_opcode(SECD.Opcode.opcLDF, c);
            c := complist(args, n, c);
            RETURN c;
          END;
        END;
      ELSIF Sexpr.isSymbolEQ(s, letrecSymbol) THEN
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        WITH le = Sexpr.pop(e), m = Sexpr.newCons(vars(e), n), args = exprs(e) DO
          VAR body: Snode.T := Sexpr.nil();
          BEGIN
            IF DEBUG THEN Debug.dump("[2] comp le", le); END;
            IF DEBUG THEN Debug.dump("[2] comp m", m); END;
            IF DEBUG THEN Debug.dump("[2] comp args", args); END;
            IF DEBUG THEN Debug.dump("[2] comp e", e); END;
            IF DEBUG THEN Debug.dump("[2] comp body", body); END;
            push_opcode(SECD.Opcode.opcRET, body);
            IF DEBUG THEN Debug.dump("[3] comp body", body); END;
            body := comp(le, m, body);
            IF DEBUG THEN Debug.dump("[4] comp body", body); END;
            push_opcode(SECD.Opcode.opcRAP, c);
            IF DEBUG THEN Debug.dump("[5] comp c", c); END;
            Sexpr.push(body, c);
            IF DEBUG THEN Debug.dump("[6] comp c", c); END;
            push_opcode(SECD.Opcode.opcLDF, c);
            IF DEBUG THEN Debug.dump("[7] comp c", c); END;
            c := complist(args, m, c);
            IF DEBUG THEN Debug.dump("[8] comp c", c); END;
            push_opcode(SECD.Opcode.opcDUM, c);
            IF DEBUG THEN Debug.dump("[9] comp c", c); END;
            RETURN c;
          END;
        END;
      ELSE
        IF DEBUG THEN Debug.sep(); END;
        IF DEBUG THEN Debug.dump("[1] comp (call) s", s); END;
        IF DEBUG THEN Debug.dump("[1] comp e", e); END;
        IF DEBUG THEN Debug.dump("[1] comp n", n); END;
        IF DEBUG THEN Debug.dump("[1] comp c", c); END;
        push_opcode(SECD.Opcode.opcAP, c);
        IF DEBUG THEN Debug.dump("[2] comp c", c); END;
        c := comp(s, n, c);
        IF DEBUG THEN Debug.dump("[3] comp c", c); END;
        c := complist(e, n, c);
        IF DEBUG THEN Debug.dump("[4] comp c", c); END;
        RETURN c;
      END;
    END;
  END;
END comp;

PROCEDURE prog(e: Snode.T): Snode.T RAISES {Error, Sexpr.Error} =
VAR c: Snode.T := Sexpr.nil();
BEGIN
  push_opcode(SECD.Opcode.opcSTOP, c);
  push_opcode(SECD.Opcode.opcAP, c);
  RETURN comp(e, Sexpr.nil(), c);
END prog;

VAR
  addSymbol    : Snode.Symbol := NIL;
  atomSymbol   : Snode.Symbol := NIL;
  carSymbol    : Snode.Symbol := NIL;
  cdrSymbol    : Snode.Symbol := NIL;
  consSymbol   : Snode.Symbol := NIL;
  divSymbol    : Snode.Symbol := NIL;
  eqSymbol     : Snode.Symbol := NIL;
  ifSymbol     : Snode.Symbol := NIL;
  lambdaSymbol : Snode.Symbol := NIL;
  leqSymbol    : Snode.Symbol := NIL;
  letSymbol    : Snode.Symbol := NIL;
  letrecSymbol : Snode.Symbol := NIL;
  mulSymbol    : Snode.Symbol := NIL;
  quoteSymbol  : Snode.Symbol := NIL;
  remSymbol    : Snode.Symbol := NIL;
  subSymbol    : Snode.Symbol := NIL;

BEGIN
  addSymbol    := Sexpr.newSymbol("ADD");
  atomSymbol   := Sexpr.newSymbol("ATOM");
  carSymbol    := Sexpr.newSymbol("CAR");
  cdrSymbol    := Sexpr.newSymbol("CDR");
  consSymbol   := Sexpr.newSymbol("CONS");
  divSymbol    := Sexpr.newSymbol("DIV");
  eqSymbol     := Sexpr.newSymbol("EQ");
  ifSymbol     := Sexpr.newSymbol("IF");
  lambdaSymbol := Sexpr.newSymbol("LAMBDA");
  leqSymbol    := Sexpr.newSymbol("LEQ");
  letSymbol    := Sexpr.newSymbol("LET");
  letrecSymbol := Sexpr.newSymbol("LETREC");
  mulSymbol    := Sexpr.newSymbol("MUL");
  quoteSymbol  := Sexpr.newSymbol("QUOTE");
  remSymbol    := Sexpr.newSymbol("REM");
  subSymbol    := Sexpr.newSymbol("SUB");
END Compile.
