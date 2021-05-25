MODULE SECD;

IMPORT Debug, Fmt, Sexpr, Snode;

VAR
  DEBUG := FALSE;

PROCEDURE opcode(opc: Opcode): CARDINAL =
BEGIN
  RETURN 1 + ORD(opc);
END opcode;

TYPE
  OpcodeNames = ARRAY Opcode OF TEXT;

VAR
  names: OpcodeNames := OpcodeNames { "LD", "LDC", "LDF", "AP", "RET", "DUM", "RAP",
                                      "SEL", "JOIN", "CAR", "CDR", "ATOM", "CONS", "EQ",
                                      "ADD", "SUB", "MUL", "DIV", "REM", "LEQ", "STOP" };

VAR
  S: Snode.T := NIL;
  E: Snode.T := NIL;
  C: Snode.T := NIL;
  D: Snode.T := NIL;

(*<>*)

PROCEDURE pop(VAR s: Snode.T; opc: Opcode; where: CARDINAL): Snode.T RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.pop(s);
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END pop;

PROCEDURE push(p: Snode.T; VAR s: Snode.T; <*UNUSED*> opc: Opcode; <*UNUSED*> where: CARDINAL) (*RAISES {Error}*) =
BEGIN
  (*TRY*)
    Sexpr.push(p, s);
  (*EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;*)
END push;

PROCEDURE pop_number(VAR s: Snode.T; opc: Opcode; where: CARDINAL): INTEGER RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.number(Sexpr.pop(s)).value();
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END pop_number;

PROCEDURE car(s: Snode.T; opc: Opcode; where: CARDINAL): Snode.T RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.car(s);
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END car;

PROCEDURE cdr(s: Snode.T; opc: Opcode; where: CARDINAL): Snode.T RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.cdr(s);
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END cdr;

PROCEDURE car_number(s: Snode.T; opc: Opcode; where: CARDINAL): INTEGER RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.number(Sexpr.car(s)).value();
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END car_number;

PROCEDURE cdr_number(s: Snode.T; opc: Opcode; where: CARDINAL): INTEGER RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.number(Sexpr.cdr(s)).value();
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END cdr_number;

PROCEDURE car_cdrn(node: Snode.T; n: CARDINAL; opc: Opcode; where: CARDINAL): Snode.T RAISES {Error} =
BEGIN
  TRY
    RETURN Sexpr.car(Sexpr.cdrn(node, n));
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;
END car_cdrn;

PROCEDURE cons(car, cdr: Snode.T; <*UNUSED*> opc: Opcode; <*UNUSED*> where: CARDINAL): Snode.Cons (*RAISES {Error}*) =
BEGIN
  (*TRY*)
    RETURN Sexpr.newCons(car, cdr);
  (*EXCEPT
  | Sexpr.Error(str) => RAISE Error("[" & names[opc] & " : " & Fmt.Int(where) & "] " & str);
  END;*)
END cons;

(*s e (LD i.c) d -> (x.s) e c d | x = взять(i, e) *)
PROCEDURE do_ld() RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_ld() [1]", NIL); END;
  WITH
    index = pop(C, Opcode.opcLD, 1),
    m = car_number(index, Opcode.opcLD, 2),
    n = cdr_number(index, Opcode.opcLD, 3),
    list = car_cdrn(E, m, Opcode.opcLD, 4),
    value = car_cdrn(list, n, Opcode.opcLD, 5)
  DO
    push(value, S, Opcode.opcLD, 6);
  END;
END do_ld;

(* s e (LDC x.c) d -> (x.s) e c d *)
PROCEDURE do_ldc() RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_ldc() [1]", NIL); END;
  WITH value = pop(C, Opcode.opcLDC, 1) DO
    push(value, S, Opcode.opcLDC, 2);
  END;
END do_ldc;

(* s e (LDF c'.c) d -> ((c'.e).s) e c d *)
PROCEDURE do_ldf() RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_ldf() [1]", NIL); END;
  WITH
    c = pop(C, Opcode.opcLDF, 1),
    value = cons(c, E, Opcode.opcLDF, 2)
  DO
    push(value, S, Opcode.opcLDF, 3);
  END;
END do_ldf;

(* ((c'.e') v.s) e (AP.c) d -> NIL (v.e') c' (s e c.d) *)
PROCEDURE do_ap() RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_ap() [1]", NIL); END;
  WITH
    ce = pop(S, Opcode.opcAP, 1),
    v = pop(S, Opcode.opcAP, 2)
  DO
    push(C, D, Opcode.opcAP, 3);
    push(E, D, Opcode.opcAP, 4);
    push(S, D, Opcode.opcAP, 5);
    C := car(ce, Opcode.opcAP, 6);
    E := cdr(ce, Opcode.opcAP, 7);
    push(v, E, Opcode.opcAP, 8);
    S := Sexpr.nil();
  END;
END do_ap;

(* (x) e' (RET) (s e c.d) -> (x.s) e c d *)
PROCEDURE do_ret() RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_ret() [1]", NIL); END;
  WITH a = pop(S, Opcode.opcRET, 1) DO
    S := pop(D, Opcode.opcRET, 2);
    E := pop(D, Opcode.opcRET, 3);
    C := pop(D, Opcode.opcRET, 4);
    push(a, S, Opcode.opcRET, 5);
  END;
END do_ret;

(* s e (DUM.c) d -> s (W.e) c d *)
PROCEDURE do_dum() RAISES {Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_dum() [1]", NIL); END;
  push(NIL, E, Opcode.opcDUM, 1);
END do_dum;

(*<>*)

(* ((c'.e') v.s) (W.e) (RAP.c) d -> NIL завершение(e', v) c' (s e c.d) | завершение(e', v) = car(e') <- v если car(e') = W *)
PROCEDURE do_rap() RAISES {Error, Sexpr.Error} =
BEGIN
  IF DEBUG THEN Debug.dump("do_rap() [1]", NIL); END;
  WITH
    ce = pop(S, Opcode.opcRAP, 1),
    v = pop(S, Opcode.opcRAP, 2),
    e = cdr(E, Opcode.opcRAP, 3)
  DO
    IF DEBUG THEN Debug.dump("do_rap() [2] ce", ce); END;
    IF DEBUG THEN Debug.dump("do_rap() [2] v", v); END;
    IF DEBUG THEN Debug.dump("do_rap() [2] S", S); END;
    IF DEBUG THEN Debug.dump("do_rap() [2] e", e); END;
    IF DEBUG THEN Debug.dump("do_rap() [2] E", E); END;
    push(C, D, Opcode.opcRAP, 4);
    push(e, D, Opcode.opcRAP, 5);
    push(S, D, Opcode.opcRAP, 6);
    IF DEBUG THEN Debug.dump("do_rap() [3] D", D); END;
    C := car(ce, Opcode.opcRAP, 7);
    IF DEBUG THEN Debug.dump("do_rap() [4] C", C); END;
    E := cdr(ce, Opcode.opcRAP, 8);
    Sexpr.complete(E, v);
    IF DEBUG THEN Debug.dump("do_rap() [5] E", E); END;
    S := Sexpr.nil();
    IF DEBUG THEN Debug.dump("do_rap() [6] S", S); END;
  END;
(*  TRY
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_rap] " & str);
  END;*)
END do_rap;

(* (x.s) e (SEL c1 c0.c) d -> s e cx (c.d) | cx = равно(x, T) ? c1 : c0 *)
PROCEDURE do_sel() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_sel() [1]", NIL); END;
    WITH
      value = Sexpr.symbol(Sexpr.pop(S)),
      ct = pop(C, Opcode.opcSEL, 2),
      cf = pop(C, Opcode.opcSEL, 3)
    DO
      push(C, D, Opcode.opcSEL, 4);
      IF Sexpr.isTrue(value) THEN
        C := ct;
      ELSE
        C := cf;
      END;
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_sel] " & str);
  END;
END do_sel;

(* s e (JOIN) (c.d) -> s e c d *)
PROCEDURE do_join() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_join() [1]", NIL); END;
    C := pop(D, Opcode.opcJOIN, 1);
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_join] " & str);
  END;
END do_join;

(* ((a.b).s) e (CAR.c) d -> (a.s) e c d *)
PROCEDURE do_car() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_car() [1]", NIL); END;
    WITH value = Sexpr.car(Sexpr.pop(S)) DO
      push(value, S, Opcode.opcCAR, 2);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_car] " & str);
  END;
END do_car;

(* ((a.b).s) e (CDR.c) d -> (b.s) e c d *)
PROCEDURE do_cdr() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_cdr() [1]", NIL); END;
    WITH value = Sexpr.cdr(Sexpr.pop(S)) DO
      push(value, S, Opcode.opcCDR, 2);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_cdr] " & str);
  END;
END do_cdr;

(* (a.s) e (ATOM.c) d -> (t.s) e c d       | t = атом(a) ? T : F *)
PROCEDURE do_atom() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_atom() [1]", NIL); END;
    WITH value = Sexpr.cond(Sexpr.isAtom(Sexpr.pop(S))) DO
      push(value, S, Opcode.opcATOM, 2);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_atom] " & str);
  END;
END do_atom;

(* (a b.s) e (CONS.c) d -> ((a.b).s) e c d *)
PROCEDURE do_cons() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_cons() [1]", NIL); END;
    WITH
      a = pop(S, Opcode.opcCONS, 1),
      b = pop(S, Opcode.opcCONS, 2),
      value = cons(a, b, Opcode.opcCONS, 3)
    DO
      push(value, S, Opcode.opcCONS, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_cons] " & str);
  END;
END do_cons;

(* (a b.s) e (EQ.c) d -> (b = a.s) e c d *)
PROCEDURE do_eq() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_eq() [1]", NIL); END;
    WITH
      a = pop(S, Opcode.opcEQ, 1),
      b = pop(S, Opcode.opcEQ, 2),
      value = Sexpr.cond(Sexpr.isAtomsAndEQ(a, b))
    DO
      push(value, S, Opcode.opcEQ, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_eq] " & str);
  END;
END do_eq;

(* (a b.s) e (ADD.c) d -> (b + a.s) e c d *)
PROCEDURE do_add() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_add() [1]", NIL); END;
    WITH
      a = pop_number(S, Opcode.opcADD, 1),
      b = pop_number(S, Opcode.opcADD, 2),
      value = Sexpr.newNumber(b + a)
    DO
      push(value, S, Opcode.opcADD, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_add] " & str);
  END;
END do_add;

(* (a b.s) e (SUB.c) d -> (b - a.s) e c d *)
PROCEDURE do_sub() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_sub() [1]", NIL); END;
    WITH
      a = pop_number(S, Opcode.opcSUB, 1),
      b = pop_number(S, Opcode.opcSUB, 2),
      value = Sexpr.newNumber(b - a)
    DO
      push(value, S, Opcode.opcSUB, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_sub] " & str);
  END;
END do_sub;

(* (a b.s) e (MUL.c) d -> (b * a.s) e c d *)
PROCEDURE do_mul() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_mul() [1]", NIL); END;
    WITH
      a = pop_number(S, Opcode.opcMUL, 1),
      b = pop_number(S, Opcode.opcMUL, 2),
      value = Sexpr.newNumber(b * a)
    DO
      push(value, S, Opcode.opcMUL, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_mul] " & str);
  END;
END do_mul;

(* (a b.s) e (DIV.c) d -> (b / a.s) e c d *)
PROCEDURE do_div() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_div() [1]", NIL); END;
    WITH
      a = pop_number(S, Opcode.opcDIV, 1),
      b = pop_number(S, Opcode.opcDIV, 2),
      value = Sexpr.newNumber(b DIV a)
    DO
      push(value, S, Opcode.opcDIV, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_div] " & str);
  END;
END do_div;

(* (a b.s) e (REM.c) d -> (b % a.s) e c d *)
PROCEDURE do_rem() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_rem() [1]", NIL); END;
    WITH
      a = pop_number(S, Opcode.opcREM, 1),
      b = pop_number(S, Opcode.opcREM, 2),
      value = Sexpr.newNumber(b MOD a)
    DO
      push(value, S, Opcode.opcREM, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_rem] " & str);
  END;
END do_rem;

(* (a b.s) e (LEQ.c) d -> (b <= a.s) e c d *)
PROCEDURE do_leq() RAISES {Error} =
BEGIN
  TRY
    IF DEBUG THEN Debug.dump("do_leq() [1]", NIL); END;
    WITH
      a = pop_number(S, Opcode.opcLEQ, 1),
      b = pop_number(S, Opcode.opcLEQ, 2),
      value = Sexpr.cond(b <= a)
    DO
      push(value, S, Opcode.opcLEQ, 4);
    END;
  EXCEPT
  | Sexpr.Error(str) => RAISE Error("[do_leq] " & str);
  END;
END do_leq;

(*<>*)

(* s e (STOP) d -> s e (STOP) d | стоп *)

PROCEDURE execute(): Snode.T RAISES {Error, Sexpr.Error} =
BEGIN
  LOOP
    IF DEBUG THEN Debug.sep(); END;
    IF DEBUG THEN Debug.dump("execute [1] S", S); END;
    IF DEBUG THEN Debug.dump("execute [1] E", E); END;
    IF DEBUG THEN Debug.dump("execute [1] C", C); END;
    IF DEBUG THEN Debug.dump("execute [1] D", D); END;
    WITH
      opc = Sexpr.pop(C),
      opcode = VAL(Sexpr.number(opc).value() - 1, Opcode)
    DO
(*      IF DEBUG THEN Debug.dump("execute [2] C", C); END;*)
      CASE opcode OF
      | Opcode.opcLD   => do_ld();
      | Opcode.opcLDC  => do_ldc();
      | Opcode.opcLDF  => do_ldf();
      | Opcode.opcAP   => do_ap();
      | Opcode.opcRET  => do_ret();
      | Opcode.opcDUM  => do_dum();
      | Opcode.opcRAP  => do_rap();
      | Opcode.opcSEL  => do_sel();
      | Opcode.opcJOIN => do_join();
      | Opcode.opcCAR  => do_car();
      | Opcode.opcCDR  => do_cdr();
      | Opcode.opcATOM => do_atom();
      | Opcode.opcCONS => do_cons();
      | Opcode.opcEQ   => do_eq();
      | Opcode.opcADD  => do_add();
      | Opcode.opcSUB  => do_sub();
      | Opcode.opcMUL  => do_mul();
      | Opcode.opcDIV  => do_div();
      | Opcode.opcREM  => do_rem();
      | Opcode.opcLEQ  => do_leq();
      | Opcode.opcSTOP => IF DEBUG THEN Debug.sep(); END; RETURN Sexpr.cons(S).car();
      ELSE
        RAISE Error("[execute] invalid opcode: " & Fmt.Int(ORD(opcode)));
      END;
(*      IF DEBUG THEN Debug.dump("execute [3] C", C); END;*)
    END;
  END;
END execute;

PROCEDURE run(fn, args: Snode.T): Snode.T RAISES {Error, Sexpr.Error} =
VAR res: Snode.T := NIL;
BEGIN
  S := Sexpr.newCons(args, Sexpr.nil());
  E := Sexpr.nil();
  C := fn;
  D := Sexpr.nil();

  res := execute();

  S := NIL;
  E := NIL;
  C := NIL;
  D := NIL;

  RETURN res;
END run;

PROCEDURE test() RAISES {Error, Sexpr.Error} =

  PROCEDURE reset() =
  BEGIN
    S := NIL;
    E := NIL;
    C := NIL;
    D := NIL;
  END reset;

  PROCEDURE dump(where: TEXT) RAISES {Sexpr.Error} =
  BEGIN
    Debug.sep();
    Debug.dump(where & " S", S);
    Debug.dump(where & " E", E);
    Debug.dump(where & " C", C);
    Debug.dump(where & " D", D);
  END dump;

  PROCEDURE skip_opcode() RAISES {Sexpr.Error} =
  BEGIN
    C := Sexpr.cons(C).cdr();
  END skip_opcode;

  (*s e (LD i.c) d -> (x.s) e c d | x = взять(i, e) *)
  PROCEDURE test_ld() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newSymbol("s");
    E := Sexpr.newCons(
             Sexpr.newCons(
                 Sexpr.newSymbol("A"),
                 Sexpr.newCons(
                     Sexpr.newSymbol("B"),
                     Sexpr.newCons(
                         Sexpr.newSymbol("C"),
                         Sexpr.nil()))),
             Sexpr.newCons(
                 Sexpr.newCons(
                     Sexpr.newSymbol("D"),
                     Sexpr.newCons(
                         Sexpr.newSymbol("E"),
                         Sexpr.newCons(
                             Sexpr.newSymbol("F"),
                             Sexpr.nil()))),
                 Sexpr.newCons(
                     Sexpr.newCons(
                         Sexpr.newSymbol("G"),
                         Sexpr.newCons(
                             Sexpr.newSymbol("H"),
                             Sexpr.newCons(
                                 Sexpr.newSymbol("I"),
                                 Sexpr.nil()))),
                     Sexpr.nil())));
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcLD)),
             Sexpr.newCons(
                 Sexpr.newCons(
                     Sexpr.newNumber(2),
                     Sexpr.newNumber(1)),
                 Sexpr.newSymbol("c")));
    D := Sexpr.newSymbol("d");
    dump("LD [1]");
    skip_opcode(); do_ld();
    dump("LD [2]");
    reset();
  END test_ld;

  (* s e (LDC x.c) d -> (x.s) e c d *)
  PROCEDURE test_ldc() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newSymbol("s");
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcLDC)),
             Sexpr.newCons(
                 Sexpr.newNumber(117),
                 Sexpr.newSymbol("c")));
    D := Sexpr.newSymbol("d");
    dump("LDC [1]");
    skip_opcode(); do_ldc();
    dump("LDC [2]");
    reset();
  END test_ldc;

  (* s e (LDF c'.c) d -> ((c'.e).s) e c d *)
  PROCEDURE test_ldf() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newSymbol("s");
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcLDC)),
             Sexpr.newCons(
                 Sexpr.newSymbol("cp"),
                 Sexpr.newSymbol("c")));
    D := Sexpr.newSymbol("d");
    dump("LDF [1]");
    skip_opcode(); do_ldf();
    dump("LDF [2]");
    reset();
  END test_ldf;

  (* ((c'.e') v.s) e (AP.c) d -> NIL (v.e') c' (s e c.d) *)
  PROCEDURE test_ap() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newCons(
                 Sexpr.newSymbol("cp"),
                 Sexpr.newSymbol("ep")),
             Sexpr.newCons(
                 Sexpr.newSymbol("v"),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcLDC)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("AP [1]");
    skip_opcode(); do_ap();
    dump("AP [1]");
    reset();
  END test_ap;

  (* (x) e' (RET) (s e c.d) -> (x.s) e c d *)
  PROCEDURE test_ret() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newSymbol("x"),
             Sexpr.nil());
    E := Sexpr.newSymbol("ep");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcRET)),
             Sexpr.nil());
    D := Sexpr.newCons(
             Sexpr.newSymbol("s"),
             Sexpr.newCons(
                 Sexpr.newSymbol("e"),
                 Sexpr.newCons(
                     Sexpr.newSymbol("c"),
                     Sexpr.newSymbol("d"))));
    dump("RET [1]");
    skip_opcode(); do_ret();
    dump("RET [2]");
    reset();
  END test_ret;

  (* s e (DUM.c) d -> s (W.e) c d *)
  PROCEDURE test_dum() RAISES {Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newSymbol("s");
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcDUM)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("DUM [1]");
    skip_opcode(); do_dum();
    dump("DUM [2]");
    reset();
  END test_dum;

  (* ((c'.e') v.s) (W.e) (RAP.c) d -> NIL завершение(e', v) c' (s e c.d) | завершение(e', v) = car(e') <- v если car(e') = W *)
  PROCEDURE test_rap() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    WITH ep = Sexpr.newCons(NIL, Sexpr.newSymbol("e")) DO
      S := Sexpr.newCons(
               Sexpr.newCons(
                   Sexpr.newSymbol("cp"),
                   ep),
               Sexpr.newCons(
                   Sexpr.newSymbol("v"),
                   Sexpr.newSymbol("s")));
      E := ep;
    END;
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcRAP)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("RAP [1]");
    skip_opcode(); do_rap();
    dump("RAP [2]");
    reset();
  END test_rap;

  (* (x.s) e (SEL c1 c0.c) d -> s e cx (c.d) | cx = равно(x, T) ? c1 : c0 *)
  PROCEDURE test_sel() RAISES {Error, Sexpr.Error} =
  BEGIN
    FOR x := FIRST(BOOLEAN) TO LAST(BOOLEAN) DO
      reset();
      S := Sexpr.newCons(
               Sexpr.cond(x),
               Sexpr.newSymbol("s"));
      E := Sexpr.newSymbol("e");
      C := Sexpr.newCons(
               Sexpr.newNumber(opcode(Opcode.opcSEL)),
               Sexpr.newCons(
                   Sexpr.newSymbol("c1"),
                   Sexpr.newCons(
                       Sexpr.newSymbol("c0"),
                       Sexpr.newSymbol("c"))));
      D := Sexpr.newSymbol("d");
      dump("SEL [1]");
      skip_opcode(); do_sel();
      dump("SEL [2]");
    END;
    reset();
  END test_sel;

  (* s e (JOIN) (c.d) -> s e c d *)
  PROCEDURE test_join() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newSymbol("s");
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcJOIN)),
             Sexpr.nil());
    D := Sexpr.newCons(
             Sexpr.newSymbol("c"),
             Sexpr.newSymbol("d"));
    dump("JOIN [1]");
    skip_opcode(); do_join();
    dump("JOIN [2]");
    reset();
  END test_join;

  (* ((a.b).s) e (CAR.c) d -> (a.s) e c d *)
  PROCEDURE test_car() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newCons(
                 Sexpr.newSymbol("a"),
                 Sexpr.newSymbol("b")),
             Sexpr.newSymbol("s"));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCAR)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("CAR [1]");
    skip_opcode(); do_car();
    dump("CAR [2]");
    reset();
  END test_car;

  (* ((a.b).s) e (CDR.c) d -> (b.s) e c d *)
  PROCEDURE test_cdr() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newCons(
                 Sexpr.newSymbol("a"),
                 Sexpr.newSymbol("b")),
             Sexpr.newSymbol("s"));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCDR)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("CDR [1]");
    skip_opcode(); do_cdr();
    dump("CDR [2]");
    reset();
  END test_cdr;

  (* (a.s) e (ATOM.c) d -> (t.s) e c d       | t = атом(a) ? T : F *)
  PROCEDURE test_atom() RAISES {Error, Sexpr.Error} =
  TYPE AtomList = ARRAY [1..3] OF Snode.T;
  VAR atomList: AtomList := AtomList { Sexpr.newNumber(137), Sexpr.newSymbol("sym"),
                                       Sexpr.newCons(Sexpr.newNumber(137), Sexpr.newSymbol("sym")) };
  BEGIN
    FOR x := FIRST(AtomList) TO LAST(AtomList) DO
      reset();
      S := Sexpr.newCons(
               atomList[x],
               Sexpr.newSymbol("s"));
      E := Sexpr.newSymbol("e");
      C := Sexpr.newCons(
               Sexpr.newNumber(opcode(Opcode.opcATOM)),
               Sexpr.newSymbol("c"));
      D := Sexpr.newSymbol("d");
      dump("ATOM [1]");
      skip_opcode(); do_atom();
      dump("ATOM [2]");
    END;
    reset();
  END test_atom;

  (* (a b.s) e (CONS.c) d -> ((a.b).s) e c d *)
  PROCEDURE test_cons() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newSymbol("a"),
             Sexpr.newCons(
                 Sexpr.newSymbol("b"),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCONS)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("CONS [1]");
    skip_opcode(); do_cons();
    dump("CONS [2]");
    reset();
  END test_cons;

  (* (a b.s) e (EQ.c) d -> (b = a.s) e c d *)
  PROCEDURE test_eq() RAISES {Error, Sexpr.Error} =
  TYPE AtomList = ARRAY [1..3] OF Snode.T;
  VAR atomList: AtomList := AtomList { Sexpr.newNumber(137), Sexpr.newSymbol("sym"),
                                       Sexpr.newCons(Sexpr.newNumber(137), Sexpr.newSymbol("sym")) };
  BEGIN
    FOR x := FIRST(AtomList) TO LAST(AtomList) DO
      FOR y := FIRST(AtomList) TO LAST(AtomList) DO
        reset();
        S := Sexpr.newCons(
                 atomList[x],
                 Sexpr.newCons(
                     atomList[y],
                     Sexpr.newSymbol("s")));
        E := Sexpr.newSymbol("e");
        C := Sexpr.newCons(
                 Sexpr.newNumber(opcode(Opcode.opcEQ)),
                 Sexpr.newSymbol("c"));
        D := Sexpr.newSymbol("d");
        dump("EQ [1]");
        skip_opcode(); do_eq();
        dump("EQ [2]");
        reset();
      END;
    END;
  END test_eq;

  (* (a b.s) e (ADD.c) d -> (b + a.s) e c d *)
  PROCEDURE test_add() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newNumber(13),
             Sexpr.newCons(
                 Sexpr.newNumber(117),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCONS)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("ADD [1]");
    skip_opcode(); do_add();
    dump("ADD [2]");
    reset();
  END test_add;

  (* (a b.s) e (SUB.c) d -> (b - a.s) e c d *)
  PROCEDURE test_sub() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newNumber(13),
             Sexpr.newCons(
                 Sexpr.newNumber(117),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCONS)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("SUB [1]");
    skip_opcode(); do_sub();
    dump("SUB [2]");
    reset();
  END test_sub;

  (* (a b.s) e (MUL.c) d -> (b * a.s) e c d *)
  PROCEDURE test_mul() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newNumber(13),
             Sexpr.newCons(
                 Sexpr.newNumber(119),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCONS)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("MUL [1]");
    skip_opcode(); do_mul();
    dump("MUL [2]");
    reset();
  END test_mul;

  (* (a b.s) e (DIV.c) d -> (b / a.s) e c d *)
  PROCEDURE test_div() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newNumber(13),
             Sexpr.newCons(
                 Sexpr.newNumber(119),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCONS)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("DIV [1]");
    skip_opcode(); do_div();
    dump("DIV [2]");
    reset();
  END test_div;

  (* (a b.s) e (REM.c) d -> (b % a.s) e c d *)
  PROCEDURE test_rem() RAISES {Error, Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newCons(
             Sexpr.newNumber(13),
             Sexpr.newCons(
                 Sexpr.newNumber(119),
                 Sexpr.newSymbol("s")));
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(
             Sexpr.newNumber(opcode(Opcode.opcCONS)),
             Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("REM [1]");
    skip_opcode(); do_rem();
    dump("REM [2]");
    reset();
  END test_rem;

  (* (a b.s) e (LEQ.c) d -> (b <= a.s) e c d *)
  PROCEDURE test_leq() RAISES {Error, Sexpr.Error} =
  TYPE AtomList = ARRAY [1..2] OF Snode.T;
  VAR atomList: AtomList := AtomList { Sexpr.newNumber(17), Sexpr.newNumber(137) };
  BEGIN
    FOR x := FIRST(AtomList) TO LAST(AtomList) DO
      FOR y := FIRST(AtomList) TO LAST(AtomList) DO
        reset();
        S := Sexpr.newCons(
                 atomList[x],
                 Sexpr.newCons(
                     atomList[y],
                     Sexpr.newSymbol("s")));
        E := Sexpr.newSymbol("e");
        C := Sexpr.newCons(
                 Sexpr.newNumber(opcode(Opcode.opcEQ)),
                 Sexpr.newSymbol("c"));
        D := Sexpr.newSymbol("d");
        dump("LEQ [1]");
        skip_opcode(); do_leq();
        dump("LEQ [2]");
        reset();
      END;
    END;
  END test_leq;

  (* s e (STOP) d -> s e (STOP) d | стоп *)
  PROCEDURE test_stop() RAISES {Sexpr.Error} =
  BEGIN
    reset();
    S := Sexpr.newSymbol("s");
    E := Sexpr.newSymbol("e");
    C := Sexpr.newCons(Sexpr.newNumber(opcode(Opcode.opcSTOP)), Sexpr.newSymbol("c"));
    D := Sexpr.newSymbol("d");
    dump("STOP [1]");
    dump("STOP [2]");
    reset();
  END test_stop;

BEGIN (*test*)
  test_ld();
  test_ldc();
  test_ldf();
  test_ap();
  test_ret();
  test_dum();
  test_rap();
  test_sel();
  test_join();
  test_car();
  test_cdr();
  test_atom();
  test_cons();
  test_eq();
  test_add();
  test_sub();
  test_mul();
  test_div();
  test_rem();
  test_leq();
  test_stop();
END test;

BEGIN
END SECD.
