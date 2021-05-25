MODULE Sexpr EXPORTS Snode, Sexpr;

IMPORT AtomTable, Fmt, Stats;

TYPE
  Private = Public OBJECT
    m_mark    : BOOLEAN := FALSE;
    m_visited : BOOLEAN := FALSE;
  END;

  PrivateCons = PublicCons OBJECT
    m_car: T := NIL;
    m_cdr: T := NIL;
  METHODS
    init(car, cdr: T): Cons;
  END;

  PrivateAtom = PublicAtom OBJECT
  END;

  PrivateNumber = PublicNumber OBJECT
    m_value: INTEGER := 0;
  METHODS
    init(value: INTEGER): Number;
  END;

  PrivateSymbol = PublicSymbol OBJECT
    m_id: CARDINAL := 0;
  METHODS
    init(string: TEXT): Symbol;
  END;

REVEAL
  T = Private BRANDED OBJECT
  OVERRIDES
    marked   := T_marked;
    markNode := T_markNode;
    markTree := T_markTree;
  END;

  Cons = PrivateCons BRANDED OBJECT
  OVERRIDES
    init := Cons_init;
    car  := Cons_car;
    cdr  := Cons_cdr;
  END;

  Atom = PrivateAtom BRANDED OBJECT
  END;

  Number = PrivateNumber BRANDED OBJECT
  OVERRIDES
    init  := Number_init;
    value := Number_value;
  END;

  Symbol = PrivateSymbol BRANDED OBJECT
  OVERRIDES
    init  := Symbol_init;
    value := Symbol_value;
  END;

PROCEDURE isCons(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Cons);
END isCons;

PROCEDURE isAtom(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Atom);
END isAtom;

PROCEDURE isNumber(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Number);
END isNumber;

PROCEDURE isSymbol(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Symbol);
END isSymbol;

PROCEDURE isNil(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Symbol) AND (NARROW(node, Symbol).m_id = nilAtom.m_id);
END isNil;

PROCEDURE isTrue(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Symbol) AND (NARROW(node, Symbol).m_id = condAtoms[TRUE].m_id);
END isTrue;

PROCEDURE isFalse(node: T): BOOLEAN =
BEGIN
  RETURN (node # NIL) AND ISTYPE(node, Symbol) AND (NARROW(node, Symbol).m_id = condAtoms[FALSE].m_id);
END isFalse;

PROCEDURE isAtomsAndEQ(a, b: T): BOOLEAN =
BEGIN
  IF (a # NIL) AND (b # NIL) THEN
    IF (ISTYPE(a, Number) AND ISTYPE(b, Number)) THEN
      RETURN (NARROW(a, Number).m_value = NARROW(b, Number).m_value);
    END;
    IF (ISTYPE(a, Symbol) AND ISTYPE(b, Symbol)) THEN
      RETURN (NARROW(a, Symbol).m_id = NARROW(b, Symbol).m_id);
    END;
  END;
  RETURN FALSE;
END isAtomsAndEQ;

PROCEDURE isSymbolEQ(a: T; b: Symbol): BOOLEAN =
BEGIN
  IF (a # NIL) AND (b # NIL) THEN
    IF ISTYPE(a, Symbol) THEN
      RETURN (NARROW(a, Symbol).m_id = b.m_id);
    END;
  END;
  RETURN FALSE;
END isSymbolEQ;

PROCEDURE cons(node: T): Cons RAISES {Error} =
BEGIN
  TYPECASE node OF
  | NULL => RAISE Error("[cast node -> Cons] node is NIL");
  | Number(e) => RAISE Error("[cast node -> Cons] node is Number (value = " & Fmt.Int(e.value()) & ")");
  | Symbol(e) => RAISE Error("[cast node -> Cons] node is Symbol (string = \"" & e.value() & "\")");
  | Atom => RAISE Error("[cast node -> Cons] node is Atom");
  | Cons(e) => RETURN e;
  ELSE
    RAISE Error("[cast node -> Cons] node has unknown type");
  END;
(*
  IF (node = NIL) THEN RAISE Error("[cast node -> Cons] node is NIL"); END;
  IF (NOT ISTYPE(node, Cons)) THEN RAISE Error("[cast node -> Cons] node"); END;
  RETURN NARROW(node, Cons);
*)
END cons;

PROCEDURE atom(node: T): Atom RAISES {Error} =
BEGIN
  TYPECASE node OF
  | NULL => RAISE Error("[cast node -> Atom] node is NIL");
  | Atom(e) => RETURN e;
  | Cons => RAISE Error("[cast node -> Atom] node is Cons");
  ELSE
    RAISE Error("[cast node -> Atom] node has unknown type");
  END;
(*
  IF (node = NIL) THEN RAISE Error("[cast node -> atom] node is NIL"); END;
  IF (NOT ISTYPE(node, Atom)) THEN RAISE Error(""); END;
  RETURN NARROW(node, Atom);
*)
END atom;

PROCEDURE number(node: T): Number RAISES {Error} =
BEGIN
  TYPECASE node OF
  | NULL => RAISE Error("[cast node -> Number] node is NIL");
  | Number(e) => RETURN e;
  | Symbol(e) => RAISE Error("[cast node -> Number] node is Symbol (string = \"" & e.value() & "\")");
  | Atom => RAISE Error("[cast node -> Number] node is Atom");
  | Cons => RAISE Error("[cast node -> Number] node is Cons");
  ELSE
    RAISE Error("[cast node -> Number] node has unknown type");
  END;
(*
  IF (node = NIL) THEN RAISE Error("[cast node -> number] node is NIL"); END;
  IF (NOT ISTYPE(node, Number)) THEN RAISE Error(""); END;
  RETURN NARROW(node, Number);
*)
END number;

PROCEDURE symbol(node: T): Symbol RAISES {Error} =
BEGIN
  TYPECASE node OF
  | NULL => RAISE Error("[cast node -> Symbol] node is NIL");
  | Number(e) => RAISE Error("[cast node -> Symbol] node is Number (value = " & Fmt.Int(e.value()) & ")");
  | Symbol(e) => RETURN e;
  | Atom => RAISE Error("[cast node -> Symbol] node is Atom");
  | Cons => RAISE Error("[cast node -> Symbol] node is Cons");
  ELSE
    RAISE Error("[cast node -> Symbol] node has unknown type");
  END;
(*
  IF (node = NIL) THEN RAISE Error("[cast node -> symbol] node is NIL"); END;
  IF (NOT ISTYPE(node, Symbol)) THEN RAISE Error(""); END;
  RETURN NARROW(node, Symbol);
*)
END symbol;

PROCEDURE car(node: T): T RAISES {Error} =
BEGIN
  RETURN cons(node).car();
END car;

PROCEDURE cdr(node: T): T RAISES {Error} =
BEGIN
  RETURN cons(node).cdr();
END cdr;

PROCEDURE caar(node: T): T RAISES {Error} =
BEGIN
  RETURN car(car(node));
END caar;

PROCEDURE cadr(node: T): T RAISES {Error} =
BEGIN
  RETURN car(cdr(node));
END cadr;

PROCEDURE cdar(node: T): T RAISES {Error} =
BEGIN
  RETURN cdr(car(node));
END cdar;

PROCEDURE cddr(node: T): T RAISES {Error} =
BEGIN
  RETURN cdr(cdr(node));
END cddr;

PROCEDURE cdrn(node: T; n: CARDINAL): T RAISES {Error} =
BEGIN
  FOR i := 1 TO n DO node := cdr(node); END;
  RETURN node;
END cdrn;

PROCEDURE complete(node, car: T) RAISES {Error} =
BEGIN
  WITH c = cons(node) DO
    IF (c.m_car # NIL) THEN RAISE Error("[node complete] CAR is not NIL"); END;
    c.m_car := car;
  END;
END complete;

PROCEDURE newCons(car, cdr: T): Cons =
BEGIN
  Stats.log(Stats.Kind.NewCons);
  RETURN NEW(Cons).init(car, cdr);
END newCons;

PROCEDURE newNumber(value: INTEGER): Number =
BEGIN
  Stats.log(Stats.Kind.NewNumber);
  RETURN NEW(Number).init(value);
END newNumber;

PROCEDURE newSymbol(string: TEXT): Symbol =
BEGIN
  Stats.log(Stats.Kind.NewSymbol);
  RETURN NEW(Symbol).init(string);
END newSymbol;

PROCEDURE nil(): Symbol =
BEGIN
  RETURN nilAtom;
END nil;

PROCEDURE true(): Symbol =
BEGIN
  RETURN condAtoms[TRUE];
END true;

PROCEDURE false(): Symbol =
BEGIN
  RETURN condAtoms[FALSE];
END false;

PROCEDURE cond(condition: BOOLEAN): Symbol =
BEGIN
  RETURN condAtoms[condition];
END cond;

PROCEDURE pop(VAR s: T): T RAISES {Error} =
BEGIN
  WITH p = car(s) DO
    s := cdr(s);
    RETURN p;
  END;
END pop;

PROCEDURE push(p: T; VAR s: T) RAISES {} =
BEGIN
  s := newCons(p, s);
END push;

PROCEDURE T_marked(self: T): BOOLEAN =
BEGIN
  RETURN self.m_mark;
END T_marked;

PROCEDURE T_markNode(self: T; state: BOOLEAN) =
BEGIN
  self.m_mark := state;
END T_markNode;

PROCEDURE T_markTree(self: T; state: BOOLEAN) =
BEGIN
  self.m_mark := state;
  IF isCons(self) AND (NOT self.m_visited) THEN
    WITH c = NARROW(self, Cons) DO
      self.m_visited := TRUE;
      WITH car = c.car() DO IF (car # NIL) THEN car.markTree(state); END; END;
      WITH cdr = c.cdr() DO IF (cdr # NIL) THEN cdr.markTree(state); END; END;
      self.m_visited := FALSE;
    END;
  END;
END T_markTree;

PROCEDURE Cons_init(self: Cons; car, cdr: T): Cons =
BEGIN
  self.m_car := car;
  self.m_cdr := cdr;
  RETURN self;
END Cons_init;

PROCEDURE Cons_car(self: Cons): T =
BEGIN
  Stats.log(Stats.Kind.Car);
  RETURN self.m_car;
END Cons_car;

PROCEDURE Cons_cdr(self: Cons): T =
BEGIN
  Stats.log(Stats.Kind.Cdr);
  RETURN self.m_cdr;
END Cons_cdr;

PROCEDURE Number_init(self: Number; value: INTEGER): Number =
BEGIN
  self.m_value := value;
  RETURN self;
END Number_init;

PROCEDURE Number_value(self: Number): INTEGER =
BEGIN
  RETURN self.m_value;
END Number_value;

PROCEDURE Symbol_init(self: Symbol; string: TEXT): Symbol =
BEGIN
  self.m_id := atomTable.putString(string);
  RETURN self;
END Symbol_init;

PROCEDURE Symbol_value(self: Symbol): TEXT =
BEGIN
  RETURN atomTable.findId(self.m_id);
END Symbol_value;

TYPE
  CondArray = ARRAY BOOLEAN OF Symbol;

VAR
  atomTable: AtomTable.T := NIL;
  nilAtom: Symbol := NIL;
  condAtoms: CondArray := CondArray{ NIL, .. };

BEGIN
  atomTable := AtomTable.new();
  nilAtom := newSymbol("NIL");
  condAtoms[TRUE] := newSymbol("T");
  condAtoms[FALSE] := newSymbol("F");
END Sexpr.
