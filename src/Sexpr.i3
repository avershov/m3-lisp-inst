INTERFACE Sexpr;

IMPORT Snode;

EXCEPTION
  Error(TEXT);

PROCEDURE isCons(node: Snode.T): BOOLEAN;
PROCEDURE isAtom(node: Snode.T): BOOLEAN;
PROCEDURE isNumber(node: Snode.T): BOOLEAN;
PROCEDURE isSymbol(node: Snode.T): BOOLEAN;
PROCEDURE isNil(node: Snode.T): BOOLEAN;
PROCEDURE isTrue(node: Snode.T): BOOLEAN;
PROCEDURE isFalse(node: Snode.T): BOOLEAN;
PROCEDURE isAtomsAndEQ(a, b: Snode.T): BOOLEAN;
PROCEDURE isSymbolEQ(a: Snode.T; b: Snode.Symbol): BOOLEAN;

PROCEDURE cons(node: Snode.T): Snode.Cons RAISES {Error};
PROCEDURE atom(node: Snode.T): Snode.Atom RAISES {Error};
PROCEDURE number(node: Snode.T): Snode.Number RAISES {Error};
PROCEDURE symbol(node: Snode.T): Snode.Symbol RAISES {Error};

PROCEDURE car(node: Snode.T): Snode.T RAISES {Error};
PROCEDURE cdr(node: Snode.T): Snode.T RAISES {Error};

PROCEDURE caar(node: Snode.T): Snode.T RAISES {Error};
PROCEDURE cadr(node: Snode.T): Snode.T RAISES {Error};
PROCEDURE cdar(node: Snode.T): Snode.T RAISES {Error};
PROCEDURE cddr(node: Snode.T): Snode.T RAISES {Error};

PROCEDURE cdrn(node: Snode.T; n: CARDINAL): Snode.T RAISES {Error};

PROCEDURE complete(node, car: Snode.T) RAISES {Error};

PROCEDURE newCons(car, cdr: Snode.T): Snode.Cons;
PROCEDURE newNumber(value: INTEGER): Snode.Number;
PROCEDURE newSymbol(string: TEXT): Snode.Symbol;

PROCEDURE nil(): Snode.Symbol;
PROCEDURE true(): Snode.Symbol;
PROCEDURE false(): Snode.Symbol;
PROCEDURE cond(condition: BOOLEAN): Snode.Symbol;

PROCEDURE pop(VAR s: Snode.T): Snode.T RAISES {Error};
PROCEDURE push(p: Snode.T; VAR s: Snode.T);

END Sexpr.
