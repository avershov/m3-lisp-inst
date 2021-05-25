INTERFACE Snode;

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    marked(): BOOLEAN;
    markNode(state: BOOLEAN);
    markTree(state: BOOLEAN);
  END;

  Cons <: PublicCons;
  PublicCons = T OBJECT
  METHODS
    car(): T;
    cdr(): T;
  END;

  Atom <: PublicAtom;
  PublicAtom = T OBJECT
  END;

  Number <: PublicNumber;
  PublicNumber = Atom OBJECT
  METHODS
    value(): INTEGER;
  END;

  Symbol <: PublicSymbol;
  PublicSymbol = Atom OBJECT
  METHODS
    value(): TEXT;
  END;

END Snode.
