INTERFACE Sfmt;

IMPORT Sexpr, Snode;

PROCEDURE toText(e: Snode.T): TEXT RAISES {Sexpr.Error};

END Sfmt.
