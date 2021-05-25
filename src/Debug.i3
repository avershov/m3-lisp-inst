INTERFACE Debug;

IMPORT Sexpr, Snode;

PROCEDURE sep() RAISES {};
PROCEDURE dump(str: TEXT; e: Snode.T) RAISES {Sexpr.Error};

END Debug.
