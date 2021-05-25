INTERFACE Compile;

IMPORT Sexpr, Snode;

PROCEDURE prog(e: Snode.T): Snode.T RAISES {Error, Sexpr.Error};

EXCEPTION
  Error(TEXT);

END Compile.
