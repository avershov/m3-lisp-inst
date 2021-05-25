INTERFACE Sscan;

IMPORT Snode;
IMPORT Rd, Thread;

EXCEPTION
  Error(TEXT);

PROCEDURE fromRd(rd: Rd.T): Snode.T RAISES {Error, Rd.Failure, Thread.Alerted};

END Sscan.
