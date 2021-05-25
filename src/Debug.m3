MODULE Debug;

IMPORT Sexpr, Sfmt, Snode, Stdio, Thread, Wr;

VAR
  DEBUG := TRUE;

<*FATAL Thread.Alerted, Wr.Failure*>

PROCEDURE sep() RAISES {} =
BEGIN
  IF DEBUG THEN
    Wr.PutText(Stdio.stdout, "#----------------" & "\n");
    Wr.Flush(Stdio.stdout);
  END;
END sep;

PROCEDURE dump(str: TEXT; e: Snode.T) RAISES {Sexpr.Error} =
BEGIN
  IF DEBUG THEN
    Wr.PutText(Stdio.stdout, str & " = " & Sfmt.toText(e) & "\n");
    Wr.Flush(Stdio.stdout);
  END;
END dump;

BEGIN
END Debug.
