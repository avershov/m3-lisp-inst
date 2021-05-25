MODULE Main;

IMPORT Compile, Debug, Sexpr, Sfmt, Snode, Sscan, Stats;
IMPORT Atom, AtomList, FileRd, Fmt, OSError, Params, Rd, SECD, Stdio, Text, Thread, Wr;

VAR
  DEBUG := FALSE;
  STATS := FALSE;
  TEST  := FALSE;

<*FATAL Rd.Failure, Thread.Alerted, Wr.Failure*>

PROCEDURE atomListToText(list: AtomList.T): TEXT =
VAR
  str: TEXT := "{";
  sep: TEXT := "";
BEGIN
  FOR i := 0 TO AtomList.Length(list)-1 DO
    str := str & sep & Atom.ToText(AtomList.Nth(list, i));
    sep := ", ";
  END;
  str := str & "}";
  RETURN str;
END atomListToText;

VAR
  rdc: Rd.T := NIL;
  rdargs: Rd.T := NIL;
  src, c, args, res: Snode.T := NIL;

TYPE
  Command = {Compile, Execute, Run};

EXCEPTION
  Exit(TEXT);

PROCEDURE usage(wr: Wr.T) RAISES {Exit} =
BEGIN
  Wr.PutText(wr,
             "Instrumental LISP utility & SECD machine; written in Modula-3 upon studying of Peter Henderson book" & "\n" &
             "Copyleft(ɔ) 2019 alex@home; no any warranty provided ))" & "\n\n" &
             "Usage: secd command file1 {file2} >outfile" & "\n\n" &
             "where command is {comp | exec | run}" & "\n\n" &
             "compile: secd comp file.lisp >file.secd" & "\n" &
             "execute: secd exec file.secd {file.args...}" & "\n" &
             "run:     secd run file.lisp {file.args...}" & "\n\n");
  RAISE Exit(NIL);
END usage;

PROCEDURE getCommand(): Command RAISES {Exit} =
VAR
  command: Command;
  required := 0;
BEGIN
  IF (Params.Count < 2) THEN usage(Stdio.stderr); END;
  WITH str = Params.Get(1) DO
    IF Text.Equal(str, "comp") THEN
      command := Command.Compile;
      required := 1;
    ELSIF Text.Equal(str, "exec") THEN
      command := Command.Execute;
      required := 2;
    ELSIF Text.Equal(str, "run") THEN
      command := Command.Run;
      required := 2;
    ELSE
      RAISE Exit("incorrect command: \"" & str & "\"");
    END;
    IF (Params.Count < 2 + required) THEN usage(Stdio.stderr); END;
  END;
  RETURN command;
END getCommand;

PROCEDURE stats(wr: Wr.T) =
VAR len := 0;
BEGIN
  IF STATS THEN
    Wr.PutText(wr, "\n" & "Statistics:" & "\n");
    FOR what := FIRST(Stats.Kind) TO LAST(Stats.Kind) DO
      len := MAX(len, Text.Length(Stats.name(what)));
    END;
    FOR what := FIRST(Stats.Kind) TO LAST(Stats.Kind) DO
      WITH name = Fmt.Pad(Stats.name(what), len, align := Fmt.Align.Left) DO
        Wr.PutText(wr, name & " : " & Fmt.Int(Stats.get(what)) & "\n");
      END;
    END;
  END;
END stats;

PROCEDURE getArglist(first, last: CARDINAL): Snode.T RAISES {Sscan.Error, OSError.E} =
VAR
  rd: Rd.T := NIL;
  list: Snode.T := Sexpr.nil();
BEGIN
  TRY
    FOR i := last TO first BY -1 DO
      rd := FileRd.Open(Params.Get(i));
      WITH arg = Sscan.fromRd(rd) DO
        Sexpr.push(arg, list);
      END;
      Rd.Close(rd);
      rd := NIL;
    END;
  FINALLY
    IF (rd # NIL) THEN
      Rd.Close(rd);
      rd := NIL;
    END;
  END;
  RETURN list;
END getArglist;

PROCEDURE main() =
BEGIN
  (*testPrint();*)
  TRY
    TRY
      IF TEST THEN
        Wr.PutText(Stdio.stdout, "*** SECD machine test(s) ***" & "\n");
        Wr.Flush(Stdio.stdout);
        SECD.test();
        Wr.PutText(Stdio.stdout, "*** SECD test(s) finished ***" & "\n");
        Wr.Flush(Stdio.stdout);
      ELSE
        WITH command = getCommand() DO
          CASE command OF
          | Command.Compile =>
            BEGIN
              rdc := FileRd.Open(Params.Get(2));
              src := Sscan.fromRd(rdc);
              res := Compile.prog(src);
              Wr.PutText(Stdio.stdout, Sfmt.toText(res) & "\n");
            END;
          | Command.Execute, Command.Run =>
            BEGIN
              rdc := FileRd.Open(Params.Get(2));
              IF command = Command.Execute THEN
                IF DEBUG THEN Debug.dump("Main.main [0] c", c); END;
                c := Sscan.fromRd(rdc);
                IF DEBUG THEN Debug.dump("Main.main [2] c", c); END;
              ELSE
                IF DEBUG THEN Debug.dump("Main.main [0] src", src); END;
                src := Sscan.fromRd(rdc);
                IF DEBUG THEN Debug.dump("Main.main [1] src", src); END;
                c := Compile.prog(src);
                IF DEBUG THEN Debug.dump("Main.main [2] c", c); END;
              END;
              args := getArglist(3, Params.Count - 1);
              IF DEBUG THEN Debug.dump("Main.main [3] c", c); END;
              IF DEBUG THEN Debug.dump("Main.main [3] args", args); END;
              res := SECD.run(c, args);
              IF DEBUG THEN Debug.dump("Main.main [4] res", res); END;
              Wr.PutText(Stdio.stdout, Sfmt.toText(res) & "\n");
            END;
          END;
        END;
      END;
    EXCEPT
    | Compile.Error(str) => Wr.Flush(Stdio.stdout); Wr.PutText(Stdio.stderr, "Compile: " & str & "\n");
    | Sscan.Error(str) => Wr.Flush(Stdio.stdout); Wr.PutText(Stdio.stderr, "Scan: " & str & "\n");
    | SECD.Error(str) => Wr.Flush(Stdio.stdout); Wr.PutText(Stdio.stderr, "SECD: " & str & "\n");
    | Sexpr.Error(str) => Wr.Flush(Stdio.stdout); Wr.PutText(Stdio.stderr, "Sexpr: " & str & "\n");
    | OSError.E(code) => Wr.Flush(Stdio.stdout); Wr.PutText(Stdio.stderr, "ERROR: OSError.E " & atomListToText(code) & "\n");
    | Exit(str) => IF (str # NIL) THEN Wr.Flush(Stdio.stdout); Wr.PutText(Stdio.stderr, "Error: " & str & "\n"); END;
    END;
  FINALLY
    IF rdargs # NIL THEN Rd.Close(rdargs); END;
    IF rdc # NIL THEN Rd.Close(rdc); END;
  END;
  stats(Stdio.stdout);
  Wr.Flush(Stdio.stdout);
END main;

BEGIN
  main();
END Main.
