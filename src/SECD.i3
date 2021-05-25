INTERFACE SECD;

IMPORT Sexpr, Snode;

TYPE (* given opcode opc, its numerical code is 1 + ORD(opc) *)
  Opcode = { opcLD, opcLDC, opcLDF, opcAP, opcRET, opcDUM, opcRAP,
             opcSEL, opcJOIN, opcCAR, opcCDR, opcATOM, opcCONS, opcEQ,
             opcADD, opcSUB, opcMUL, opcDIV, opcREM, opcLEQ, opcSTOP };

EXCEPTION
  Error(TEXT);

PROCEDURE opcode(opc: Opcode): CARDINAL;
PROCEDURE run(fn, arglist: Snode.T): Snode.T RAISES {Error, Sexpr.Error};
PROCEDURE test() RAISES {Error, Sexpr.Error};

END SECD.
