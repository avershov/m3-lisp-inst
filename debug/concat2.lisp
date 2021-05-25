(LETREC concat
    (concat LAMBDA (x y)
        (IF (EQ y (QUOTE NIL)) x (conc x y)))
    (conc LAMBDA (x y)
        (IF (EQ x (QUOTE NIL)) y (CONS (CAR x) (conc (CDR x) y)))))
