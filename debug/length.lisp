(LETREC LENGTH
    (LENGTH LAMBDA (X)
        (IF (EQ X (QUOTE NIL)) (QUOTE 0)
            (ADD (LENGTH (CDR X)) (QUOTE 1)))))
