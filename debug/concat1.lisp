(LETREC CONCAT
    (CONCAT LAMBDA (X Y)
        (IF (EQ X (QUOTE NIL)) Y
            (CONS (CAR X) (CONCAT (CDR X) Y)))))
