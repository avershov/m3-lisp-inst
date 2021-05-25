(LETREC reverse
    (reverse LAMBDA (x)
        (rev x (QUOTE NIL)))
    (rev LAMBDA (x y)
        (IF (EQ x (QUOTE NIL)) y
            (rev (CDR x) (CONS (CAR x) y)))))
