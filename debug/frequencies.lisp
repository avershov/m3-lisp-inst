
(LETREC frequencies

    (frequencies LAMBDA (e)
        (freq (collection e) e (QUOTE NIL)))

    (freq LAMBDA (a e s)
        (IF (EQ a (QUOTE NIL)) s
            (freq (CDR a) e (CONS (instances (CAR a) e) s))))

    (instances LAMBDA (x e)
        (CONS x (CONS (inst x e) (QUOTE NIL))))

    (inst LAMBDA (x e)
        (IF (EQ e (QUOTE NIL)) (QUOTE 0)
        (IF (ATOM e) (IF (EQ x e) (QUOTE 1) (QUOTE 0))
        (ADD (inst x (CAR e))
             (inst x (CDR e))))))

    (collection LAMBDA (e)
        (coll e (QUOTE NIL)))

    (coll LAMBDA (e s)
        (IF (EQ e (QUOTE NIL)) s
        (IF (ATOM e) (insert e s)
        (coll (CAR e) (coll (CDR e) s)))))

    (insert LAMBDA (x s)
        (IF (member x s) s (CONS x s)))

    (member LAMBDA (x s)
        (IF (EQ s (QUOTE NIL)) (QUOTE F)
        (IF (EQ x (CAR s)) (QUOTE T)
        (member x (CDR s)))))
)
