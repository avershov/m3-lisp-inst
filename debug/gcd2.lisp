(LETREC gcd
    (gcd LAMBDA (a b)
        (LET
            (IF (EQ a (QUOTE 0)) b (gcd (SUB x y) y))
            (x max a b)
            (y min a b)))
    (min LAMBDA (a b) (IF (LEQ a b) a b))
    (max LAMBDA (a b) (IF (LEQ a b) b a)))
