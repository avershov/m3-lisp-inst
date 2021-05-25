(LETREC gcd
    (gcd LAMBDA (a b)
        (IF (EQ a (QUOTE 0)) b
        (IF (LEQ a b)
            (gcd (SUB b a) a)
            (gcd (SUB a b) b)))))
