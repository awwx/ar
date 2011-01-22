( (testis (string '(a (b (c d))) 4 5) "abcd45") )

( (testis (ssyntax 'abc) nil) )
( (testis (ssyntax 'a:b) t) )

( (testis (ac-symbol->chars 'abc) '(#\a #\b #\c)) )

( (testis (ac-tokens [is _ #\:] '(#\a #\: #\b #\c) nil nil nil)
          '((#\a) (#\b #\c))) )

( (testis (ac-chars->value '(#\1 #\2 #\3)) 123) )

( (testis (ac-expand-compose 'abc:d:e) '(compose abc d e)) )
( (testis (ac-expand-compose '~:a) '(compose no a)) )
( (testis (ac-expand-compose '~abc:def) '(compose (complement abc) def)) )

( (testis (car:+ '(1 2) '(3 4)) 1) )
( (testis (~acons 3) t) )

( (testis (ac-build-sexpr '((#\a #\b)) nil) 'ab) )
( (testis (ac-build-sexpr '((#\a) #\!) nil) '(get 'a)) )
( (testis (ac-build-sexpr '((#\a) #\! (#\b)) nil) '(b 'a)) )
( (testis (ac-build-sexpr '((#\a) #\. (#\b)) nil) '(b a)) )

( (testis (ac-expand-sexpr 'ab!cde) '(ab 'cde)) )

( (testis acons!a nil) )
( (testis type.cons 'fn) )

( (testis ((andf acons cdr) '(1 . 2)) 2) )

( (testis (ac-expand-and 'acons&cdr) '(andf acons cdr)) )

( (testis (acons&cdr '(1 . 2)) 2) )

( (testis (and&or 3) 3) )

( (testis (ac-decompose '(and or) '(3)) '(and (or 3))) )

( (testis (and:or 3) 3) )

( (testis (~and 3 nil) t) )

( (testis ((racket-fn '+) 2 3) 5) )

( (testis (newstring 5 #\A) "AAAAA") )

( (testis (max) nil) )
( (testis (max 1 3 6 3 7 2) 7) )

( (testis (map [coerce (+ (coerce _ 'int) 1) 'char] "ABC") "BCD") )
( (testis (map + '(1 2 3) '(4 5 6)) '(5 7 9)) )


( (testis (tostring (warn "foo" 1 2)) "Warning: foo. 1 2 \n") )


( (testis (type (make-thread-cell 5)) 'thread-cell) )


;; don't know how to test atomic adequately

( (testis (atomic-invoke (fn () 123))
          123) )
( (testis (atomic-invoke (fn ()
                           (atomic-invoke (fn () 123))))
          123) )


( (= x '(1 2 3))
  (= (car x) 'one)
  (testis x '(one 2 3))

  (= (cadr x) 'two)
  (testis x '(one two 3))

  (= (cdr x) 'blam)
  (testis x '(one . blam))
)

( (testis (accum a (down x 5 0 (a x))) '(5 4 3 2 1 0)) )

( (testis (accum a (each (x y) '((1 2) (3 4) (5 6)) (a (+ x y)))) '(3 7 11)) )

( (testis (accum a (each c "abc" (a c))) '(#\a #\b #\c)) )
