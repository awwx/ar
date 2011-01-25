(testis ([+ 3 _] 4) 7)

(testis (aif 3 it) 3)
(testis (aif nil 3 nil 4 5 it) 5)

(testis (string '(a (b (c d))) 4 5) "abcd45")

(testis (ssyntax 'abc) nil)
(testis (ssyntax 'a:b) t)

(testis (ac-symbol->chars 'abc) '(#\a #\b #\c))

(testis (ac-tokens [is _ #\:] '(#\a #\: #\b #\c) nil nil nil)
        '((#\a) (#\b #\c)))


(testis (ac-chars->value '(#\1 #\2 #\3)) 123)

(testis (ac-expand-compose 'abc:d:e) '(compose abc d e))
(testis (ac-expand-compose '~:a) '(compose no a))
(testis (ac-expand-compose '~abc:def) '(compose (complement abc) def))

(testis (car:+ '(1 2) '(3 4)) 1)
(testis (~acons 3) t)

(testis (ac-build-sexpr '((#\a #\b)) nil) 'ab)
(testis (ac-build-sexpr '((#\a) #\!) nil) '(get 'a))
(testis (ac-build-sexpr '((#\a) #\! (#\b)) nil) '(b 'a))
(testis (ac-build-sexpr '((#\a) #\. (#\b)) nil) '(b a))

(testis (ac-expand-sexpr 'ab!cde) '(ab 'cde))

(testis acons!a nil)
(testis type.cons 'fn)

(testis ((andf acons cdr) '(1 . 2)) 2)

(testis (ac-expand-and 'acons&cdr) '(andf acons cdr))

(testis (acons&cdr '(1 . 2)) 2)

(testis (and&or 3) 3)

(testis (ac-decompose '(and or) '(3)) '(and (or 3)))

(testis (and:or 3) 3)

(testis (~and 3 nil) t)

(testis ((racket-fn '+) 2 3) 5)

(testis (newstring 5 #\A) "AAAAA")

(testis (max) nil)
(testis (max 1 3 6 3 7 2) 7)

(testis (map [coerce (+ (coerce _ 'int) 1) 'char] "ABC") "BCD")
(testis (map + '(1 2 3) '(4 5 6)) '(5 7 9))


(testis (tostring (warn "foo" 1 2)) "Warning: foo. 1 2 \n")


(testis (type (make-thread-cell 5)) 'thread-cell)


;; don't know how to test atomic adequately

(testis (atomic-invoke (fn () 123))
        123)
(testis (atomic-invoke (fn ()
                         (atomic-invoke (fn () 123))))
        123)


(do (= x '(1 2 3))
    (= (car x) 'one)
    (testis x '(one 2 3))

    (= (cadr x) 'two)
    (testis x '(one two 3))

    (= (cdr x) 'blam)
    (testis x '(one . blam)))

(testis (accum a (down x 5 0 (a x))) '(5 4 3 2 1 0))

(testis (accum a (each (x y) '((1 2) (3 4) (5 6)) (a (+ x y)))) '(3 7 11))

(testis (accum a (each c "abc" (a c))) '(#\a #\b #\c))

; todo test for whilet

(testis (last '(1 2 3 4 5 6)) 6)

(testis (rem 3 '(1 2 3 4 5 6)) '(1 2 4 5 6))

(testis (keep 3 '(1 2 3 4 5 6)) '(3))

(testis (trues acons '(1 2 '(3 4) 5 '(6))) '(t t))

(do (= x '(1 2 3))
    (push 4 x)
    (testis x '(4 1 2 3)))

(do (= x 'one)
    (= y 'two)
    (swap x y)
    (testis x 'two)
    (testis y 'one))

(do (= x '(1 2 3 4 5))
    (swap (car x) (cadr x))
    (testis x '(2 1 3 4 5)))

(do (= x '(1 2 3 4))
    (rotate (car x) (cadr x))
    (testis x '(2 1 3 4)))

(do (= x '(1 2 3 4))
    (= y 5)
    (rotate (car x) (cadr x) y)
    (testis x '(2 5 3 4))
    (testis y 1))

(do (= x '(1 2 3 4))
    (testis (pop x) 1)
    (testis x '(2 3 4)))

(testis (adjoin 1 '(2 3 4)) '(1 2 3 4))
(testis (adjoin 3 '(2 3 4)) '(2 3 4))

(do (= x '(1 2 3 4))
    (pushnew 0 x)
    (testis x '(0 1 2 3 4)))

(do (= x '(1 2 3 4))
    (pushnew 3 x)
    (testis x '(1 2 3 4)))

(do (= x '(a (1 2 3 4) b))
    (pull 3 (cadr x))
    (testis x '(a (1 2 4) b)))

(do (= x '(1 2 3 4))
    (togglemem 5 x)
    (testis x '(5 1 2 3 4)))

(do (= x '(1 2 3 4))
    (togglemem 3 x)
    (testis x '(1 2 4)))

(do (= x '(0 1 2 3 4))
    (++ (x 3))
    (testis x '(0 1 2 4 4)))

(do (= x 7)
    (-- x)
    (testis x 6))

(do (= x 7) 
    (zap + x 1)
    (testis x 8))

(testis (tostring (prt 1 2 nil 3)) "123")

(do (= x '(1 2 3 4))
    (= y 5)
    (wipe (cadr x) y)
    (testis x '(1 nil 3 4))
    (testis y nil))

(do (= x '(1 nil 3 4))
    (= y nil)
    (set (cadr x) y)
    (testis x '(1 t 3 4))
    (testis y t))

; todo test for awhen

(testis (aand (+ 3 4) (+ it 5)) 12)

(testis (let x 5 (drain (-- x) 0)) '(4 3 2 1))

; todo test for whiler

; todo test for consif

(testis (flat '(1 (2 (3 4 (5) 6) 7) 8 9)) '(1 2 3 4 5 6 7 8 9))

; todo test for check

(testis (pos 'c '(a b c d)) 2)
(testis (pos #\c "abcd") 2)

(testis (even 0) t)
(testis (even 1) nil)
(testis (odd 1) t)

(testis (tostring (system "echo hello")) "hello\n")

(do (system "echo abc >/tmp/foo")
    (w/infile s "/tmp/foo"
      (testis (readc s) #\a)
      (testis (readc s) #\b)))

(do (w/outfile s "/tmp/foo" (disp "hi" s))
    (testis (tostring (system "cat /tmp/foo")) "hi"))

(testis (let s (outstring) (disp "foo" s) (inside s)) "foo")
(testis (w/outstring s (disp "foo" s) (inside s)) "foo")

(do (w/outfile s "/tmp/foo" (disp "hello" s))
    (w/appendfile s "/tmp/foo" (disp " there" s))
    (testis (tostring (system "cat /tmp/foo")) "hello there"))

(do (w/outfile s "/tmp/foo"
      (w/stdout s
        (disp "xyzzy")))
    (testis (tostring (system "cat /tmp/foo")) "xyzzy"))

(testis (fromstring "abc" (readc)) #\a)

(testis (w/instring s "abc" (allchars s)) "abc")

(do (w/outfile s "/tmp/foo" (disp "1 2 3" s))
    (testis (readfile "/tmp/foo") '(1 2 3)))

(do (w/outfile s "/tmp/foo" (disp "123 456" s))
    (testis (readfile1 "/tmp/foo") 123))

(do (system "echo '1 2 (3 4)' >/tmp/foo")
    (testis (w/infile s "/tmp/foo" (readall s)) '(1 2 (3 4))))

(testis (readall "1 2 (3 4)") '(1 2 (3 4)))

(do (system "echo xyzzy >/tmp/foo")
    (testis (filechars "/tmp/foo") "xyzzy\n"))

(do (system "echo abc >/tmp/foo")
    (mvfile "/tmp/foo" "/tmp/bar")
    (testis (filechars "/tmp/bar") "abc\n"))

(do (writefile '(a b "cd" 5 6) "/tmp/foo")
    (testis (filechars "/tmp/foo") "(a b \"cd\" 5 6)"))

(testis (rand 1) 0)

(testis (tostring (w/rand (fn (n) 1)
                    (rand-choice (pr "a") (pr "b") (pr "c"))))
        "b")
