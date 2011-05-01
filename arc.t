(testis (w/instring s "" (readc s 'end)) 'end)

(testis (do (assign a 1)
            (assign b 2)
            (assign c 3)
            (list a b c))
        '(1 2 3))

(testis (do (safeset a 123) a) 123)

(testis
 (let p (outstring)
   (w/stderr p
     (assign a 123)
     (safeset a 456))
   (inside p))
 "*** redefining a\n")

(testis
 (do (assign-fn foo () (fn () 123))
     (foo))
 123)

(mac achtung (x) `(+ ,x 2))
(let achtung [+ _ 5]
  (testis (achtung 0) 5))

(testis
  (do (def a () 123)
      (a))
  123)

(testis (car nil) nil)
(testis (car '(1 2 3)) 1)
(testis (cdr nil) nil)
(testis (cdr '(1 2 3)) '(2 3))
(testis (caar '((1 2))) 1)
(testis (cadr '(1 2 3)) 2)
(testis (cddr '(1 2 3)) '(3))

(testis (acons 3) nil)
(testis (acons '(3)) t)

(testis (atom 3) t)
(testis (atom '(3)) nil)
 
(testis (idfn 123) 123)

(testis (map1 acons '(1 (2) 3 (4))) '(nil t nil t))

(testis (pair '(1 2 3 4 5)) '((1 2) (3 4) (5)))
(testis (pair '(1 2 3 4 5) cons) '((1 . 2) (3 . 4) (5)))

(testis (and) t)
(testis (and 3) 3)
(testis (and 3 4) 4)
(testis (and nil 4) nil)
(testis (and 3 4 5) 5)

(testis (alref '((a 1) (b 2)) 'b) 2)

(testis (with (a 1 b 2) (list a b)) '(1 2))

(testis (let a 1 a) 1)

(testis (withs (a 1 b (+ a 2)) (list a b)) '(1 3))

(testis ((rfn foo (x) (if (no x) 0 (+ 1 (foo (cdr x))))) '(a b c)) 3)

(testis ((afn (x) (if (no x) 0 (+ 1 (self (cdr x))))) '(a b c)) 3)

(testis ((compose + car) '(3 4)) 3)

(testis ((complement acons) 3) t)

(testis (rev '(1 2 3 4 5)) '(5 4 3 2 1))

(testis (isnt 5 5) nil)
(testis (isnt 4 6) t)

(testis (w/uniq (a b)) nil)

(testis (or) nil)
(testis (or 3) 3)
(testis (or 3 4) 3)
(testis (or nil 4) 4)
(testis (or nil nil 5) 5)

(testis (alist nil) t)
(testis (alist '(1 2 3)) t)
(testis (alist 3) nil)

(testis (in 'c 'a 'b 'c) t)
(testis (in 'x 'a 'b 'c) nil)

(testis (iso '(1 2 3) (list 1 2 3)) t)
(testis (iso 'x 5) nil)

(testis (when 'yes 1 2 3 (+ 4 5)) 9)

(testis (unless 'yes 1 2 3 (+ 4 5)) nil)

(testis (do (assign x 5)
            (while (> x 0)
              (assign x (- x 1)))
            x)
        0)

(testis (empty '()) t)
(testis (empty '(a b)) nil)
(testis (empty "") t)
(testis (empty "ab") nil)
(testis (empty (table)) t)
(testis (empty (let h (table)
                 (sref h 4 'x)
                 h))
        nil)

(testis (some 'x '(a b c)) nil)
(testis (some 'x '(a x c)) t)

(testis (some atom '((a) (b) (c))) nil)
(testis (some atom '((a) b (c))) t)

(testis (some #\x "abc") nil)
(testis (some #\x "abx") t)

(testis (some (fn (_) (> (coerce _ 'int) 99)) "abc") nil)
(testis (some (fn (_) (> (coerce _ 'int) 99)) "abcd") t)

(testis (all 'x '(a x x)) nil)
(testis (all 'x '(x x x)) t)

(defvar N1WcGPq9wG (fn () 3))
(testis N1WcGPq9wG 3)

(testis (sym "foo") 'foo)

(testis (int "123") 123)

(ac-defvar 'j4K3II9Jh6 (list (fn () 'foo)))
(testis j4K3II9Jh6 'foo)

(ac-defvar 'EFsEQlU8S1 (list nil (fn (x) (assign U5liRzluxt (+ x 1)))))
(assign EFsEQlU8S1 5)
(testis U5liRzluxt 6)

(ac-defvar 'Y1jAJlOrAr (list nil))
(testis
 (catcherr (assign Y1jAJlOrAr 5))
 (makeerr "Y1jAJlOrAr is not assignable"))

(testis (do (assign NJyVDPGgYb (parameter 33))
            (parameterize NJyVDPGgYb 77 (NJyVDPGgYb)))
        77)

(make-dynamic fJFyT3D6q9 (parameter 36))
(testis fJFyT3D6q9 36)

(make-dynamic NwhlXqTbBJ (parameter 36))
(testis (type (paramfor NwhlXqTbBJ)) 'parameter)

(make-dynamic asbHOchCJG (parameter 36))
(testis (dlet asbHOchCJG 77 asbHOchCJG) 77)

(dynamic Y2J8kJ2gV6 33)
(testis (dlet Y2J8kJ2gV6 77 Y2J8kJ2gV6) 77)

(dynamic GuLWTizlbc 33)
(make-w/ GuLWTizlbc)
(testis (w/GuLWTizlbc 77 GuLWTizlbc) 77)

(make-implicit niW5mJq4IO (parameter 3))
(testis niW5mJq4IO 3)
(testis (w/niW5mJq4IO 4 niW5mJq4IO) 4)

(implicit imapnBKGmp 3)
(testis imapnBKGmp 3)
(assign imapnBKGmp 4)
(testis imapnBKGmp 4)
(testis (w/imapnBKGmp 5 imapnBKGmp) 5)

(testis (do1 3 4) 3)

(testis (tostring (pr 1 2 3)) "123")

(testis (tostring (prn "hi")) "hi\n")

(testis (aif 3 it) 3)
(testis (aif nil 3 nil 4 5 it) 5)

(def w1Sf3jg6ii (x)
  0)
(defrule w1Sf3jg6ii (is x 4)
  (* 2 x))
(testis (w1Sf3jg6ii 3) 0)
(testis (w1Sf3jg6ii 4) 8)

(def NdGo83w92b (a (o b 3))
  b)
(testis (NdGo83w92b 1) 3)
(defrule NdGo83w92b (is a 5)
  (* b 2))
(testis (NdGo83w92b 1) 3)
(testis (NdGo83w92b 5) 6)

(testis (ac-complex-args nil 'ra) nil)
(testis (ac-complex-args 'a  'ra) '((a ra)))

(testis ((eval (ac-complex-fn '(a (o b 3)) '((+ a b)))) 5) 8)

(testis (ac-complex-args? '(a b c)) nil)
(testis (ac-complex-args? '(a b . rest)) nil)
(testis (ac-complex-args? '(a (o b))) t)

(testis ((fn (a (o b 3)) (+ a b)) 5) 8)
(testis ((fn ((o a 3) . rest) (list a rest))) '(3 nil))
(testis ((fn ((o a 3) . rest) (list a rest)) 1) '(1 nil))
(testis ((fn ((o a 3) . rest) (list a rest)) 1 2) '(1 (2)))
(testis ((fn (a (o b a)) b) 3) 3)
(testis ((fn ((a b c)) (+ a (* b c))) (list 1 2 3)) 7)

(testis (newstring 5) "\u0000\u0000\u0000\u0000\u0000")
(testis (newstring 5 #\a) "aaaaa")

(testis (tostring (disp '(a "b" 3))) "(a b 3)")
(testis (tostring (disp '(a . b))) "(a . b)")
(testis (tostring (disp '(a ((b (c)))))) "(a ((b (c))))")
(testis (tostring (write '(a "b" 3))) "(a \"b\" 3)")

(testis (max) nil)
(testis (max 1 3 6 3 7 2) 7)

(testis (map [+ _ 1] '(1 2 3 4)) '(2 3 4 5))
(testis (map (fn (_) #\x) "hello") "xxxxx")

(testis (string 1 nil #\b "c" '(d e)) "1bcde")

(testis (ac-ssyntax 'abc) nil)
(testis (ac-ssyntax 'a:b) t)

(testis (ac-symbol->chars 'abc) '(#\a #\b #\c))

(testis (ac-tokens [is _ #\:] '(#\a #\: #\b #\c) nil nil nil)
        '((#\a) (#\b #\c)))

(testis (point foo (foo 5) 6) 5)

(testis (catch 1 2 (throw 3) 4 5) 3)

(testis (let a 1
          (catch (after (throw nil)
                        (assign a 2)))
          a)
        2)

(testis (case 2 1 7 2 8 3 9) 8)

(testis (tostring (system "echo hello")) "hello\n")

(assign td "/tmp/SR0hwhic5P")

(def clean ()
  (system (string "rm -rf " td))
  (system (string "mkdir " td)))

(let f (string td "/foo")
  (clean)
  (w/outfile s f (disp "hi" s))
  (testis (tostring (system (string "cat " f))) "hi"))

(testis (let s (outstring) (disp "foo" s) (inside s)) "foo")

(testis (w/outstring s (disp "foo" s) (inside s)) "foo")

(let f (string td "/foo")
  (clean)
  (w/outfile s f (disp "hello" s))
  (w/appendfile s f (disp " there" s))
  (testis (tostring (system (string "cat " f))) "hello there"))

(let f (string td "/foo")
  (clean)
  (w/outfile s f
    (w/stdout s (disp "xyzzy")))
  (testis (tostring (system (string "cat " f))) "xyzzy"))

(testis (readstring1 "123") 123)

(testis (readstring1 "[+ 3 _]") '(square-bracket + 3 _))

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

;; don't know how to test atomic adequately

(testis (atomic-invoke (fn () 123))
        123)
(testis (atomic-invoke (fn ()
                         (atomic-invoke (fn () 123))))
        123)

(testis (firstn 3 '(1 2 3 4 5)) '(1 2 3))

(testis (nthcdr 4 '(a b c d e)) '(e))

(testis (tuples '(1 2 3 4 5 6 7) 3) '((1 2 3) (4 5 6) (7)))

(let x '(1 2 3)
  (= (car x) 'one)
  (testis x '(one 2 3))

  (= (cadr x) 'two)
  (testis x '(one two 3))

  (= (cdr x) 'blam)
  (testis x '(one . blam)))

(testis (accum a (down x 5 0 (a x))) '(5 4 3 2 1 0))

(testis (accum a (repeat 5 (a 7))) '(7 7 7 7 7))

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

(testis (adjoin 1 '(2 3 4) (fn (a b) (and (odd a) (odd b)))) '(2 3 4))
(testis (adjoin 1 '(2 4 6) (fn (a b) (and (odd a) (odd b)))) '(1 2 4 6))

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

(do (clean)
    (system (string "echo abc >" (string td "/foo")))
    (mvfile (string td "/foo") (string td "/bar"))
    (testis (filechars (string td "/bar")) "abc\n"))

(do (writefile '(a b "cd" 5 6) "/tmp/foo")
    (testis (filechars "/tmp/foo") "(a b \"cd\" 5 6)"))

(testis (rand 1) 0)

(testis (tostring (dlet rand (fn (n) 1)
                    (rand-choice (pr "a") (pr "b") (pr "c"))))
        "b")

(testis (n-of 5 7) '(7 7 7 7 7))

(testis (aracket-false (racket (racket-> 1 2))) t)
(testis (aracket-false (racket (racket-< 1 2))) nil)

(fromstring "λ"
  (testis (readb) 206)
  (testis (readb) 187)
  (testis (readb) nil))

(testis (tostring (writeb 65) (writeb 66) (writeb 67))
        "ABC")

(testis (dlet infile (fn (name)
                       ;; a not very random /dev/urandom :-)
                       (instring "\u0000\u0001\u0002"))
          (rand-string 3))
        "012")

(testis (accum a (a 1) (a 2) (a 3))
        '(1 2 3))

(testis (accum a (forlen i '(a b c) (a i))) '(0 1 2))

(testis (accum a (on x '(a b c) (a (list index x))))
        '((0 a) (1 b) (2 c)))

(testis (most - '(5 9 2 7 3)) 2)

(testis (insert-sorted < 4 '(1 5 6)) '(1 4 5 6))

(testis (let x '(a b (1 5 6) c d)
          (insort < 4 (x 2))
            x)
        '(a b (1 4 5 6) c d))

(testis (let x nil
          (each i '(6 4 9 2 5 1 2) (insort < i x))
          x)
        '(1 2 2 4 5 6 9))

; todo test for reinsert-sorted, insortnew

; todo test for memo, defmemo

(testis (<=) t)
(testis (<= 3) t)
(testis (<= 3 4) t)
(testis (<= 4 4) t)
(testis (<= 5 4) nil)
(testis (<= 1 2 3) t)

(testis (>=) t)
(testis (>= 3) t)
(testis (>= 3 4) nil)
(testis (>= 4 4) t)
(testis (>= 5 4) t)

(testis (alphadig #\7) t)
(testis (alphadig #\:) nil)

(testis
 (tostring (xloop (x 0)
             (pr x)
             (if (< x 10)
                  (next (+ x 1)))))
 "012345678910")

(fromstring ""
  (testis (readline) nil)
  (testis (readc) nil))
(fromstring "\n"
  (testis (readline) "")
  (testis (readc) nil))
(fromstring "\r\n"
  (testis (readline) "")
  (testis (readc) nil))
(fromstring "one"
  (testis (readline) "one")
  (testis (readc) nil))
(fromstring "one\n"
  (testis (readline) "one")
  (testis (readc) nil))
(fromstring "one\r\n"
  (testis (readline) "one")
  (testis (readc) nil))
(fromstring "one\ntwo"
  (testis (readline) "one")
  (testis (readc) #\t))
(fromstring "one\r\ntwo"
  (testis (readline) "one")
  (testis (readc) #\t))

(testis (whitec #\a) nil)
(testis (whitec #\space) t)
(testis (letter #\a) t)
(testis (letter #\0) nil)
(testis (digit #\a) nil)
(testis (digit #\0) t)
(testis (punc #\;) t)

(testis (summing p (p nil) (p t) (p t) (p nil)) 2)

(testis (sum [* _ 2] '(1 2 3)) 12)

(testis (treewise cons [if _ (+ _ 1)] '(1 2 (3 (4 5) 6 7) 8)) '(2 3 (4 (5 6) 7 8) 9))

; todo test for prall, prs

(testis (tree-subst 3 'X '(1 (2 3 (4 5) 6))) '(1 (2 X (4 5) 6)))

(testis (accum a
          (ontree a '(1 . (2 . 3))))
        '((1 . (2 . 3)) 1 (2 . 3) 2 3))

(testis (dotted 'a) nil)
(testis (dotted '(a b)) nil)
(testis (dotted '(a . b)) t)

(testis (accum a (each (k v) (fill-table (table) '(a 1)) (a (list k v))))
        '((a 1)))

(testis (with (h (listtab '((a 1) (b 2) (c 3) (d 4) (e 5)))
               ks nil)
          (each k (keys h) (insort < k ks))
          ks)
        '(a b c d e))

;; (testis (tostring (write (listtab '((a 1))))) "#table((a 1))")

;; (testis (tablist (car (readall "#table((a 1))"))) '((a 1)))

;; (testis (sort < (keys '#table((a 1) (b 2) (c 3) (d 4))))
;;         '(a b c d))

(def assoc-key-sort (xs)
  (sort (fn (a b) (< (car a) (car b))) xs))

(testis (assoc-key-sort '((b 2) (d 4) (a 1) (c 3)))
        '((a 1) (b 2) (c 3) (d 4)))

(do (writefile (obj a 1 b 2) "/tmp/foo")
    (testis (assoc-key-sort (erp (tablist (w/infile s "/tmp/foo" (read-table s)))))
            '((a 1) (b 2))))

(testis (erp (fromstring "((a 1) (b 2))" (read-table)))
        (obj a 1 b 2))

(do (writefile (obj a 1 b 2) "/tmp/foo")
    (testis (w/infile s "/tmp/foo" (read-table s))
            (obj a 1 b 2)))

(testis (do (writefile (obj a 1 b 2 c 3) "/tmp/foo")
            (load-table "/tmp/foo"))
        (obj a 1 b 2 c 3))

(testis (do (save-table (obj a 1 b 2 c 3) "/tmp/foo")
            (listtab (readfile1 "/tmp/foo")))
        (obj a 1 b 2 c 3))

(testis (listtab (read (tostring (write-table (obj a 1 b 2 c 3 d 4 e 5)))))
        (obj a 1 b 2 c 3 d 4 e 5))

(testis (copy 'abc) 'abc)
(testis (copy '(a b c d)) '(a b c d))
(testis (copy "hello") "hello")
(testis (copy (obj a 1 b 2 c 3 d 4 e 5)) (obj a 1 b 2 c 3 d 4 e 5))

(testis (copy "hello" 2 #\X 3 #\X) "heXXo")

(testis (abs -33) 33)

(testis (trunc 3) 3)
(testis (trunc 3.0) 3)
(testis (trunc 3.3) 3)
(testis (trunc 3.999) 3)

(testis (round 3) 3)
(testis (round 3.01) 3)
(testis (round 3.49) 3)
(testis (round 3.50) 4)
(testis (round 3.51) 4)
(testis (round 3.99) 4)

(testis (round 4.5) 4)
(testis (round -4.5) -4)

(testis (round -3.5) -4)

(testis (roundup 3.0) 3)
(testis (roundup 3.01) 3)
(testis (roundup 3.5) 4)
(testis (roundup 3.99) 4)
(testis (roundup 4.5) 5)
(testis (roundup -3.01) -3)
(testis (roundup -3.5) -4)
(testis (roundup -4.5) -5)

(testis (nearest 13 10) 10)
(testis (nearest 18 10) 20)

(testis (avg '(2 20 8)) 10)

(testis (med '(3 30000 5 30001 4 30002 2 1)) 4)

(testis (sort < '(7 9 13 42 193 -50)) '(-50 7 9 13 42 193))
(testis (sort < "cbfdaeg") "abcdefg")

(testis (bestn 3 < '(9 13 11 5 78 4 0)) '(0 4 5))

(testis (split '(a b c d e f g) 3) '((a b c) (d e f g)))

(def stream (xs)
  (fn ()
    (if xs
         (do1 (car xs)
              (= xs (cdr xs))))))

(let s (stream '(a b c))
  (testis (s) 'a)
  (testis (s) 'b)
  (testis (s) 'c)
  (testis (s) nil))

(w/msec (stream '(-205086062 -205086061))
  (testis (tostring (time (+ 8 90)))
          "time: 1 msec.\n"))

(testis (union is '(a b c d e) '(x y c z)) '(a b c d e x y z))

; todo test templates

(testis (number 123) t)
(testis (number 10.5) t)
(testis (number "foo") nil)

(w/seconds (fn () 1000500)
  (testis (since 1000000) 500))

(testis (accum a (let x nil (loop (assign x 0) (< x 5) (assign x (+ x 1))
                            (a x))))
      '(0 1 2 3 4))

(testis (accum a (for x 0 5 (a x))) '(0 1 2 3 4 5))

; todo tests for cache, defcache

(testis (errsafe (/ 1 0)) nil)

(testis (saferead "123") 123)
(testis (saferead "#abc") nil)

(do (racket-delete-file "/tmp/foo")
    (testis (safe-load-table "/tmp/foo") (obj))
    (writefile (obj a 1 b 2) "/tmp/foo")
    (testis (safe-load-table "/tmp/foo") (obj a 1 b 2)))

(do (system "rm -rf /tmp/bar /tmp/bar2")
    (system "mkdir /tmp/bar")
    (testis (dir-exists "/tmp/bar") "/tmp/bar")
    (testis (dir-exists "/tmp/bar2") nil)
    (system "rm -rf /tmp/bar"))

(do (system "rm -rf /tmp/foo")
    (ensure-dir "/tmp/foo/a/b/c")
    (testis (dir-exists "/tmp/foo/a/b/c") "/tmp/foo/a/b/c")
    (system "rm -rf /tmp/foo"))

(testis (timedate 1296961475) '(35 4 19 5 2 2011))

(testis (date 1296961475) '(2011 2 5))

(testis (datestring 1296961475) "2011-02-05")

(testis (count even '(1 2 3 4 5 6)) 3)
(testis (count 'a '(a b c a b c a a)) 4)

(testis (ellipsize "hello" 10) "hello")
(testis (ellipsize "hello there" 10) "hello ther...")

(dlet rand (fn (x) 2)
  (testis (rand-elt '(a b c d e)) 'c))

; todo test for until

(testis (before 'c 'e '(a b c d e f g)) t)
(testis (before even [> _ 10] '(3 7 2 11 4)) t)
(testis (before even [> _ 10] '(3 11 7 2 4)) nil)

(testis ((orf letter digit) #\6) t)
(testis ((orf letter digit) #\*) nil)

(testis ((andf even [> _ 0]) -4) nil)
(testis ((andf even [> _ 0]) 4) t)

(testis (atend 1 "abc") nil)
(testis (atend 2 "abc") t)
(testis (atend 3 "abc") t)

(testis (multiple 25 5) t)
(testis (multiple 25 25) t)
(testis (multiple 25 4) nil)

(testis (nor nil nil nil) t)
(testis (nor nil t nil nil) nil)

(testis (only.avg '(3 2 3 4)) 3)
(testis (only.avg '()) nil)

(testis (retrieve 3 even '(1 2 3 4 5 6 7)) '(2 4 6))

(testis (dedup '(a b c a c d e e f a)) '(a b c d e f))

(testis (single '()) nil)
(testis (single '(a)) t)
(testis (single '(a b)) nil)

(testis (intersperse 'x '(a b c d)) '(a x b x c x d))

(testis (counts '(a b a c c d c b))
        (obj a 2 b 2 c 3 d 1))

(testis (commonest '(a b a c c d c b)) '(c 3))

(testis (reduce + '(1 2 3 4 5 6)) 21)

(testis (len> "abc" 3) nil)
(testis (len> "abc" 2) t)

(let f (string td "/foo.arc")
  (clean)
  (writefile '(def MWiyhWKuZG (x) (* x 5)) f)
  (load f)
  (testis (MWiyhWKuZG 6) 30))

(testis (positive 'foo) nil)
(testis (positive -5) nil)
(testis (positive 0) nil)
(testis (positive 0.0) nil)
(testis (positive 55) t)
(testis (positive 55/3) t)
(testis (positive 48.6) t)

(testis (keys (w/table h (set h!x))) '(x))

(testis (w/outstring s (w/stderr s (ero "foo" 3)) (inside s)) "\"foo\" 3 \n")

;; todo tests for queue

(testis (median '(1 2 3 4 5 6)) 3)

;; todo tests for flushout, noisy-each

(testis (downcase "AbCDef..00") "abcdef..00")
(testis (downcase #\B) #\b)
(testis (downcase 'FOObar) 'foobar)

(testis (upcase "AbCDef..00") "ABCDEF..00")
(testis (upcase #\b) #\B)
(testis (upcase 'FOObar) 'FOOBAR)

(testis (inc 10) 11)
(testis (inc 10 5) 15)
(testis (inc "13") "14")

(testis (range 1 5) '(1 2 3 4 5))

(testis (mismatch "abcdef" "abcXef") 3)
(testis (mismatch "abcdef" "abcdef") nil)

(testis (memtable '(a b c d)) (obj a t b t c t d t))

(testis (tostring (w/bars (pr "ab") (pr "cd") (pr "ef"))) "ab | cd | ef")

;; todo tests for threads, trav

(let x (list 'a nil 'c)
  (or= (cadr x) 'b)
  (or= (cadr x) 'X)
  (testis x '(a b c)))

;; todo tests for hooks, out, get

;; todo tests for disk savers

;; todo test for evtil, rand-key, ratio

(testis (find digit "abc9def") #\9)
(testis (find positive '(-7 -8 6 3)) 6)
(testis (find positive '()) nil)
(testis (find positive '(-7 -8 -6 -3)) nil)

(testis (exact 3/5) nil)
(testis (exact 1/3) nil)
(testis (exact 5) t)
(testis (exact 5.0) nil)

(testis (expt 10 3) 1000)

(testis (sqrt 25) 5)
