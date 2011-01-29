(implicit pos*)
(implicit fail)

; failf must be called *outside* the w/fail scope,
; thus the code to pass the normal/fail result of calling f
; out through the w/fail

(def onmatch-impl (matcher successf failf)
  (with (return-value nil failed nil)
    (catch
      (w/fail (fn ()
                (assign failed t)
                (throw nil))
        (assign return-value (matcher))))
    (if failed
         (failf)
         (successf return-value))))

(mac onmatch (var matcher onsuccess onfail)
 `(onmatch-impl (fn () ,matcher)
                (fn (,var) ,onsuccess)
                (fn () ,onfail)))

(mac onfail (onfail . body)
  (w/uniq gv
    `(onmatch ,gv (do ,@body)
       ,gv
       ,onfail)))

(def at-impl (matcher)
  (let mark pos*
    (after (matcher)
           (assign pos* mark))))

(mac at body
  `(at-impl (fn () ,@body)))

(def try-impl (matcher successf failf)
  (let mark pos*
    (onmatch r (matcher)
      (successf r)
      (do (assign pos* mark)
          (failf)))))

(mac try (v matcher successf failf)
  `(try-impl (fn () ,matcher)
             (fn (,v) ,successf)
             (fn () ,failf)))

(def ascons (x)
  (if (or (no x) (acons x))
       x
      ; todo
      ;(isa x 'input)
      ; (drain (readc x))
      (isa x 'string)
       (coerce x 'cons)
       (err "don't know how to parse" x)))

(def fmatch (seq body)
  (onfail nil
    (w/pos* (ascons seq)
      (body))))

(mac match (s . body)
  `(fmatch ,s (fn () ,@body)))

(mac test-matchpos (input matcher expected)
  `(testis (ascons ,expected)
         (match ,input (do ,matcher) pos*)))

(mac test-match1 (str matcher expected)
  `(testf ,str (fn (_)
                 (match _ (onfail '<<parse-fail>> ,matcher)))
          ,matcher
          ,expected))

(mac test-match (matcher . body)
  `(do ,@(map1 (fn (a)
                 (with (input (car a) expected (cadr a))
                   `(test-match1 ,input ,matcher ,expected)))
               (pair body))))

; todo fail this?
(def at-end ()
  (no pos*))

; todo rename?
(def fail-at-end ()
  (if (at-end) (fail)))

(def next ()
  (fail-at-end)
  (do1 (car pos*)
       (assign pos* (cdr pos*))))

(def one (item)
  (ret v (next)
    (unless ((testify item) v) (fail))))

(test-match (one #\a)
  "abc" #\a
  "xyz" '<<parse-fail>>)

(test-match (one (fn (_) (is _ #\a)))
  "abc" '#\a
  "#bc" '<<parse-fail>>)

(test-match (one (fn (x) t))
  "" '<<parse-fail>>)

(test-marker
 (match "abcd"
   (test-iso (at (one (fn (_) (is _ #\a)))) #\a)
   (test-iso pos* '(#\a #\b #\c #\d))))

(test-match (do (next)
                (try x (n-of 2 (one letter))
                  'didnt-expect-success
                  pos*))
  "ab123" '(#\b #\1 #\2 #\3))

; todo: confusing name
(def onenot (item)
  (ret v (next)
    (unless ((complement (testify item)) v) (fail))))

; don't have [ _ ] yet
(def oneof items
  (ret v (next)
    (unless (some (fn (_) ((testify _) v)) items) (fail))))

(test-match (oneof #\a #\b #\c)
  "a56" #\a
  "c56" #\c
  "56"  '<<parse-fail>>)

(def falt2 (f1 f2)
  (let mark pos*
    (onfail
     (do (assign pos* mark)
         (f2))
     (f1))))

(mac alt2 (p1 p2)
  `(falt2 (fn () ,p1) (fn () ,p2)))

(mac alt ps
  (if (no ps)
       `(fail)
      (no (cdr ps))
       (car ps)
       `(alt2 ,(car ps) (alt ,@(cdr ps)))))

(test-match (alt) "greetings" '<<parse-fail>>)

(mac optional body
  `(alt (do ,@body) nil))

(def mliteral1 (pat val)
  (let pat (ascons pat)
    (if (begins pos* pat)
         (do (assign pos* (nthcdr (len pat) pos*))
             val)
         (fail))))

; todo don't like the name "mliteral"
(def mliteral args
  (if (no args)
       (fail)
      (no (cdr args))
       (mliteral1 (car args) (car args))
       (alt (mliteral1 (car args) (cadr args))
            (apply mliteral (cddr args)))))

(test-match (mliteral "xyz")
  "greetings" '<<parse-fail>>)
(test-match (mliteral "greet")
  "greetings" "greet")
(test-match (list (mliteral "greet")
                  (mliteral "ings"))
  "greetings" '("greet" "ings"))
(test-match (list (mliteral "greetx") (mliteral "ings"))
  "greetings" '<<parse-fail>>)

(test-match  (list (mliteral "greet") (mliteral "xings"))
  "greetings" '<<parse-fail>>)

(test-match (alt (mliteral "greet")) "greeting" "greet")

(test-match (alt (mliteral "greet")
                 (mliteral "foo"))
  "greetings" "greet")

(test-match (alt (mliteral "foo")
                 (mliteral "greet"))
 "greetings" "greet")

(test-match (alt (mliteral "foo")
                 (mliteral "bar")
                 (mliteral "greet"))
 "greetings" "greet")

(test-match (list (mliteral "greet")
                  (optional (mliteral "ings")))
 "greetings" '("greet" "ings"))

(test-match (list (mliteral "greet")
                  (optional (mliteral "xyz")))
 "greetings" '("greet" nil))

(def rfmany (matchf acc)
  (try v (matchf)
    (rfmany matchf (cons v acc))
    acc))

(def fmany (matchf)
  (rev (rfmany matchf nil)))

(mac many body
  `(fmany (fn () ,@body)))

(test-match (many (mliteral "x"))
 "xxxyz" '("x" "x" "x"))

(test-match (list (many (mliteral "x"))
                  (mliteral "y"))
  "xxxyz"   '(("x" "x" "x") "y")
  "yzzz"    '(nil "y")
  "xxxyzzz" '(("x" "x" "x") "y"))

(def manyis (v)
  (many (one v)))

(def manyisnt (v)
  (let test (complement (testify v))
    (many (one test))))

(mac many1 parser
  `(cons (do ,@parser) (many (do ,@parser))))

(test-match (list (many1 (mliteral "x"))
                  (mliteral "y"))
  "yzzz" '<<parse-fail>>)

;; todo don't like this name either
(def many1is (v)
  (let test (testify v)
    (many1 (one test))))

(mac must (errmsg . body)
  `(onfail (err ,errmsg)
     ,@body))

(test-match (must "want cookie!" (mliteral "cookie"))
  "xyzzy" (makeerr "want cookie!"))

(mac parse-intersperse (separator parser must-message)
  `(optional
    (cons ,parser
          (many ,separator
                (must ,must-message ,parser)))))

(test-match (parse-intersperse
             (one #\,)
             (one alphadig)
             "comma must be followed by alphadig")
  "a,b,c" '(#\a #\b #\c))

(def skipwhite ()
  (manyis whitec))

(test-match (do (skipwhite)
                (one #\a))
  "  abc" #\a)

(mac comma-separated (parser (o must-message "a comma must be followed by a value"))
  `(parse-intersperse
    (do (skipwhite) (one #\,))
    ,parser
    ,must-message))

(test-match (comma-separated
             (one #\a)
             "comma must be followed by 'a'")
  "a  ,b  ,c"
  (makeerr "comma must be followed by 'a'"))

(test-match (comma-separated
             (one alphadig)
             "comma must be followed by alphadig")
  "a  ,b  ,c" '(#\a #\b #\c))

(mac entire body
  `(do1 (do ,@body)
        (unless (at-end) (fail))))

(test-match (entire (mliteral "abc")) "abc" "abc")
(test-match (entire (mliteral "ab"))  "abc" '<<parse-fail>>)

(def fmatched-input (matcher)
  (let mark pos*
    (matcher)
    (accum a
      (xloop (p mark)
        (when (and p (isnt p pos*))
          (a (car p))
          (next (cdr p)))))))

(mac matched-input body
  `(fmatched-input (fn () ,@body)))

(test-match (matched-input (one #\a) (one #\b) (one #\c))
  "abcd" '(#\a #\b #\c))

(mac not body
  (w/uniq v
    `(try ,v (do ,@body)
       (fail)
       t)))

(test-match (do (mliteral "ab")
                (not (one #\!))
                (mliteral "c"))
  "abc" "c")

(test-match (do (mliteral "ab")
                (not (one #\!)))
  "ab"   t
  "ab!c" '<<parse-fail>>)

(def fupto (n parser)
  (if (< n 1)
       '()
      (try v (parser)
        (cons v (fupto (- n 1) parser))
        nil)))

(mac upto (n . body)
  `(fupto ,n (fn () ,@body)))

(test-match (upto 3 (one #\a))
  "b"     '()
  "ab"    '(#\a)
  "aaab"  '(#\a #\a #\a)
  "aaaab" '(#\a #\a #\a))

(def ffrom1upto (n parser)
  (or (upto n (parser))
      (fail)))

(mac from1upto (n . body)
  `(ffrom1upto ,n (fn () ,@body)))

(test-match (from1upto 3 (one #\a))
  ""      '<<parse-fail>>
  "b"     '<<parse-fail>>
  "ab"    '(#\a)
  "aaab"  '(#\a #\a #\a)
  "aaaab" '(#\a #\a #\a))

(mac defalt (name . body)
  (if (bound name)
       (w/uniq orig
         `(let ,orig ,name
            (assign ,name
              (fn ()
                (alt (do ,@body)
                     (,orig))))))
       `(def ,name () ,@body)))
