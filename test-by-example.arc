(use strings capture equal-wrt-testing runtime)

;; todo goes somewhere else

(= racket-#f (ail-code "#f"))

(def regexp (pattern)
   (if (isa pattern 'string) (racket-pregexp pattern) pattern))

(def re-match (pattern (o in stdin))
  (let result (racket-regexp-try-match (regexp pattern) in)
    (if (is result racket-#f)
         nil
         (map racket-bytes->string/utf-8 (ar-r/list-toarc result)))))

; This isn't anchored at the beginning of the input unless you use
; "^" yourself.

(def re-looking-at (pattern (o in stdin))
  (ar-tnil:racket-regexp-match-peek (regexp pattern) in))

(def skip-newlines ((o in stdin))
  (when (re-match "^ *\r?\n" in)
    (skip-newlines in)))

(def parse-test-expr-block (indentation)
  (let expr (readline)

    (= expr (+ expr "\n"))

    ((afn ()
       (when (re-looking-at "^ ")
         (= expr (+ expr (cut (readline) indentation) "\n"))
         (self))))

    expr))

(def parse-expression (indentation)
  `((expr ,(parse-test-expr-block indentation))))

(def read-upto-empty-line ()
  (string:accum a
    ((afn ()
       (let line (readline)
         (when (and line (> len.line 0))
           (a (+ line "\n"))
           (self)))))))

(def parse-backslash-escapes (s)
  (multisubst '(("\\\\" "\\") ("\\n" "\n")) s))

(def parse-test-result ()
  (accum a
    (let lines (read-upto-empty-line)
      (w/stdin (instring lines)
        ((afn ()
           (when (peekc)
             (aif (re-match "^prints: ")
                   (a `(prints ,(parse-backslash-escapes:readline)))
                  (re-match "^err: ")
                   (a `(err ,(parse-backslash-escapes:readline)))
                  (re-match "^stderr: ")
                   (a `(errout ,(parse-backslash-escapes:readline)))
                   (a `(val ,(parse-test-expr-block 0))))
             (self))))))))

(def parse-one-test-spec ()
  (skip-newlines)
  (if (peekc)
       (let spaces (re-match "^>( +)")
         (unless spaces (err "expecting >"))
           (+ (parse-expression len.spaces)
              (parse-test-result)))))

(def parse-test-specs ()
  (drain (parse-one-test-spec)))

(def write-val (runtime result)
  (aif (assoc 'val result)
        (+ `((val ,(tostring (runtime!write cadr.it))))
           (rem [is (car _) 'val] result))
        result))

(def eval-test (runtime spec)
  (write-val runtime
    (capture-val-out-errout
     (fn ()
       (runtime!eval (runtime!read (alref spec 'expr)))))))

(def check-test-result (runtime expected actual)
  (catch
   (if (and (alref actual 'err) (no (alref expected 'err)))
        (throw (+ "error: " (alref actual 'err)))
        (each (key expected-value) (keep [in (car _) 'val 'err 'prints 'stderr] expected)
          (let actual-value-assoc (assoc key actual)
            (if (no actual-value-assoc)
                 (throw (+ "expected " key " " (tostring:write expected-value) ", "
                           "not present in actual result: "
                           (tostring:write actual)))
                 (let actual-value (cadr actual-value-assoc)
                   (if (isnt (trim expected-value 'end)
                             (trim actual-value   'end))
                        (throw (+ "expected " key " " (tostring:write expected-value) ", "
                                  "actual " (tostring:write actual)))))))))))

(def example-test (runtime spec-string)
  (let specs (fromstring spec-string (parse-test-specs))
    (each spec specs
      (let actual (eval-test runtime spec)
        (aif (check-test-result runtime spec actual)
              (do (pr "FAIL ")
                  (write spec)
                  (prn " " it)
                  (err "test failed"))
              (do (pr "ok ")
                  (write spec)
                  (prn)))))))
