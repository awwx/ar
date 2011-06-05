(use arc)

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

(def parse-test-result ()
  (accum a
    (let lines (read-upto-empty-line)
      (w/stdin (instring lines)
        ((afn ()
           (when (peekc)
             (aif (re-match "^prints: ")
                   (a `(out ,(readline)))
                  (re-match "^error: ")
                   (a `(err ,(readline)))
                  (re-match "^stderr: ")
                   (a `(errout ,(readline)))
                   (a `(val ,(parse-test-expr-block 0))))
             (self))))))))

(def parse-one-test-spec ()
  (skip-newlines)
  (let spaces (re-match "^>( +)")
    (unless spaces (err "expecting >"))
    (+ (parse-expression len.spaces)
       (parse-test-result))))
