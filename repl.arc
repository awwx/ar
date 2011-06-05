(use arc)

(= repl-eof-value (list 'eof))

(def repl ()
  (on-err
   (fn (c)
     (prn "Error: " (details c))
     (repl))
   (fn ()
     (disp "arc> ")
     (let expr (read stdin repl-eof-value)
       (if (in expr ':a repl-eof-value)
            (prn)
            (do (readline)
                (let val (eval expr)
                  (write val)
                  (prn)
                  (= that val)
                  (= thatexpr expr)
                  (repl))))))))

(repl)
