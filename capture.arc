(use arc)

(def capture-out (f)
  (let out (tostring (f))
    (if (> len.out 0)
         `((out ,out)))))

(def capture-errout (f)
  (let errout (w/outstring s
                (w/stderr s (f))
                (inside s))
    (if (> len.errout 0)
         `((errout ,errout)))))

(def capture-val (f)
  (on-err (fn (e) `((err ,(details e))))
          (fn ()  `((val   ,(f))))))

(def capture-val-out (f)
  (with (val nil out nil)
    (= out (capture-out
            (fn ()
              (= val (capture-val f)))))
    (+ val out)))

(def capture-val-out-errout (f)
  (with (val-out nil errout nil)
    (= errout (capture-errout
               (fn ()
                 (= val-out (capture-val-out f)))))
    (+ val-out errout)))
