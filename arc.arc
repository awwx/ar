(mac square-bracket body
  `(fn (_) (,@body)))

(mac aif (expr . body)
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(def readline ((o s stdin))
  (aif (readc s)
    (coerce
     (accum a
       (xloop (c it)
         (if (is c #\return)
              (if (is (peekc s) #\newline)
                   (readc s))
             (is c #\newline)
              nil
              (do (a c)
                  (aif (readc s)
                        (next it))))))
     'string)))

(def toy-repl ()
  (disp "arc> ")
  (aif (readline)
        (do (on-err (fn (e) (prn "err: " (details e)))
              (fn ()
                (map1 (fn (r)
                        (write r)
                        (prn))
                      (read-eval it))))
            (toy-repl))
        (prn)))

(def string args
  (apply + "" (map1 [coerce _ 'string] args)))

(def ssyntax (x)
  (and (isa x 'sym)
       (no (in x '+ '++ '_))
       (some [in _ #\: #\~ #\& #\. #\!] (string x))))

(def ac-symbol->chars (x)
  (coerce (coerce x 'string) 'cons))

(def ac-tokens (testff source token acc keepsep?)
  (if (no source)
       (rev (if (acons token)
                 (cons (rev token) acc)
                 acc))
      (testff (car source))
       (ac-tokens testff
                  (cdr source)
                  '()
                  (let rec (if (no token)
                                acc
                                (cons (rev token) acc))
                    (if keepsep?
                         (cons (car source) rec)
                         rec))
                  keepsep?)
       (ac-tokens testff
                  (cdr source)
                  (cons (car source) token)
                  acc
                  keepsep?)))

(def ac-chars->value (x)
  (read1 (coerce x 'string)))

(def ac-expand-compose (sym)
  (let elts (map1 (fn (tok)
                    (if (is (car tok) #\~)
                         (if (no (cdr tok))
                             'no
                             `(complement ,(ac-chars->value (cdr tok))))
                         (ac-chars->value tok)))
                  (ac-tokens [is _ #\:] (ac-symbol->chars sym) nil nil nil))
    (if (no (cdr elts))
         (car elts)
         (cons 'compose elts))))

(def ac-expand-ssyntax (sym)
  (err "Unknown ssyntax" sym))

(mac defrule (name test . body)
  (let arglist (sig name)
    (w/uniq (orig args)
      `(let ,orig ,name
         (assign ,name
           (fn ,args
             (aif (apply (fn ,arglist ,test) ,args)
                   (apply (fn ,arglist ,@body) ,args)
                   (apply ,orig ,args))))))))

(defrule ac (ssyntax s)
  (ac (ac-expand-ssyntax s) env))

(def ac-insym? (char sym)
  (mem char (ac-symbol->chars sym)))

(defrule ac-expand-ssyntax (or (ac-insym? #\: sym) (ac-insym? #\~ sym))
  (ac-expand-compose sym))

(def ac-build-sexpr (toks orig)
  (if (no toks)
       'get
      (no (cdr toks))
       (ac-chars->value (car toks))
       (list (ac-build-sexpr (cddr toks) orig)
             (if (is (cadr toks) #\!)
                  (list 'quote (ac-chars->value (car toks)))
                  (if (in (car toks) #\. #\!)
                       (err "Bad ssyntax" orig)
                       (ac-chars->value (car toks)))))))

(def ac-expand-sexpr (sym)
  (ac-build-sexpr (rev (ac-tokens [in _ #\. #\!] (ac-symbol->chars sym) nil nil t))
                  sym))

(defrule ac-expand-ssyntax (or (ac-insym? #\. sym) (ac-insym? #\! sym))
  (ac-expand-sexpr sym))

(def cdar (x) (cdr:car x))

(def ac-andf (s env)
  (ac (let gs (map1 [uniq] (cdr s))
        `((fn ,gs
            (and ,@(map1 (fn (f) `(,f ,@gs))
                         (cdar s))))
          ,@(cdr s)))
      env))

(def xcar (x) (and (acons x) (car x)))

(defrule ac (is (xcar:xcar s) 'andf) (ac-andf s env))

(def ac-expand-and (sym)
  (let elts (map1 ac-chars->value
                  (ac-tokens [is _ #\&] (ac-symbol->chars sym) nil nil nil))
    (if (no (cdr elts))
         (car elts)
         (cons 'andf elts))))

(defrule ac (ssyntax (xcar s))
  (ac (cons (ac-expand-ssyntax (car s)) (cdr s)) env))

(defrule ac-expand-ssyntax (ac-insym? #\& sym) (ac-expand-and sym))

; (and:or 3) => ((compose and or) 3) => (and (or 3))

(def ac-decompose (fns args)
  (if (no fns)
       `((fn vals (car vals)) ,@args)
      (no (cdr fns))
       (cons (car fns) args)
       (list (car fns) (ac-decompose (cdr fns) args))))

(defrule ac (is (xcar:xcar s) 'compose)
  (ac (ac-decompose (cdar s) (cdr s)) env))

(def cadar (x) (car (cdar x)))

; (~and 3 nil) => ((complement and) 3 nil) => (no (and 3 nil))

(defrule ac (is (xcar:xcar s) 'complement)
  (ac (list 'no (cons (cadar s) (cdr s))) env))

(def racket-fn (name (o module 'scheme))
  ((racket-module module) name))

(mac each (var expr . body)
  (w/uniq (gseq gf gv)
    `(let ,gseq ,expr
       (if (alist ,gseq)
            ((rfn ,gf (,gv)
               (when (acons ,gv)
                 (let ,var (car ,gv) ,@body)
                 (,gf (cdr ,gv))))
             ,gseq)
           (isa ,gseq 'table)
            (maptable (fn ,var ,@body)
                      ,gseq)
            (for ,gv 0 (- (len ,gseq) 1)
              (let ,var (,gseq ,gv) ,@body))))))

(assign-fn newstring (k (o char)) (racket-fn 'make-string))

; no = yet
(def best (f seq)
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (assign wins elt)))
        wins)))
              
(def max args (best > args))
(def min args (best < args))

(def map (f . seqs)
  (if (some [isa _ 'string] seqs) 
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs)) 
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)  
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac inline (x)
  `',(eval x))

(assign-fn make-semaphore ((o init)) (racket-fn 'make-semaphore))

;; Might want to support the optional arguments to call-with-semaphore,
;; but not using them at the moment.

(def call-with-semaphore (sema func)
  ((inline (racket-fn 'call-with-semaphore))
   sema (fn () (func))))

(def make-thread-cell (v (o preserved))
  ((inline (racket-fn 'make-thread-cell))
   v
   (nil->racket-false preserved)))

(assign-fn thread-cell-ref (cell)   (racket-fn 'thread-cell-ref))
(assign-fn thread-cell-set (cell v) (racket-fn 'thread-cell-set!))

(def macex (e (o once))
  (if (acons e)
       (let m (ac-macro? (car e))
         (if m
              (let expansion (apply m (cdr e))
                (if (no once) (macex expansion) expansion))
              e))
       e))
                
(def scar (x val)
  (sref x val 0))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(assign ar-the-sema (make-semaphore 1))

(assign ar-sema-cell (make-thread-cell nil))

(def atomic-invoke (f)
  (if (thread-cell-ref ar-sema-cell)
       (f)
       (do (thread-cell-set ar-sema-cell t)
           (after
             (call-with-semaphore ar-the-sema f)
             (thread-cell-set ar-sema-cell nil)))))

(mac atomic body
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  `(atomic (let ,@args)))
  
(mac atwith args
  `(atomic (with ,@args)))

(mac atwiths args
  `(atomic (withs ,@args)))

(def mappend (f . args)
  (apply + nil (apply map f args)))

; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place

; A bit gross that it works based on the *name* in the car, but maybe
; wrong to worry.  Macros live in expression land.

; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

(assign setter (table))

(mac defset (name parms . body)
  (w/uniq gexpr
    `(sref setter 
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional 
; position.  Such bugs will be seen only when the code is executed, when 
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (if (ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr)
                     g
                     `(fn (,h) (assign ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
        (and (acons expr) (acons (car expr)) (is (caar expr) 'get))
         (setforms (list (cadr expr) (cadr (car expr))))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call"
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,(car argsyms))))))))))))

(def metafn (x)
  (or (ssyntax x)
      (and (acons x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
       ((afn (fs)
          (if (caris (car fs) 'compose)            ; nested compose
               (self (join (cdr (car fs)) (cdr fs)))
              (cdr fs)
               (list (car fs) (self (cdr fs)))
              (cons (car fs) args)))
        (cdr f))
      (is (car f) 'no)
       (err "Can't invert " (cons f args))
       (cons f args)))

(def expand= (place val)
  (if (and (isa place 'sym) (~ssyntax place))
      `(assign ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  (expand=list args))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (- ,min 1))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (- ,v 1))
         ,@body))))

; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
  (let end (if (no end)   (len seq)
               (< end 0)  (+ (len seq) end) 
                          end)
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))

(mac whilet (var test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))

(def last (xs)
  (if (cdr xs)
      (last (cdr xs))
      (car xs)))

(def rem (test seq)
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

; Seems like keep doesn't need to testify-- would be better to
; be able to use tables as fns.  But rem does need to, because
; often want to rem a table from a list.  So maybe the right answer
; is to make keep the more primitive, not rem.

(def keep (test seq) 
  (rem (complement (testify test)) seq))

;(def trues (f seq) 
;  (rem nil (map f seq)))

(def trues (f xs)
  (and xs
      (let fx (f (car xs))
        (if fx
            (cons fx (trues f (cdr xs)))
            (trues f (cdr xs))))))

(mac push (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g) 
              (,setter (cdr ,g)))))))

; todo fix need to rename "test" as "testff"

(def adjoin (x xs (o testff iso))
  (if (some [testff x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac togglemem (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (if (mem ,gx ,val)
                      (rem ,gx ,val)
                      (adjoin ,gx ,val ,@args)))))))

(mac ++ (place (o i 1))
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (++ x) equiv to (zap + x 1)

(mac zap (op place . args)
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs 
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

(def prt args
  (map1 [if _ (disp _)] args)
  (car args))

(mac wipe args
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
  `(do ,@(map (fn (a) `(= ,a t)) args)))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))

(mac awhen (expr . body)
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  (if (no args)
      't 
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  (w/uniq (gacc gdone gres)
    `(with (,gacc nil ,gdone nil)
       (while (no ,gdone)
         (let ,gres ,expr
           (if (is ,gres ,eof)
               (= ,gdone t)
               (push ,gres ,gacc))))
       (rev ,gacc))))

; For the common C idiom while x = snarfdata != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  (w/uniq gf
    `(withs (,var nil ,gf (testify ,endval))
       (while (no (,gf (= ,var ,expr)))
         ,@body))))

(def consif (x y) (if x (cons x y) y))

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)   
                nil
               (f (car seq)) 
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq) 
         start)
        (recstring [if (f (seq _)) _] seq start))))

(assign-fn mod (x) (racket-fn 'modulo))

(def even (n) (is (mod n 2) 0))

(def odd (n) (no (even n)))

(def system (cmd)
  ((inline (racket-fn 'system 'scheme/system)) cmd)
  nil)

(def close ports
  (map (fn (port)
         (case (type port)
           input  ((inline (racket-fn 'close-input-port)) port)
           output ((inline (racket-fn 'close-output-port)) port)
           socket ((inline (racket-fn 'tcp-close)) port)
                  (err "Can't close " port)))
       ports)
  ;; todo try-custodian
  nil)

(defrule ac (is (xcar s) 'racket-code)
  ((inline (racket-fn 'read))
   ((inline (racket-fn 'open-input-string)) (cadr s))))

(assign-fn infile (name) (racket-fn 'open-input-file))

(def outfile (filename (o append))
   (let flag (if append 'append 'truncate)
     (racket-code "(open-output-file filename #:mode 'text #:exists flag)")))

(let expander 
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))
  )

(mac w/appendfile (var name . body)
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

(mac fromstring (str . body)
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

;; todo no incremental reading yet...
; (def read ((o x (stdin))) ...)

(def readfile (name)
  (w/infile s name (readall (coerce (allchars s) 'cons))))

(def readfile1 (name)
  ;; bogus :)
  (car (readfile name)))
