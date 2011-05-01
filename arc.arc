(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))

(assign safeset
  (annotate 'mac
    (fn (var val)
      `(do (if (bound ',var)
               (do (racket-disp "*** redefining " (racket-stderr))
                   (racket-disp ',var (racket-stderr))
                   (racket-disp #\newline (racket-stderr))))
           (assign ,var ,val)))))

(assign assign-fn
  (annotate 'mac
    (fn (name signature func)
      `(do (sref sig ',signature ',name)
           (safeset ,name ,func)))))

(assign def
  (annotate 'mac
    (fn (name parms . body)
      `(assign-fn ,name ,parms (fn ,parms ,@body)))))

(def caar (xs) (car (car xs)))
(def cadr (xs) (car (cdr xs)))
(def cddr (xs) (cdr (cdr xs)))

(def no (x) (is x nil))

(def acons (x) (is (type x) 'cons))

(def atom (x) (no (acons x)))

(def idfn (x) x)

(def isa (x y) (is (type x) y))

(assign-fn pair (xs (o f list))
  (fn args
    ((fn (xs f)
       (if (no xs)
            nil
           (no (cdr xs))
            (list (list (car xs)))
            (cons (f (car xs) (cadr xs))
                  (pair (cddr xs) f))))
     (car args)
     (if (cdr args) (cadr args) list))))

(assign mac (annotate 'mac
              (fn (name parms . body)
                `(do (sref sig ',parms ',name)
                     (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

(mac square-bracket body
  `(fn (_) (,@body)))

(mac and args
  (if args
      (if (cdr args)
          `(if ,(car args) (and ,@(cdr args)))
          (car args))
      t))

(def assoc (key al)
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key) (cadr (assoc key al)))

(mac with (parms . body)
 `((fn ,(map1 car (pair parms))
    ,@body)
   ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
  (if (no parms) 
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms) 
         (withs ,(cddr parms) ,@body))))

(mac rfn (name parms . body)
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
 `(let self nil
    (assign self (fn ,parms ,@body))))

(mac compose args
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

(mac complement (f)
  (let g (uniq)
    `(fn ,g (no (apply ,f ,g)))))

(def rev (xs) 
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def isnt (x y) (no (is x y)))

(mac w/uniq (names . body)
  (if (acons names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                             names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac or args
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))

(def alist (x) (or (no x) (is (type x) 'cons)))

(mac in (x . choices)
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))

(def iso (x y)
  (or (is x y)
      (and (acons x) 
           (acons y) 
           (iso (car x) (car y)) 
           (iso (cdr x) (cdr y)))))

(mac when (test . body)
  `(if ,test (do ,@body)))

(mac unless (test . body)
  `(if (no ,test) (do ,@body)))

(mac while (test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

(def empty (seq) 
  (or (no seq) 
      (and (or (is (type seq) 'string) (is (type seq) 'table))
           (is (len seq) 0))))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def caddr (x) (car (cddr x)))

(assign-fn recstring (test s (o start 0))
  (fn args
    (with (test (car args)
           s    (cadr args)
           start (if (cddr args) (caddr args) 0))
      ((afn (i)
         (and (< i (len s))
              (or (test i)
                  (self (+ i 1)))))
       start))))

(def testify (x)
  (if (isa x 'fn) x [is _ x]))

(def some (test seq)
  (let f (testify test)
    (if (alist seq)
         (reclist   (compose f car) seq)
         (recstring (compose f seq) seq))))

(def all (test seq) 
  ((complement some) (complement (testify test)) seq))

(def mem (test seq)
  (let f (testify test)
    (reclist [if (f (car _)) _] seq)))

(mac aif (expr . body)
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(mac defrule (name test . body)
  (let arglist (sig name)
    (w/uniq (orig args)
      `(let ,orig ,name
         (assign ,name
           (fn ,args
             (aif (apply (fn ,arglist ,test) ,args)
                   (apply (fn ,arglist ,@body) ,args)
                   (apply ,orig ,args))))))))

(defrule ac (caris s 'racket)
  (let x (cadr s)
    (if (isa x 'string)
         (racket-read-from-string x)
         x)))

(assign ac-defined-vars* (table))

(def ac-defvar (v x)
  (sref ac-defined-vars* x v)
  nil)

(def ac-defined-var (v)
  (ac-defined-vars* v))

(defrule ac-global (ac-defined-var v)
  `(,(car it)))

(def ac-not-assignable (v)
  (fn (x)
    (err (string v " is not assignable"))))

(defrule ac-global-assign (ac-defined-var a)
  `(,(or (cadr it) (ac-not-assignable a)) ,b))

(mac defvar args
  (with (name (car args)
         get  (cadr args)
         set  (caddr args))
  `(ac-defvar ',name (list ,get ,set))))
(sref sig '(name get (o set)) 'defvar)

(def sym (x) (coerce x 'sym))

(assign-fn int (x (o b 10))
  (fn args
    (with (x (car args)
           b (if (cdr args) (cadr args) 10))
      (coerce x 'int b))))

(mac parameterize (param val . body)
  `(racket-parameterize ,param ,val (fn () ,@body)))

(assign dynamic-parameter* (table))

(mac make-dynamic (name param)
  (w/uniq paramval
    `(let ,paramval ,param
       (sref dynamic-parameter* ,paramval ',name)
       (defvar ,name (fn () (,paramval)) (fn (val) (,paramval val))))))

(mac paramfor (name)
  `(dynamic-parameter* ',name))

(mac dlet (name val . body)
  `(racket-parameterize (paramfor ,name) ,val (fn () ,@body)))

(mac dynamic args
  (with (name (car args)
         init (cadr args))
    `(make-dynamic ,name (parameter ,init))))

(mac make-w/ (name)
  (let w/name (sym (+ "w/" name))
    `(mac ,w/name (val . body)
       `(dlet ,',name ,val ,@body))))

(mac make-implicit (name param)
  `(do (make-dynamic ,name ,param)
       (make-w/ ,name)))

(mac implicit args
  (with (name (car args)
         init (cadr args))
    `(make-implicit ,name (parameter ,init))))
(sref sig '(name (o init)) 'implicit)

(make-implicit stdin  racket-stdin)
(make-implicit stdout racket-stdout)
(make-implicit stderr racket-stderr)

(def print (primitive x port)
  (primitive x port))

(def disp args
  (with (x (car args)
         port (or (cadr args) stdout))
    (print racket-disp x port)))
(sref sig '(x (o port)) 'disp)

(def write args
  (with (x (car args)
         port (or (cadr args) stdout))
    (print racket-write x port)))
(sref sig '(x (o port)) 'write)

(def pr args
  (map1 disp args)
  (car args))

(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

(mac w/outstring (var . body)
  `(let ,var (outstring) ,@body))

(mac tostring body
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))

(def ac-complex-args? (args)
  (if (no args)
       nil
      (isa args 'sym)
       nil
      (and (acons args) (isa (car args) 'sym))
       (ac-complex-args? (cdr args))
       t))

(def ac-complex-getargs (a) (map1 car a))

(def ac-complex-opt (var expr ra)
  (list (list var `(if (acons ,ra) (car ,ra) ,expr))))

(def ac-complex-args (args ra)
  (if (no args)
       nil
      (isa args 'sym)
       (list (list args ra))
      (acons args)
       (withs (a (car args)
               r (cdr args)
               x (if (and (acons a) (is (car a) 'o))
                      (ac-complex-opt (cadr a) (car (cddr a)) ra)
                      (ac-complex-args a (list 'car ra))))
         (join x (ac-complex-args (cdr args) (list 'cdr ra))))
       (err "Can't understand fn arg list" args)))

(def ac-complex-fn (args body)
  (let ra (uniq)
    `(fn ,ra
       (withs ,(apply join (ac-complex-args args ra))
         ,@body))))

(mac erp (x)
  (w/uniq (gx)
    `(let ,gx ,x
       (w/stdout stderr
         (write ',x)
         (disp ": ")
         (write ,gx)
         (disp #\newline))
       ,gx)))

(defrule ac-fn (ac-complex-args? args)
  (ac (ac-complex-fn args body) env))

(def printwith-list (primitive x port)
  (if (no (cdr x))
       (do (print primitive (car x) port)
           (disp ")" port))
      (acons (cdr x))
       (do (print primitive (car x) port)
           (disp " " port)
           (printwith-list primitive (cdr x) port))
       (do (print primitive (car x) port)
           (disp " . " port)
           (print primitive (cdr x) port)
           (disp ")" port))))

(defrule print (isa x 'cons)
  (disp "(" port)
  (printwith-list primitive x port))

(assign-fn newstring (k (o char)) racket-make-string)

(def maptable (f table)
  (racket-hash-for-each table f)
  table)

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

(def string args
  (apply + "" (map [coerce _ 'string] args)))

(def ac-ssyntax (x)
  (and (isa x 'sym)
       (no (in x '+ '++ '_))
       (some [in _ #\: #\~ #\& #\. #\!] (string x))))

(def ac-symbol->chars (x)
  (coerce (coerce x 'string) 'cons))

(def ac-tokens (test source token acc keepsep?)
  (if (no source)
       (rev (if (acons token)
                 (cons (rev token) acc)
                 acc))
      (test (car source))
       (ac-tokens test
                  (cdr source)
                  '()
                  (let rec (if (no token)
                                acc
                                (cons (rev token) acc))
                    (if keepsep?
                         (cons (car source) rec)
                         rec))
                  keepsep?)
       (ac-tokens test
                  (cdr source)
                  (cons (car source) token)
                  acc
                  keepsep?)))

(def racket-true (x)
  (racket (racket-if x (racket-quote t) (racket-quote nil))))

(def sread (p eof)
  (let v (racket-read p)
    (if (racket-true (racket-eof-object? v))
         eof
         (ar-toarc v))))

(assign-fn ccc (k) racket-call-with-current-continuation)

(mac point (name . body)
  (w/uniq (g p)
    `(ccc (fn (,g)
            (let ,name (fn ((o ,p)) (,g ,p))
              ,@body)))))

(mac catch body
  `(point throw ,@body))

(def protect (during after)
  (racket (racket-dynamic-wind (racket-lambda () #t) during after)))

(mac after (x . ys)
  `(protect (fn () ,x) (fn () ,@ys)))

(mac inline (x)
  `',(eval x))

(def system (cmd)
  ((inline ((racket-module 'scheme/system) 'system)) cmd)
  nil)

(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (no (cdr args)) 
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))

;; todo try-custodian redefined in io.arc

(def try-custodian (port))

(def close ports
  (each port ports
    (case (type port)
      input  (racket-close-input-port port)
      output (racket-close-output-port port)
      socket (racket-tcp-close port)
             (err "Can't close " port))
    (try-custodian port)))

(dynamic infile racket-open-input-file)
(sref sig '(name) 'infile)

(def outfile (filename (o append))
  (let flag (if append 'append 'truncate)
    (racket (racket-open-output-file filename #:mode (racket-quote text) #:exists flag))))

(def open-socket (port)
  ((inline ((racket-module 'scheme/tcp) 'tcp-listen)) port 50 (racket "#t")))

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

(def readstring1 (s (o eof nil)) (w/instring i s (read i eof)))

(def read ((o x stdin) (o eof nil))
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

(def ac-chars->value (x)
  (read (coerce x 'string)))

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

(defrule ac (ac-ssyntax s)
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

(assign cdar cdr:car)

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

(defrule ac (ac-ssyntax (xcar s))
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

(def scdr (x val)
  ((racket racket-set-mcdr!) x val))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(def make-semaphore ((o init))
  (racket-make-semaphore init))

(def call-with-semaphore (sema func)
  ((racket call-with-semaphore) sema (fn () (func))))

(def nil->racket-false (x)
  (if (no x) (racket "#f") x))

(def make-thread-cell (v (o preserved))
  (racket-make-thread-cell v (nil->racket-false preserved)))

(def thread-cell-ref (cell)
  (racket-thread-cell-ref cell))

(def thread-cell-set (cell v)
  ((racket racket-thread-cell-set!) cell v))

(assign ar-the-sema (make-semaphore 1))

(assign ar-sema-cell (make-thread-cell nil))

(def atomic-invoke (f)
  (if (thread-cell-ref ar-sema-cell)
       (f)
       (do (thread-cell-set ar-sema-cell t)
           (after
             (racket-call-with-semaphore ar-the-sema f)
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

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

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

(def ssexpand (x)
  (if (isa x 'sym) (ac-expand-ssyntax x) x))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional 
; position.  Such bugs will be seen only when the code is executed, when 
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (if (ac-ssyntax expr)
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
  (or (ac-ssyntax x)
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
  (if (and (isa place 'sym) (~ac-ssyntax place))
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

(mac loop (start test update . body)
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm) 
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (+ ,v 1))
         ,@body))))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (- ,min 1))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (- ,v 1))
         ,@body))))

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))

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

(def adjoin (x xs (o test iso))
  (if (some [test x _] xs)
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

(def mod (n m)
  (racket-modulo n m))

(def even (n) (is (mod n 2) 0))

(def odd (n) (no (even n)))

(mac fromstring (str . body)
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name) (w/infile s name (read s)))

(def readall (src (o eof nil))
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(def mvfile (old new)
  (racket-rename-file-or-directory old new (racket "#t"))
  nil)

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(dynamic rand racket-random)
(= (sig 'rand) '((o n)))

(mac rand-choice exprs
  `(case (rand ,(len exprs))
     ,@(let key -1 
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  (w/uniq ga
    `(let ,ga nil     
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

(def aracket-false (x)
  (is x (racket "#f")))

(def aracket-true (x)
  (no (aracket-false x)))

(def aracket-eof (x)
  (aracket-true (racket-eof-object? x)))

(def readb ((o str stdin))
  (let c (racket-read-byte str)
    (if (aracket-eof c) nil c)))

(def writeb (b (o str stdout))
  (racket-write-byte b str))

; rejects bytes >= 248 lest digits be overrepresented

(def rand-string (n)
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0)
      (w/infile str "/dev/urandom"
        (while (< i n)
          (let x (readb str)
             (unless (> x 247)
               (= (s i) (c (mod x nc)))
               (++ i)))))
      s)))

(mac forlen (var s . body)
  `(for ,var 0 (- (len ,s) 1) ,@body))

(mac on (var s . body)
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def most (f seq) 
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

(def insert-sorted (test elt seq)
  (if (no seq)
       (list elt) 
      (test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (no seq) 
       (list elt) 
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq)) 
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

(def memo (f)
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))

(mac defmemo (name parms . body)
  `(safeset ,name (memo (fn ,parms ,@body))))

(def <= args
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (no (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(mac summing (sumfn . body)
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def sum (f xs)
  (let n 0
    (each x xs (++ n (f x)))
    n))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree)) 
         (treewise f base (cdr tree)))))

(def prall (elts (o init "") (o sep ", "))
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))
             
(def prs args     
  (prall args "" #\space))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

(mac accum (accfn . body)
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

(def fill-table (table data)
  (each (k v) (pair data) (= (table k) v))
  table)

(def keys (h) 
  (accum a (each (k v) h (a k))))

(def vals (h) 
  (accum a (each (k v) h (a v))))

(def tablist (h)
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(mac obj args
  `(listtab (list ,@(map (fn ((k v))
                           `(list ',k ,v))
                         (pair args)))))

(mac xloop (withses . body)
  (let w (pair withses)
    `((rfn next ,(map1 car w) ,@body) ,@(map1 cadr w))))

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

(def read-table ((o i stdin) (o eof))
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-table (file (o eof))
  (w/infile i file (read-table i eof)))

(def save-table (h file)
  (writefile (tablist h) file))

(def write-table (h (o o stdout))
  (write (tablist h) o))

(def copy (x . args)
  (let x2 (case (type x)
            sym    x
            cons   (apply (fn args args) x)
            string (let new (newstring (len x))
                     (forlen i x
                       (= (new i) (x i)))
                     new)
            table  (let new (table)
                     (each (k v) x 
                       (= (new k) v))
                     new)
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def abs (n)
  (if (< n 0) (- n) n))

(def trunc (x)
  (racket-inexact->exact (racket-truncate x)))

(def round (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2)
        ((if (> n 0) + -) base 1)
        base)))

(def nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

(def med (ns (o test >))
  ((sort test ns) (round (/ (len ns) 2))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1) 

(def sort (test seq)
  (if (alist seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved 
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup nil
        (assign lup
                (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                  (if (less? (car y) (car x))
                    (do (if r-x? (scdr r y))
                        (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                    ; (car x) <= (car y)
                    (do (if (no r-x?) (scdr r x))
                        (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (list (cut seq 0 pos) (cut seq pos)))

(implicit msec racket-current-milliseconds)
(= (sig 'msec nil))

(mac time (expr)
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  `(time (repeat 10 ,expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(def carif (x) (if (atom x) x (car x)))

(= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name) 
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name) 
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  (let x (table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (with (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))       
       (w/infile in file (readall in))))

(def number (n) (in (type n) 'int 'num))

(implicit seconds racket-current-seconds)
(= (sig 'seconds nil))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

; could use a version for fns of 1 arg at least

(def cache (timef valf)
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts . body)
  `(safeset ,name (cache (fn () ,lasts)
                         (fn () ,@body))))

(mac errsafe (expr)
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def saferead (arg) (errsafe:read arg))

(def safe-load-table (filename) 
  (or (errsafe:load-table filename)
      (table)))

(def file-exists (path)
  (if (racket-true (racket-file-exists? path)) path))

(def dir-exists (path)
  (if (racket-true (racket-directory-exists? path)) path))
                    
(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

(def timedate ((o seconds (seconds)))
  (let d (racket-seconds->date seconds)
    (map [_ d] (list racket-date-second
                     racket-date-minute
                     racket-date-hour
                     racket-date-day
                     racket-date-month
                     racket-date-year))))

(def date ((o s (seconds)))
  (rev (nthcdr 3 (timedate s))))

(def datestring ((o s (seconds)))
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def rand-elt (seq) 
  (seq (rand (len seq))))

(mac until (test . body)
  `(while (no ,test) ,@body))

(def before (x y seq (o i 0))
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns
  (fn args
    ((afn (fs)
       (and fs (or (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def andf fns
  (fn args
    ((afn (fs)
       (if (no fs)       t
           (no (cdr fs)) (apply (car fs) args)
                         (and (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args `(no (or ,@args))) 

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

(def only (f) 
  (fn args (if (car args) (apply f args))))

(def retrieve (n f xs)
  (if (no n)                 (keep f xs)
      (or (<= n 0) (no xs))  nil
      (f (car xs))           (cons (car xs) (retrieve (- n 1) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (set (h x))))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (++ (c (car seq) 0))
          (counts (cdr seq) c))))

(def commonest (seq)
  (with (winner nil n 0)
    (each (k v) (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def reduce (f xs)
  (if (cddr xs)
      (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

(def rreduce (f xs)
  (if (cddr xs)
      (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(def load (file)
  (w/infile f file
    (w/uniq eof
      (whiler e (read f eof) eof
        (eval e)))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  `(let ,var (table) ,@body ,var))

(def ero args
  (w/stdout stderr
    (each a args
      (write a)
      (writec #\space))
    (writec #\newline))
  (car args))

(def queue () (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  ((sort > ns) (trunc (/ (len ns) 2))))

(def flushout ()
  (racket-flush-output)
  t)

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".") 
           (flushout)
           )
         ,@body)
       (prn)
       (flushout))))

(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (sym (map downc (coerce x 'string)))
             (err "Can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (sym (map upc (coerce x 'string)))
             (err "Can't upcase" x))))

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
  (if (> start end)
      nil
      (cons start (range (inc start) end))))

(def mismatch (s1 s2)
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(def memtable (ks)
  (let h (table)
    (each k ks (set (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (set ,needbars)
                                 (pr ,out))))))
                  body)))))

(def new-thread (f)
  (racket-thread (fn () (f))))

(def kill-thread (thd)
  (racket-kill-thread thd))

(def break-thread (thd)
  (racket-break-thread thd))

(def current-thread ()
  (racket-current-thread))

(def sleep ((o secs 0))
  (racket-sleep secs)
  nil)

(mac thread body 
  `(new-thread (fn () ,@body)))

(mac trav (x . fs)
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(mac or= (place expr)
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (or ,val (,setter ,expr)))))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(= (hooks* ',name) (fn ,@rest)))
  
(mac out (expr) `(pr ,(tostring (eval expr))))

(def get (index) [_ index])

(= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    `(unless (bound ',var)
       (do1 (= ,var (iflet ,gf (file-exists ,file)
                               (,load ,gf)
                               ,init))
            (= (savers* ',var) (fn (,gv) (,save ,gv ,file)))))))

(mac diskvar (var file)
  `(fromdisk ,var ,file nil readfile1 writefile))

(mac disktable (var file)
  `(fromdisk ,var ,file (table) load-table save-table))

(mac todisk (var (o expr var))
  `((savers* ',var) 
    ,(if (is var expr) var `(= ,var ,expr))))

(mac evtil (expr test)
  (w/uniq gv
    `(let ,gv ,expr
       (while (no (,test ,gv))
         (= ,gv ,expr))
       ,gv)))

(def rand-key (h)
  (if (empty h)
      nil
      (let n (rand (len h))
        (catch
          (each (k v) h
            (when (is (-- n) -1)
              (throw k)))))))

(def ratio (test xs)
  (if (empty xs)
      0
      (/ (count test xs) (len xs))))

(def quit ()
  (racket-exit))

(def find (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

; note that (exact 1/3) is false, which is confusing

(def exact (n)
  (isa n 'int))

(def expt (x y)
  (racket-expt x y))

(def sqrt (x)
  (racket-sqrt x))
