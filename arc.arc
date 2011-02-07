(mac square-bracket body
  `(fn (_) (,@body)))

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

(mac inline (x)
  `',(eval x))

(def maptable (f table)
  ((inline (racket-fn 'hash-for-each)) table f)
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

(dynamic infile (racket-fn 'open-input-file))
(= (sig 'infile) '(name))

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

(defrule ascons (isa x 'input)
  (drain (readc x)))

(def readfile (name)
  (w/infile s name (primitive-readall (coerce (allchars s) 'cons))))

(def readfile1 (name)
  (w/infile s name (read1 s)))

; todo eof
(def readall (src)
  (primitive-readall (coerce (if (isa src 'string) src (allchars src)) 'cons)))

(def filechars (name)
  (w/infile s name (allchars s)))

(def mvfile (old new)
  (racket-code "(rename-file-or-directory old new #t)")
  nil)

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(implicit rand (racket-fn 'random))
(= (sig 'rand) '((o n)))

(mac rand-choice exprs
  `(case (rand ,(len exprs))
     ,@(let key -1 
         (mappend [list (++ key) _]
                  exprs))))

(= racket-false (racket-code "#f"))

(def aracket-false (x)
  (is x racket-false))

(def aracket-true (x)
  (no (aracket-false x)))

(def aracket-eof (x)
  (aracket-true (racket-code "(eof-object? x)")))

(def readb ((o str stdin))
  (let c ((inline (racket-fn 'read-byte)) str)
    (if (aracket-eof c) nil c)))

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

(defrule print (isa x 'table)
  (disp "#table" port)
  (print primitive (tablist x) port))

(defalt parse-value-here
  (mliteral "#table")
  (listtab (parse-value)))

(mac obj args
  `(listtab (list ,@(map (fn ((k v))
                           `(list ',k ,v))
                         (pair args)))))

; todo remove
(def dumbsort (xs (o f <))
  (let r nil
    (each x xs (insort f x r))
    r))

; todo do better
; todo doesn't work for tables with compound keys
(defrule iso (and (isa x 'table) (isa y 'table))
  (with (kx (keys x) ky (keys y))
    (and (is (len kx) (len ky))
         (iso (dumbsort kx) (dumbsort ky))
         (all [iso x._ y._] kx))))

; todo eof
(def read-table ((o i stdin))
  (let e (read1 i)
    (if (alist e) (listtab e) e)))

; todo eof
(def load-table (file)
  (w/infile i file (read-table i)))

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
  (racket-code "(inexact->exact (truncate x))"))

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

(implicit msec (racket-fn 'current-milliseconds))
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

(implicit seconds (racket-fn 'current-seconds))
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

(def saferead (arg) (errsafe:read1 arg))

(def safe-load-table (filename) 
  (or (errsafe:load-table filename)
      (table)))

(def dir-exists (path)
  (if (racket->tnil (racket-code "(directory-exists? path)")) path))
                    
(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

;; todo something less ugly
(def timedate ((o seconds (seconds)))
  (let d (racket-code "(seconds->date seconds)")
    (list (racket-code "(date-second d)")
          (racket-code "(date-minute d)")
          (racket-code "(date-hour d)")
          (racket-code "(date-day d)")
          (racket-code "(date-month d)")
          (racket-code "(date-year d)"))))

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
