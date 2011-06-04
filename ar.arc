(ail-code

 (racket-define -         racket--)
 (racket-define /         racket-/)
 (racket-define *         racket-*)
 (racket-define cons      racket-mcons)
 (racket-define inside    racket-get-output-string)
 (racket-define instring  racket-open-input-string)
 (racket-define nil       (racket-quote nil))
 (racket-define outstring racket-open-output-string)
 (racket-define t         (racket-quote t))
 (racket-define uniq      racket-gensym)

 (racket-define (ar-toarc x)
   (racket-cond
    ((racket-pair? x)
     (racket-mcons (ar-toarc (racket-car x))
                   (ar-toarc (racket-cdr x))))
    ((racket-null? x)
     nil)
    ((racket-string? x)
     (racket-string-copy x))
    (racket-else x)))

 (racket-define sig (racket-make-hash))
 (racket-hash-set! sig (racket-quote -)         (ar-toarc (racket-quote args)))
 (racket-hash-set! sig (racket-quote /)         (ar-toarc (racket-quote args)))
 (racket-hash-set! sig (racket-quote *)         (ar-toarc (racket-quote args)))
 (racket-hash-set! sig (racket-quote cons)      (ar-toarc (racket-quote (a b))))
 (racket-hash-set! sig (racket-quote inside)    (ar-toarc (racket-quote (s))))
 (racket-hash-set! sig (racket-quote instring)  (ar-toarc (racket-quote (str))))
 (racket-hash-set! sig (racket-quote outstring) (ar-toarc (racket-quote ())))
 (racket-hash-set! sig (racket-quote uniq)      (ar-toarc (racket-quote ())))
 (racket-hash-set! sig (racket-quote ar-toarc)  (ar-toarc (racket-quote (x))))

 (racket-define-syntax-rule (ar-def name signature . body)
   (racket-begin
     (racket-hash-set!
      sig
      (racket-quote name)
      (ar-toarc (racket-quote signature)))
     (racket-define (name . signature)
       . body)))

 (ar-def ar-r/list-toarc (x)
   (racket-cond
    ((racket-pair? x)
     (racket-mcons (racket-car x) (ar-r/list-toarc (racket-cdr x))))
    ((racket-null? x)
     (racket-quote nil))
    (racket-else x)))

 (ar-def list args
   (ar-r/list-toarc args))

 (ar-def ar-list-fromarc (x)
   (racket-cond
    ((racket-mpair? x)
     (racket-cons (racket-mcar x) (ar-list-fromarc (racket-mcdr x))))
    ((racket-eq? x (racket-quote nil))
     (racket-quote ()))
    (racket-else x)))

 (racket-define-struct ar-tunnel-struct (v))

 (racket-define ar-tunnel make-ar-tunnel-struct)

 (ar-def ar-deep-fromarc (x)
   (racket-cond
    ((ar-tunnel-struct? x)
     (ar-tunnel-struct-v x))

    ;; nil in the car position isn't a list terminator, and so can
    ;; be left alone.
    ((racket-mpair? x)
     (racket-cons (racket-let ((a (racket-mcar x)))
                    (racket-if (racket-eq? a nil)
                                 nil
                                 (ar-deep-fromarc a)))
                  (racket-let ((b (racket-mcdr x)))
                    (racket-if (racket-eq? b nil)
                                (racket-quote ())
                                (ar-deep-fromarc b)))))
    (racket-else
     x)))

 (racket-define err racket-error)
 (racket-hash-set! sig (racket-quote err) (racket-quote args))

 (ar-def car (x)
   (racket-if (racket-eq? x (racket-quote nil))
               (racket-quote nil)
               (racket-mcar x)))

 (ar-def cdr (x)
   (racket-if (racket-eq? x (racket-quote nil))
               (racket-quote nil)
               (racket-mcdr x)))

 (ar-def cadr (x)
   (car (cdr x)))

 (ar-def cddr (x)
   (cdr (cdr x)))

 (ar-def ar-apply-non-fn (x args)
   (racket-cond
    ((racket-mpair? x)
     (racket-mlist-ref x (car args)))
    ((racket-string? x)
     (racket-string-ref x (car args)))
    ((racket-hash? x)
     (racket-hash-ref x
       (car args)
       (racket-let ((default
                     (racket-if (racket-mpair? (cdr args))
                                 (car (cdr args))
                                 nil)))
         (racket-lambda () default))))
    (racket-else
     (err "Function call on inappropriate object" x args))))

 (ar-def ar-apply (fn . racket-arg-list)
   (racket-if (racket-procedure? fn)
               (racket-apply fn racket-arg-list)
               (ar-apply-non-fn fn (ar-r/list-toarc racket-arg-list))))

 (ar-def ar-funcall0 (fn)
   (racket-if (racket-procedure? fn)
               (fn)
               (ar-apply fn)))

 (ar-def ar-funcall1 (fn arg1)
   (racket-if (racket-procedure? fn)
                (fn arg1)
                (ar-apply fn arg1)))

 (ar-def ar-funcall2 (fn arg1 arg2)
   (racket-if (racket-procedure? fn)
               (fn arg1 arg2)
               (ar-apply fn arg1 arg2)))

 (ar-def ar-funcall3 (fn arg1 arg2 arg3)
   (racket-if (racket-procedure? fn)
               (fn arg1 arg2 arg3)
               (ar-apply fn arg1 arg2 arg3)))

 (ar-def ar-funcall4 (fn arg1 arg2 arg3 arg4)
   (racket-if (racket-procedure? fn)
               (fn arg1 arg2 arg3 arg4)
               (ar-apply fn arg1 arg2 arg3 arg4)))

 (ar-def ar-combine-args (as)
   (racket-let next ((as as) (accum (racket-list)))
     (racket-cond
      ((racket-null? as)
       accum)
      ((racket-null? (racket-cdr as))
       (racket-append accum (ar-list-fromarc (racket-car as))))
      (racket-else
       (next (racket-cdr as)
             (racket-append accum (racket-list (racket-car as))))))))

 (ar-def apply (fn . args)
   (racket-apply ar-apply fn (ar-combine-args args)))

 (ar-def ar-exint (x)
   (racket-and (racket-integer? x) (racket-exact? x)))

 (ar-def ar-tagged (x)
   (racket-and (racket-vector? x)
               (racket-eq? (racket-vector-ref x 0) (racket-quote tagged))))

 (ar-def rep (x)
   (racket-if (ar-tagged x)
               (racket-vector-ref x 2)
               x))

 (ar-def type (x)
   (racket-cond
    ((ar-tagged x)             (racket-vector-ref x 1))
    ((racket-mpair? x)         (racket-quote cons))
    ((racket-symbol? x)        (racket-quote sym))
    ((racket-parameter? x)     (racket-quote parameter))
    ((racket-procedure? x)     (racket-quote fn))
    ((racket-char? x)          (racket-quote char))
    ((racket-string? x)        (racket-quote string))
    ((ar-exint x)              (racket-quote int))
    ((racket-number? x)        (racket-quote num))
    ((racket-hash? x)          (racket-quote table))
    ((racket-output-port? x)   (racket-quote output))
    ((racket-input-port? x)    (racket-quote input))
    ((racket-exn? x)           (racket-quote exception))
    ((racket-thread? x)        (racket-quote thread))
    ((racket-thread-cell? x)   (racket-quote thread-cell))
    ((racket-semaphore? x)     (racket-quote semaphore))
    (racket-else               (racket-quote unknown))))

 (ar-def annotate (totype rep)
   (racket-cond
    ((racket-eqv? (type rep) totype) rep)
    (racket-else (racket-vector (racket-quote tagged) totype rep))))

 (ar-def ar-tnil (x)
   (racket-if x (racket-quote t) (racket-quote nil)))

 (ar-def ar-no (x)
   (racket-eq? x nil))

 (ar-def ar-true (x)
   (racket-not (ar-no x)))

 (ar-def map1 (f xs)
   (racket-if (ar-no xs)
               nil
               (cons (ar-funcall1 f (car xs)) (map1 f (cdr xs)))))

 (ar-def ar-iround (x)
   (racket-inexact->exact (racket-round x)))

 (ar-def coerce (x totype . args)
   (racket-cond
     ((ar-tagged x)
      (err "Can't coerce annotated object"))
     ((racket-eqv? totype (type x))
      x)
     ((racket-char? x)
      (racket-case totype
        ((int)       (racket-char->integer x))
        ((string)    (racket-string x))
        ((sym)       (racket-string->symbol (racket-string x)))
        (racket-else (err "Can't coerce" x type))))
     ((ar-exint x)
      (racket-case totype
        ((num)       x)
        ((char)      (racket-integer->char x))
        ((string)    (racket-apply racket-number->string x args))
        (racket-else (err "Can't coerce" x type))))
     ((racket-number? x)
      (racket-case totype
        ((int)       (ar-iround x))
        ((char)      (racket-integer->char (ar-iround x)))
        ((string)    (racket-apply racket-number->string x args))
        (racket-else (err "Can't coerce" x type))))
     ((racket-string? x)
      (racket-case totype
        ((sym)       (racket-string->symbol x))
        ((cons)      (ar-r/list-toarc (racket-string->list x)))
        ((num)       (racket-or (racket-apply racket-string->number x args)
                                (err "Can't coerce" x totype)))
        ((int)       (racket-let ((n (racket-apply racket-string->number x args)))
                       (racket-if n
                                   (ar-iround n)
                                   (err "Can't coerce" x totype))))
        (racket-else (err "Can't coerce" x totype))))
     ((racket-mpair? x)
      (racket-case totype
        ((string)    (racket-apply racket-string-append
                                   (ar-list-fromarc
                                    (map1 (racket-lambda (y)
                                            (coerce y (racket-quote string)))
                                          x))))
        (racket-else (err "Can't coerce" x totype))))
     ((racket-eq? x (racket-quote nil))
      (racket-case totype
        ((string)    "")
        ((cons)      (racket-quote nil))
        (racket-else (err "Can't coerce" x type))))
     ((racket-symbol? x)
      (racket-case totype
        ((string)    (racket-symbol->string x))
        (racket-else (err "Can't coerce" x type))))
     (racket-else x)))

 (ar-def ar-pairwise (pred lst)
   (racket-cond
     ((racket-null? lst) (racket-quote t))
     ((racket-null? (racket-cdr lst)) (racket-quote t))
     ((racket-not (racket-eqv? (pred (racket-car lst) (racket-cadr lst))
                               (racket-quote nil)))
      (ar-pairwise pred (racket-cdr lst)))
     (racket-else (racket-quote nil))))

 (ar-def is2 (a b)
   (ar-tnil
    (racket-or (racket-eqv? a b)
               (racket-and (racket-string? a)
                           (racket-string? b)
                           (racket-string=? a b)))))

 (ar-def is args
   (ar-pairwise is2 (ar-list-fromarc args)))

 (ar-def caris (x val)
   (ar-tnil
    (racket-and (racket-mpair? x)
                (ar-true (is (car x) val)))))

 (ar-def <2 (x y)
   (ar-tnil
    (racket-cond
     ((racket-and (racket-number? x) (racket-number? y)) (racket-< x y))
     ((racket-and (racket-string? x) (racket-string? y)) (racket-string<? x y))
     ((racket-and (racket-symbol? x) (racket-symbol? y))
      (racket-string<? (racket-symbol->string x)
                       (racket-symbol->string y)))
     ((racket-and (racket-char? x) (racket-char? y)) (racket-char<? x y))
     (racket-else (err "Can't <" x y)))))

 (ar-def < args
   (ar-pairwise <2 (ar-list-fromarc args)))

 (ar-def >2 (x y)
   (ar-tnil
    (racket-cond
     ((racket-and (racket-number? x) (racket-number? y)) (racket-> x y))
     ((racket-and (racket-string? x) (racket-string? y)) (racket-string>? x y))
     ((racket-and (racket-symbol? x) (racket-symbol? y))
      (racket-string>? (racket-symbol->string x) (racket-symbol->string y)))
     ((racket-and (racket-char? x) (racket-char? y)) (racket-char>? x y))
     (racket-else (err "Can't >" x y)))))

 (ar-def > args
   (ar-pairwise >2 (ar-list-fromarc args)))

 (ar-def list-len (x)
   (racket-cond
    ((ar-no x)         0)
    ((racket-mpair? x) (racket-+ 1 (list-len (racket-mcdr x))))
    (racket-else       (err "len expects a proper list"))))

 (ar-def len (x)
   (racket-cond
    ((racket-string? x) (racket-string-length x))
    ((racket-hash? x)   (racket-hash-count x))
    (racket-else        (list-len x))))

 (ar-def join args
   (ar-r/list-toarc
    (racket-apply racket-append
                  (racket-map ar-list-fromarc (ar-list-fromarc args)))))

 (ar-def ar-alist (x)
   (racket-or (ar-no x) (racket-mpair? x)))

 (ar-def + args
   (racket-cond
    ((racket-null? args)
     0)
    ((racket-or (racket-char? (racket-car args))
                (racket-string? (racket-car args)))
     (racket-apply racket-string-append
                   (racket-map (racket-lambda (a)
                                 (coerce a (racket-quote string)))
                               args)))
    ((ar-alist (racket-car args))
     (racket-apply join args))
    (racket-else
     (racket-apply racket-+ args))))

 (racket-hash-set! sig
                   (racket-quote peekc)
                   (ar-toarc (racket-quote ((o port stdin)))))

 (racket-define (peekc (port (racket-current-input-port)))
   (racket-let ((c (racket-peek-char port)))
     (racket-if (racket-eof-object? c) (racket-quote nil) c)))

 (racket-hash-set! sig
                   (racket-quote readc)
                   (ar-toarc (racket-quote ((o port stdin) (o eof nil)))))

 (racket-define (readc (port (racket-current-input-port))
                       (eof (racket-quote nil)))
   (racket-let ((c (racket-read-char port)))
     (racket-if (racket-eof-object? c) eof c)))

 (racket-hash-set! sig
                   (racket-quote writec)
                   (ar-toarc (racket-quote (c (o port stdout)))))

 (racket-define (writec c (port (racket-current-output-port)))
   (racket-write-char c port))

 (ar-def ar-rread-from-string (str)
   (racket-parameterize ((racket-current-readtable #f))
     (racket-read (racket-open-input-string str))))

 (ar-def ac-ail-code (forms)
   (ar-tunnel
    (racket-cons
     (racket-quote racket-begin)
     (racket-map (racket-lambda (form)
                   (racket-if (racket-string? form)
                               (ar-rread-from-string form)
                               (ar-deep-fromarc form)))
                 (ar-list-fromarc forms)))))

 (ar-def ac-use (items)
   (cons (racket-quote racket-begin)
         (map1 (racket-lambda (item)
                 (cons (racket-quote use-load)
                       (list (list (racket-quote racket-quote)
                                   item))))
               items)))

 (ar-def ac (s env)
   (racket-cond
    ((ar-true (caris s (racket-quote ail-code)))
     (ac-ail-code (cdr s)))
    ((ar-true (caris s (racket-quote use)))
     (ac-use (cdr s)))
    (racket-else
     (err "Bad object in expression" s))))

 (racket-define (eval form (runtime (racket-quote nil)))
   (ar-racket-eval
    (racket-if (ar-true runtime) runtime runtime*)
    (ar-deep-fromarc (ac form (racket-quote nil)))))

 (racket-define arc-readtable* #f)

 (racket-hash-set! sig
                   (racket-quote ar-read)
                   (ar-toarc (racket-quote (input (o eof nil)))))

 (racket-define (ar-read input (eof (racket-quote nil)))
   (racket-let ((v (racket-parameterize
                    ((racket-current-readtable arc-readtable*))
                    (racket-read input))))
     (racket-if (racket-eof-object? v)
                 eof
                 (ar-toarc v))))

 (ar-def loadin (in)
   (racket-let ((x (ar-read in)))
     (racket-if (ar-no x)
                 nil
                 (racket-begin
                  (eval x)
                  (loadin in)))))

 (ar-def load (filename)
   (racket-call-with-input-file filename
     (racket-lambda (p) (loadin p))))

)
