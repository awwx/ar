The goal of this runtime project is to make Arc (even more!) hackable
by reflecting the Arc compiler into Arc, which in turn also lets more
of the Arc compiler to be written in Arc itself.

    arc> (ac-literal? 123)
    t
    arc> (eval 123)
    123
    arc> +
    #<procedure:ar-+>
    arc> (ac-literal? +)
    nil
    arc> (eval +)
    err: Bad object in expression #<procedure:ar-+>
    arc> (defrule ac-literal? (isa x 'fn) t)
    #<procedure:g1444>
    arc> (ac-literal? +)
    t
    arc> (eval +)
    #<procedure:ar-+>

Along the way we take advantage of the more hackable Arc to fix some
bugs and make some enhancements in the runtime that turn out to be
easier to do with a compiler which isn't quite as tightly bound to
Scheme.

This code is under development, much of Arc is unimplemented.

Get to the REPL with:

    racket as.ss

or, if you have rlwrap:

    rlwrap -q \" racket as.ss

You can also use "mzscheme" instead of "racket".

Note that you don't use the "-f" option like you would with Arc 3.1.

Run tests with:

    racket ar-test.ss
    racket ac-test.ss
    racket arc-test.ss
    racket strings-test.ss

Bug reports are *greatly* appreciated!


Todo
----

* clean up messy code in io.arc
* the strategy for representing Racket lists in Arc (which we need to
  have ac return an Arc list representing Racket code) is a bit
  confused... a clearer way to distinguish nil and () would be better.
* would be nice if typing ^C returned to the REPL
* ac-nameit, ac-dbname
* atstrings
* ac-binaries
* direct-calls
* macex1
* explicit-flush
* declare
* primitives
  * current-process-milliseconds
  * current-gc-milliseconds
  * memory
  * sin
  * cos
  * tan
  * asin
  * acos
  * atan
  * log
* Arc 3.1 calls ac-macex in ac-assignn... I wonder why?
* need tests for
  * atomic
  * force-close on sockets (see comment on force-close in arc3.1/ac.scm)
  * threads
  * whilet
  * awhen
  * whiler
  * consif
  * check
  * reinsert-sorted and insortnew
  * memo and defmemo
  * prall, prs
  * templates
  * cache, defcache
  * until
  * queue
  * flushout
  * noisy-each
  * trav
  * hooks
  * out
  * get
  * evtil
  * rand-key
  * ratio
  * dead
  * socket-accept
  * setuid
  * dir
  * rmfile
  * client-ip

 
Changes
-------

These bug fixes and enhancements are demonstrations of things that
become easier to do when more of the Arc compiler is written in Arc.
Because of the flexibility of the compiler they're easily reversed or
changed.

* Arc lists are implemented using Racket's mutable pairs (mpair's)

  as a fix for the [queue bug](http://awwx.ws/queue-test-summary).


* quasiquotation is implemented with Alan Bawden's algorithm

  as a fix for list splicing in nested quasiquotes, which was giving
  people trouble writing macro-defining macros.


* Function rest arguments are 'nil terminated Arc lists

         (cdr ((fn args args) 1)) => nil


* lexical identifiers take precedence over macros

         arc> (mac achtung (x) `(+ ,x 2))
         #(tagged mac #<procedure>)
         arc> (let achtung [+ _ 5] (achtung 0))
         5

* quote passes its value unchanged through the compiler, instead of
  copying it

  This isn't noticeable when just using quote to quote literal values
  in the usual way like '(a b c); because the original value isn't
  accessible to the program we can't tell if it was copied or not.

  However the behavior of quote is visible when using macros, since
  they can insert arbitrary values inside the quote expression.

  Choosing not to copy the quoted value means we can define inline
  like this:

         (mac inline (x)
           `',(eval x))

  and we'll get the same value out of inline that we put in:

         arc> (= x '(a b c))
         (a b c)
         arc> (is x (inline x))
         t

  I'm not sure if I understand all the ramifications of this change;
  but that we can define inline so simply is at least suggestive that
  this may be the right axiomatic approach.


* function values are considered literals by the compiler

  This is another change which isn't visible unless you're using
  macros (there otherwise isn't a way to insert a function *value*
  into the source code the compiler compiles).

  In Arc 3.1, a function value can be included in a macro expansion,
  but it needs to be quoted:

         (mac evens (xs) `(',keep even ,xs))

         (def foo () (evens '(1 2 3 4 5 6 7 8)))

         (wipe keep)

         arc> (foo)
         (2 4 6 8)

  With this change, the function value no longer needs to be quoted:

         (mac evens (xs) `(,keep even ,xs))


* macro values can also be included in a macro expansion

         (mac bar () `(prn "hi, this is bar"))

         (mac foo () `(,bar))

         arc> (foo)
         hi, this is bar


* join can accept a non-list as its last argument

         (join '(1 2) 3) => (1 2 . 3)

  which turns out to be useful in macros and other code which works
  with dotted lists.  It means that any list can be split on any cdr,
  and applying join to the pieces will result in the original list.


* global variables can be stored in an Arc table instead of in a Racket namespace

  This has been turned off by default though, as it did turn out to be
  slower than using a Racket namespace for global variables as Arc 3.1
  does.


* global variables are represented in Racket's namespace with their plain name

  In Arc 3.1, global variable are stored in Racket's namespace with a
  "_" prefix, which can be seen e.g. in some error messages:

         arc> x
         Error: "reference to undefined identifier: _x"

  This implementation uses the plain variable name with no prefix:

         arc> x
         Error: reference to undefined identifier: x

  To avoid clashes with Racket identifiers which need to be in the
  namespace, Racket identifiers are prefixed with "racket-".


* implicit variables

  which can help make programs more concise when the same variable
  doesn't need to be threaded through many layers of function calls.


* implements stdin, stdout, stderr as implicit variables

  removing an unnecessary layer of parentheses.


* uniq implemented using Racket's gensym


* defvar allows global variables to be hacked to supply your own
  implementation for getting or setting the variable


* readline accepts CR-LF line endings

  which is useful for Internet protocols such as HTTP.


* [...] is implemented with a macro

  [a b c] is expanded by the reader into (square-bracket a b c).
  Meanwhile there's a square-bracket macro:

         (mac square-bracket body
           `(fn (_) (,@body)))

  this makes it easier to hack the square bracket syntax. 

* the REPL removes excess characters at the end of the input line

  In Arc 3.1:

         arc> (readline) ;Fee fi fo fum   
         " ;Fee fi fo fum"
         arc> 

  this is because Racket's reader reads up to the closing ")", leaving
  the rest of the input line in the input buffer, which is then read
  by readline.

  On the assumption that the REPL is being run from a terminal and
  thus there will always be a trailing newline (which sends the input
  line to the process), the ar REPL cleans out the input buffer up to
  and including the newline:

         arc> (readline) ;Fee fi fo fum
         hello
         "hello"
         arc> 


Contributors
------------

This project is derived from Paul Graham and Robert Morris's [Arc 3.1
release](http://arclanguage.org/item?id=10254); indeed, a goal is to
incorporate as much of the original code with the fewest changes as
possible.

Kartik Agaram discovered the queue bug (and provided a runnable
example!), which was the original motivation for implementing Arc
lists using Racket mpair's.

Waterhouse [investigated the queue
bug](http://arclanguage.org/item?id=13518), determining that it is a
garbage collection issue; this in turn gives us confidence that
implementing Arc lists with Racket mpair's is in fact one way to fix
the bug.  (Note that waterhouse also provided a [direct
fix](http://arclanguage.org/item?id=13616) for Arc 3.1, so you don't
need this runtime implementation just to get a fix for the queue bug).

Reflecting the Arc compiler into Arc was inspired by rntz's [Arc
compiler written in Arc](https://github.com/nex3/arc/tree/arcc).

rocketnia explained why my definition of inline was broken by quote
copying its value, and contributed the patch to make quote not do
that.

rocketnia provided the patch to make lexical variables take precedence
over macros with the same name; waterhouse contributed the test.
