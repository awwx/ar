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

* user break (i.e. typing ^C) doesn't seem to be enabled
* ac-nameit, ac-dbname
* atstrings
* ac-binaries
* direct-calls
* macex1
* explicit-flush
* declare
* primitives
* Arc 3.1 calls ac-macex in ac-assignn... I wonder why?

 
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


* join can accept a non-list as its last argument

         (join '(1 2) 3) => (1 2 . 3)

  which turns out to be useful in macros and other code which works
  with dotted lists.  It means that any list can be split on any cdr,
  and applying join to the pieces will result in the original list.



* global variables are stored in an Arc table instead of in a Racket namespace

  as an experiment to see if the simpler data structure is sufficient.


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
implementing Arc lists with Racket mpair's is in fact one way to
fix the bug.

Reflecting the Arc compiler into Arc was inspired by rntz's [Arc
compiler written in Arc](https://github.com/nex3/arc/tree/arcc).

rocketnia explained why my definition of inline was broken by quote
copying its value, and contributed the patch to make quote not do
that.

rocketnia provided the patch to make lexical variables take precedence
over macros with the same name; waterhouse contributed the test.
