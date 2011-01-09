My goals for this Arc runtime project are:

* to fix bugs in the runtime

* to make Arc more hackable

* and to do that while (mostly...) avoiding changing the Arc language.

The later two go together because when there's some change we'd like
to make to Arc, we can make Arc more hackable instead, and then we can
get the "different" Arc that we want as a library, instead of having
to actually change Arc.

I haven't completely managed to keep myself from making any changes to
the language (see removal of parenthesis from the std ports
below)... but since the runtime is designed to be very hackable, we
should at least be able to have a version that's more compatible with
Arc 3.1 for people who want that.

This code is under development, most of Arc is still unimplemented.

There's now a toy REPL.  It only reads one line and eval's that,
instead of reading as many lines as are needed to complete the input.

To run:

    mzscheme arc.ss

or, if you have rlwrap:

    rlwrap -q "\"" mzscheme arc.ss

Note that you don't use the "-f" option like you would with Arc 3.1.

Tests can be run by setting run-tests* to 'inline or 'atend in the
source, though it's slow because the Arc compiler and Arc are loaded
afresh for each test.

Todo
----

* ac-nameit, ac-dbname
* ((compose ...) ...)
* ((complement ...) ...)
* atstrings
* ac-binaries
* direct-calls
* ar-funcallN optimizations
* ac-macex
* explicit-flush
* printing and reading tables
* incremental parsing
* full REPL
* declare
* primitives
* optional namespace argument to eval
* Arc 3.1 calls ac-macex in ac-assignn... why?
* rename "ac-fn-foo"
* rename "this"


Changes
-------

This version of the Arc runtime:

* Implements Arc lists using Racket's mutable pairs (mpair's)

which I hope will fix the [queue bug](http://awwx.ws/queue-test-summary).


* implements quasiquotation with Alan Bawden's algorithm

which I hope will fix list splicing in nested quasiquotes which was
giving people trouble writing macro-defining macros.


* Function rest arguments are 'nil terminated Arc lists

(cdr ((fn args args) 1)) => nil


* join can accept a non-list as its last argument

(join '(1 2) 3) => (1 2 . 3)

which turns out to be useful in macros and other code which works with
dotted lists.  It means that any list can be split on any cdr, and
applying join to the pieces will result in the original list.


* Reflects the Arc compiler into Arc to make Arc more hackable

Thus (hypothetically):

    arc> (ac-literal? 123)
    t
    arc> (eval 123)
    123
    arc> +
    #<procedure:+>
    arc> (ac-literal? +)
    nil
    arc> (eval +)
    Error: "Bad object in expression #<procedure:+>"
    arc> (def (ac-literal? x) (in (type x) 'str 'number 'int 'char 'fn))
    *** redefining ac-literal?
    #<procedure: ac-literal?>
    arc> (ac-literal? +)
    t
    arc> (eval +)
    #<procedure:+>


* Arc reader implemented in Arc, again to help make Arc more hackable

though incremental reading isn't currently supported.


* global variables are stored in an Arc table instead of in a Racket namespace

as an experiment to see if the simpler data structure is sufficient.


* replaces (stdin), (stdout), (stderr) with stdin, stdout, stderr

removing an unnecessary layer of parentheses.


* uniq implemented using Racket's gensym

* defvar allows global variables to be hacked to supply your own
  implementation for getting or setting the variable

* implicit variables

which can help make programs more concise when the same variable
doesn't need to be threaded through many layers of function calls.


* readline accepts CR-LF line endings

which is useful for some Internet protocols.

* string literals can contain newlines

This was unintentional (I wrote the reader and later noticed that it
permitted unescaped newlines), but then I couldn't think of a reason
to forbid it.
