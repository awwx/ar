Under development, all it does currently is run tests against what's
been implemented so far.  You won't get any output unless a test
fails.

To run:

    mzscheme ar.ss

Note that you don't use the "-f" option like you would with Arc 3.1.


Changes
-------

This version of the Arc runtime:

* Implements Arc lists using Racket's mutable pairs (mpair's)

which I hope will fix the [queue bug](http://awwx.ws/queue-test-summary).


* implements quasiquotation with Alan Bawden's algorithm

which I hope will fix list splicing in nested quasiquotes which was giving people trouble writing macro-defining macros.


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


* global variables are stored in an Arc table instead of in a Racket namespace

as an experiment to see if the simpler data structure is sufficient.


* replaces (stdin), (stdout), (stderr) with stdin, stdout, stderr

removing an unnecessary layer of parentheses.


* uniq implemented using Racket's gensym

* defvar allows global variables to be hacked to supply your own
  implementation for getting or setting the variable

* implicit variables can help make programs more concise when the same
  variable doesn't need to be threaded through many layers of function
  calls
