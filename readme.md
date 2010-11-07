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
