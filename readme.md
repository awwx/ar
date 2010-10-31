To run:

    mzscheme ar.ss

Note that you don't use the "-f" option like you would with Arc 3.1.

All this work in progress does so far is run tests, so you won't get
any output unless a test fails.

Changes
-------

This version of the Arc runtime:

* Implements Arc lists using Racket's mutable pairs (mpair's)

which I hope will fix the queue bug (http://awwx.ws/queue-test-summary).


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


* implements quasiquotation with Alan Bawden's algorithm

which I hope will fix list splicing in nested quasiquotes which was giving people trouble writing macro-defining macros.


* replaces (stdin), (stdout), (stderr) with stdin, stdout, stderr

removing an unnecessary layer of parentheses.
