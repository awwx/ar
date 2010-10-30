My goals for this Arc runtime project are:

* to fix bugs, and

* to make Arc more hackable, and

* to avoid changing the Arc language.

The later two go together because when there's some change we'd like
to make to Arc, we can make Arc more hackable instead, and then we can
get the "different" Arc that we want as a library, instead of having
to actually change Arc.

The primary bug I'm focused on is the queue bug
(http://awwx.ws/queue-test-summary), which I suspect is caused by
mutating Racket's immutable pairs.  Thus in this version of the
runtime, Arc's lists are implemented with Racket's mutable pairs
(mpair's).

To support making Arc more hackable, I'm working on reflecting the Arc compiler
into Arc.  Thus (hypothetically):

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

To support this reflection, this version of the Arc compiler both
takes as input an Arc list (the Arc expression to compile) and returns
an Arc list (an Arc list representation of the Racket expression the
Arc expression is compiled into).  Thus Arc, either in it's runtime or
in it's compiler, never sees Racket's () list terminator or immutable
pairs.
