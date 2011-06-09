Join the conversation about the Arc Runtime Project ("ar") on Convore:
https://convore.com/arc-runtime-project/

Status
------

ar is now loading and running code from strings.arc.  Much of Arc
remains unimplemented (see the todo below for the complete list).

Bug reports are *greatly* appreciated!


Run
---

Run the REPL with:

    ./run repl

or, if you have rlwrap:

    rlwrap -q \" ./run repl

You can load Arc files from the command line and then go into the
REPL (files without an extension will default to ".arc"):

    ./run foo bar repl

(Note that foo.arc should either start with `(use arc)` to get Arc
loaded for it, or else you could start your arguments with "arc").

Or, if you just want to run your Arc program without entering the
REPL:

    ./run foo bar

You can pass command line arguments into your program by putting them
after "--":

    ./run mylibrary myprogram -- arg1 arg2

With `script`, you can write a shell script in Arc.  (Though still
todo is conveniently accessing the command line arguments).

For example, if the file "hello" contained:

    #!/path/to/ar/script
    (use arc)
    (prn "hello there")

you could run this script with:

    $ chmod +x hello
    $ ./hello

if you have ar on your path, you can also use env to avoid hard coding
the path to ar:

    #!/usr/bin/env script
    (use arc)
    (prn "hello there")

Run tests with:

    ./tests.pl


Goals
-----

* Make Arc (even more!) hackable, enabling people to create their
  own personal programming language -- beyond what can be done just
  with using macros.

* Provide a complete implementation of Arc 3.1, as one of the
  available languages based on ar.

* Be at least as good as Arc 3.1 at running a production website; thus
  for example you should be able to run a news.arc site on top of ar
  if you wanted to.

* Use the latest Racket version directly, instead of relying on the
  "mzscheme" backwards compatibility mode.

* Fix bugs and make enhancements in the runtime which are easier to do
  with a compiler which isn't quite as tightly bound to Scheme.

* And, ideally, to provide a clean and well-factored implementation of
  all of the above.


Non-Goals
---------

* It's not intended to be within the scope of the ar project itself to
  come up with some better language than Arc 3.1; though it *is* the
  job of ar to *support* the creation of languages better than Arc
  3.1.

    Thus questions such as "would it be better get the standard output
    port with `stdout` or `(stdout)`?" or "should the table constructor
    function be called `table` or `hash`?" aren't for ar to decide;
    though ar would ideally let you use `stdout` or `(stdout)`, or
    `table` or `hash`, as you wanted to.

    Of course, the line between the two is rather blurry (at what point
    does "supporting a better language" become simply "a better
    language"), and the current ar arguably already crosses the line
    with changes such as `stdout`... but to frame the discussion, a
    question to ask is not "should Arc do X instead of Y?", but instead
    to ask, "I want to do X.  How can we get ar to let me to do that
    easily?"

* It's also not a goal of ar to provide a comprehensive implementation
  of all the features included with ar.

    For example, ar has tests, and so includes a small testing library
    to run the tests.  But it isn't a goal of ar to provide a general
    purpose testing library; instead, there's just enough of a test
    framework to run ar's tests.

    As another example, some useful utilities such as `ret` and `cwd`
    are included in ar.  But it's not a goal of ar to provide useful
    utilities; the ones that are included are included because they're
    needed to load libraries.

    In summary, if it can be loaded as a library, then it doesn't need
    to be in ar itself.  (Since there are some things that we'll be
    able to move out of ar and into libraries when the loader is
    further along, ar may be able to get smaller over time).


Design Philosophy
-----------------

If an existing language or library does `(A, B and C)`, and someone
wants it to do `(A, B, and D)`, there are various approaches that we
could take:

1. We can say that the former is the most common case and should be
the default, and thus say that when you want `(A, B, D)` you need to
add code to prevent C from happening and to add code to get D to
happen.

2. We can add a way to configure the language or library so that you
can specify whether it should do `(A, B, C)` or `(A, B, D)`.

3. We can provide `(A, B)`, `(C)`, and `(D)` as separate composable
pieces, and leave it to the programmer to combine them as
`((A, B), (C))` or `((A, B), (D))`.

The guiding design philosophy of ar is to consistently choose the
third approach.

Of course, sometimes you may *want* to provide a more monolithic
facility that provides various guarantees... guarantees which perhaps
can't be made if a programmer can break things just by combining the
wrong pieces or by combining things in the wrong order.  Since ar is
supposed to support doing what you want, ideally you'd be able to
build such a facility on top or ar; even if ar isn't monolithic
itself.

And, just because ar has a design philosophy doesn't mean that it will
work in all cases.  Still, a proposed enhancement to ar implemented in
a way that goes against ar's design philosophy will typically need a
higher standard of proof that the approach is truly necessary than
making a similar change in a more conventional project would.


Making ar Less Terrible
-----------------------

ar is currently terrible in many ways: various things aren't
implemented; of the things that are implemented, much is written in an
awkward way; there are many bugs; and of the functionality which is
actually working, much doesn't have tests -- so the functionality
could be accidentally lost at any time.

When I work on ar myself, I pick the next thing which seems especially
terrible to me, and I work on that.  Thus ar (slowly) gets less
terrible over time.

Naturally you may have different priorities about which problems are
the most important for you.  Or, you may be impatient with the rate of
progress.

If you'd like to help make ar less terrible, you can also pick
something that seems especially important to you, fix or implement it,
and send a github pull request with your change.

Small commits are much easier to digest and incorporate than large
commits.  (Even if your commit is perfect, I still need to understand
it myself).  Thus if you have a larger change, it is better to submit
it as a sequence of small pull requests instead of one large pull
request.  That may seem less efficient, but it's faster in practice.

If you look at the [ar commit
history](https://github.com/awwx/ar/commits/master), you'll notice
that each commit is usually quite small: it makes just one isolated
change.  I work in small chunks because of the complexity of ar
itself: it's easier to see the ramifications of a small change.  An
ideal github pull request would be of *similar size*: this makes it
possible for me to quickly accept your commit, and the faster I can
accept your commits, the faster they can get into ar.


Todo
----

* split arc.arc into smaller pieces.

* allow special forms such as "fn" to be used as a lexical variable, and
  to be overridden by a macro.

* tidier implementations of dynamic and implicit.

(see https://github.com/awwx/ar/pull/1 for the above)

* When a runtime is passed to eval, it isn't running ac from the
  passed runtime.
* Date/time tests are failing on different computers... maybe a
  timezone problem?
* The dependency loader should track which files have been loaded by
  their full path.
* Optimizations (such as direct-calls)
* See if we can move coerce and + into Arc.
* (err "foo" '(1 2 3)) prints "Error: foo {1 2 3 . nil}"
* defrule is a fun hack but awkward to use for ssyntax: we have to put
  defrule's in a particular order to specify ssyntax precedence
* would be nice if typing ^C returned to the REPL
* pipe-from
* ac-nameit, ac-dbname
* atstrings
* ac-binaries
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


Changes
-------

* Arc lists are implemented using Racket's mutable pairs (mpair's)

    as one way to fix the
    [queue bug](http://awwx.ws/queue-test-summary), and to avoid needing
    to use pointer operations to modify Racket's "immutable" pairs.


* quasiquotation is implemented with Alan Bawden's algorithm

    as a fix for list splicing in nested quasiquotes, which was giving
    people trouble writing macro-defining macros.


* Function rest arguments are 'nil terminated Arc lists

         (cdr ((fn args args) 1)) => nil


* the Arc compiler is reflected into Arc

    where it can be hacked in Arc by redefining or extending the
    functions which implement the compiler:

         arc> (ac-literal? 123)
         t
         arc> (eval 123)
         123
         arc> =
         #<mac>
         arc> (ac-literal? =)
         nil
         arc> (eval =)
         Error: Bad object in expression #(tagged mac #<procedure>)
         arc> (defrule ac-literal? (isa x 'mac) t)
         #<fn>
         arc> (ac-literal? =)
         t
         arc> (eval =)
         #<mac>

* lexical identifiers take precedence over macros

         arc> (mac achtung (x) `(+ ,x 2))
         #<mac>
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


* (coerce '() 'cons) now returns nil

    thus any list can be coerce'd to a "cons", even though the empty
    list isn't actually represented by a cons cell.


* TCP ports no longer have associated custodians for force-close

    Arc 3.1's ac.scm says:

         ; there are two ways to close a TCP output port.
         ; (close o) waits for output to drain, then closes UNIX descriptor.
         ...
         ; mzscheme close-output-port doesn't work (just raises an error)
         ; if there is buffered output for a non-responsive socket.
         ; must use custodian-shutdown-all instead.

    I haven't been able to reproduce this behavior in any version of
    Racket or PLT Scheme that runs on my computer: in my testing sending
    data to a non-responsive client, `close-output-port` returns
    immediately and doesn't throw an error.

    Of course, if I'm wrong I'd be delighted to see an example
    demonstrating the problem.


* embedding other runtimes based on ar

    Multiple runtimes can loaded and run within the same memory space.
    Each runtime has its own set of global variables, and can have a
    different set of definitions loaded.  Thus the other runtimes can be
    a hacked version of ar, or have some other language than Arc loaded.

         arc> (use runtime)
         nil
         arc> (= a (runtime '(arc)))
         #<namespace:0>
         arc> a!+
         #<fn:+>
         arc> (a!eval '(map odd '(1 2 3 4 5 6)))


The Arc Implementation Language (Ail)
-------------------------------------

Ail is a language intermediate between Racket and Arc, though closer
to Racket than to Arc.  The Arc runtime is written in Ail, and the Arc
compiler compiles Arc to Ail.

The purpose of Ail is to make Arc more hackable, because it puts Arc's
runtime implementation in Arc's namespace where it can be directly
modified from Arc.

Ail is a terrible language for *writing* code in.  It is like assembly
language or bytecode: it's something you'd really rather have
generated *for* you.

Ail can also be used to access Racket from Arc, though it's not a very
convenient way to do that.  However, a more friendly interface from
Arc to Racket could probably be built that used Ail internally, or at
least used a few calls to `ail-code` to get going.

Ail details:

* Definitions and global variables are in Arc's namespace. Thus if you
  define a function `foo` in Ail, it becomes a function `foo` in Arc.
  Likewise, if code in Ail calls a function `bar`, and `bar` is
  defined in Arc, Arc's `bar` will be called.

* Function calls such as `(foo 1 2 3)` are made using Racket's plain
  function call mechanism, and so can only call functions.

* Racket identifiers are loaded into the namespace with a "racket-"
  prefix.  Thus you can refer to Racket's `+` with `racket-+`,
  Racket's `let` with `racket-let`, and so on.

* Ail code is not loaded in a Racket module, but is instead eval'ed
  one form at a time.  This is like Arc's `load` or Racket's
  [racket/load](http://docs.racket-lang.org/reference/load-lang.html)
  language.

    This means that Ail code isn't separated into compile-time and
    run-time phases like code in Racket's modules are; but it also means
    that we don't get some optimizations done for us that Racket's
    modules provide.

* Racket macros can be used from Ail code.  (But note this doesn't
  mean we can use Racket macros from *Arc*, because Arc's macro
  expansion and Racket's macro expansion are separate: all the Arc
  macros get expanded before Racket sees anything, and so there's no
  way to intermix Arc macros and Racket macros).

* Ail code can be generated from Arc by using `ail-code` (and an Arc
  macro can expand into `ail-code`, and so you can write Arc macros
  which generate Ail code).  For example, from Arc:

         (ail-code (racket-let ((foo 3))
                     (+ foo 2)))

    note that *Arc's* `+` is being called here, not Racket's.  If we
    wanted Racket's `+`, we'd use `racket-+`.

    `ail-code` is only necessary when we need to use a Racket macro or
    special form, since Racket functions can be called directly from
    Arc.  For example, from Arc:

         (racket-+ 3 4 5)


The Arc Compiler (ac)
---------------------

In Arc 3.1, the Arc compiler operates on Racket lists: it takes Arc
source code converted to a Racket list as input, and returns Racket
code as a Racket list as output.

In ar, the Arc compiler operates on Arc lists: it takes Arc code in
its original format as an Arc list, and returns a representation of
Ail code as an Arc list.

This choice of representation means that extensions to the compiler
can be written in Arc, and often using just plain Arc lists.

    arc> (defrule ac (is s 'foo) '(prn "Fee-fi-fo-fum!"))
    #<fn>
    arc> foo
    Fee-fi-fo-fum!

(note that the `prn` expression is in fact Ail code; it just happens
to be written the same as it would be in Arc because it's a simple
example).

In Arc `nil` represents both the symbol "nil" and the end of a list,
but in Racket (and thus in Ail) the end of list (and thus the empty
list as well) is represented by Racket's null: `()`.  (In addition,
an Arc list is constructed with mutable pairs and a Racket list is
constructed with immutable pairs).

When converting an Arc list representing Ail code into a Racket list
that can be fed to Racket's `eval`, we can choose some default for
when we'd like `nil` to be translated into either a symbol or the end
of list; but whatever choice we make there will be some other case
that the default doesn't cover.

The choice made in ar (which seems to be the most useful the most
often) is to translate an Arc `nil` into the symbol "nil" when it
appears in the car of a pair, and to translate it into a Racket null
"()" when it's in the cdr of a pair.

Thus in:

    arc> (ail-code (racket-quote nil))
    nil

the *input* expression as an Arc list, showing the end of list
terminator, looks like:

    (racket-quote nil . nil)

the first nil is in the car of a pair, and the second nil is in the
cdr.  Thus the expression when converted to a Racket list looks like:

    (racket-quote nil . ())

which is why Racket accepts the expression as a valid Racket list
terminated with `()`, but the the final result output is "nil" instead
of "()".

What this choice of default *doesn't* cover is representing an empty
list in a car position.  For example, suppose we wanted to specify the
argument list for a Racket lambda:

    `(racket-lambda ,args 123)

If `args` happens to be an empty list, we'd like the output to be the
Ail equivalent of the Racket code:

    (lambda () 123)

But that's not what happens.  The input expression as an Arc list
looks like:

    (racket-lambda nil 123 . nil)

which gets converted to the Racket list:

    (racket-lambda nil 123 . ())

for which the equivalent code in standard Racket would be:

    (lambda nil 123)

i.e., what we end up with is a lambda that takes any number of
arguments, that has a rest parameter called "nil".

We can tunnel arbitrary values through the Arc to Ail conversion using
`ar-tunnel`:

    `(racket-lambda ,(ar-tunnel (ar-list-fromarc args)) 123)

Now if `args` is the empty list `nil`, `ar-list-fromarc` will convert
that to a Racket null `()`, and we'll end up with a Racket lambda
which takes no arguments.


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

rocketnia

* explained why my definition of inline was broken by quote copying
  its value, and contributed the patch to make quote not do that.

* provided the patch to make lexical variables take precedence over
  macros with the same name; waterhouse contributed the test.

* provided a fix for the ssyntax precedence being different than Arc
  3.1.

Pauan

 * moved Arc's coerce and + functions out of ar; and made `(coerce
   '() 'cons)` return nil.

 * provided an implementation for defcall.

 * contributed testing by example.

 * made functions and macros print as #&lt;fn&gt; and #&lt;mac&gt;.

 * tests for extend and defrule.

ret comes from Sean Kenney's kwizwiz.arc:
https://github.com/skenney26/kwizwiz/blob/ac0547e7d090690bec3a8a5d06fb1cfcf6484b82/kwizwiz.arc#L22

.
