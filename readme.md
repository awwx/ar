The goals of this Arc runtime project are:

* to fix bugs in the runtime

* to make Arc more hackable

* and to do that while avoiding changing the Arc language.

The later two go together because when there's some change we'd like
to make to Arc, we can make Arc more hackable instead, and then we can
get the "different" Arc that we want as a library, instead of having
to actually change Arc.

This code is under development, most of Arc is still unimplemented.

There's now a toy REPL.  (It only reads one line and eval's that,
instead of reading as many lines as are needed to complete the input).

To run:

    racket ar.ss

or, if you have rlwrap:

    rlwrap -q \" racket ar.ss

You can also use "mzscheme" instead of "racket".

Note that you don't use the "-f" option like you would with Arc 3.1.

Runtime tests can be run by using "--test-inline" or "--test-atend" command
line arguments.  (Tests are slow because the Arc compiler and Arc are
loaded afresh for each test).

Running all tests (both the runtime tests and the Arc tests) can be
done with:

    racket ar.ss --test-inline ar.t arc.t


Ways to Help
------------

### Write tests for Arc

Writing tests is often easy.  A test is just an example that throws
an error if an expression doesn't return the expected result or do the
expected thing. Here's a test for `sort`:

    (unless (iso (sort < '(5 3 7)) '(3 5 7)) (err "nope"))

If you find it convenient, you can use the unit testing library at
[test.arc](https://github.com/awwx/ar/blob/master/test.arc), with that
the above example can be written as:

    (testis (sort < '(5 3 7)) '(3 5 7))

or you can use your own unit testing library... write tests in
whatever way is most convenient for you.

You don't need to run this new runtime at all, just write tests for
Arc 3.1.  You also don't have to figure out exactly what the result of
an expression will be; instead, type the expression into the Arc 3.1
REPL, and copy the answer.

Tests for Arc will be valuable for far more than just this runtime.
They can be used for all the other runtimes that people work on
(implementations of Arc on top of Java, Javascript, etc. etc.)  And
they serve as illustrative examples.  People wondering what `sort`
does and how to use it can look at your example and see.

Which tests should you write? First naturally you should look at the
existing [test files](https://github.com/awwx/ar) to make sure that
the test you're thinking of hasn't already been written.  After that,
test the things that your own Arc programs use.  Does your Arc program
call `sort`, or `begins`, or `img`?  Add a couple of debugging lines
to find out with what arguments your program calls the function and
what it returns, and make a test for that.

Tests are enormously helpful.  I don't *know* how your program uses
Arc, I'm just guessing.  With a test I can immediately see what's not
working, or if a change I've made has broken something -- that in turn
would break your program.


### Pick a to-do item

There's a list of to-do's below, you can pick one and do it :-).  You
can fork the github project and push changes to me that way, or just
send me an email with your patches.

Note that patches to the runtime itself need to be released under the
Perl Artistic License, as the code is derived from Arc 3.1.  (You can
*also* release your contributions under whatever other license you
want, as long as you dual-licence with the Perl Artistic License).

 
### Address code quality issues

My primary focus so far has been getting things to work, with less
attention on readable code.  A fresh pair of eyes can see duplicate
code, awkward names, and notice less confusing ways of doing things.

### Find bugs

Running the runtime right now is pretty slow because none of the Arc
optimizations have been written yet (they're on the to-do list...)
However, if you can stand it and do find a bug, bug reports are
*greatly* appreciated :D.




Todo
----

* ac-nameit, ac-dbname
* atstrings
* ac-binaries
* direct-calls
* ar-funcallN optimizations
* ac-macex
* explicit-flush
* incremental parsing
* full REPL
* declare
* primitives
* optional namespace argument to eval
* Arc 3.1 calls ac-macex in ac-assignn... I wonder why?
* either need not to have a macro called "test", or not have macro
  names take priority over lexical variables, to avoid renames such as
  "testff" for test.

Idea: Testing each definition immediately after it's been defined is a
useful discipline for ensuring that we don't have any forward
references, which in turn makes hacking and rebasing the code easy.
But the tests are particularly ugly right now, in part because we
don't have much of Arc implemented yet at the time we want to test.
Since we can now create new Arc runtimes with new-ac, perhaps we could
create a complete version of Arc which could then incrementally test
the Arc under construction.

 
Changes
-------

This version of the Arc runtime:

* Implements Arc lists using Racket's mutable pairs (mpair's)

  as a fix for the [queue bug](http://awwx.ws/queue-test-summary).


* implements quasiquotation with Alan Bawden's algorithm

  as a fix for list splicing in nested quasiquotes, which was giving
  people trouble writing macro-defining macros.


* Function rest arguments are 'nil terminated Arc lists

         (cdr ((fn args args) 1)) => nil


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


* Reflects the Arc compiler into Arc to make Arc more hackable

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


* Arc reader implemented in Arc


* global variables are stored in an Arc table instead of in a Racket namespace

  as an experiment to see if the simpler data structure is sufficient.


* replaces (stdin), (stdout), (stderr) with stdin, stdout, stderr

  removing an unnecessary layer of parentheses; though violating
  goal #3.


* uniq implemented using Racket's gensym


* defvar allows global variables to be hacked to supply your own
  implementation for getting or setting the variable


* implicit variables

  which can help make programs more concise when the same variable
  doesn't need to be threaded through many layers of function calls.


* readline accepts CR-LF line endings

  which is useful for Internet protocols such as HTTP.

* tables read and print as "#table" followed by the tablist of the
  table

Note that most of these choices are very easily reversed if they turn
out to be a bad idea.


Acknowledgments
---------------

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

Rocketnia explained why my definition of inline was broken by quote
copying its value, and contributed the patch to make quote not do
that.
