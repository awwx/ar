(use test-by-example test path)

(w/testdir
 (let dir (path testdir "git")

   (let r (runtime '(use-git))
     (= r!git-cachedir* dir)
     (example-test r #<<.

> (use "git://github.com/awwx/for-testing.git" bar)
prints: hello, this is bar\n

.
     ))

   (let r (runtime '(use-git))
     (= r!git-cachedir* dir)
     (example-test r #<<.

> (use "git://github.com/awwx/for-testing.git" bar)
prints: hello, this is bar\n

.
     ))))
