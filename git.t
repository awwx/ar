(use test test-by-example path cwd)

(example-test (runtime '(print-table git)) #<<.

> (filenamize "github.com/nex3/arc.git")
"github.com_nex3_arc.git"

> (parse-git-spec "foo.arc")
nil

> (parse-git-spec "git://github.com/nex3/arc.git")
#table((repo "git://github.com/nex3/arc.git"))

> (parse-git-spec "git://github.com/nex3/arc.git:lib/ns.arc")
#table((file "lib/ns.arc") (repo "git://github.com/nex3/arc.git"))

> (parse-git-spec "git://github.com/nex3/arc.git!arcc")
#table((repo "git://github.com/nex3/arc.git") (revision "arcc"))

> (parse-git-spec "git://github.com/nex3/arc.git!arcc:arcc/ac.arc")
#table((file "arcc/ac.arc") (repo "git://github.com/nex3/arc.git") (revision "arcc"))

> (git-dir (parse-git-spec "git://github.com/nex3/arc.git!arcc:arcc/ac.arc"))
"github.com_nex3_arc.git/arcc"

.
)

(w/testdir
 (let r (runtime '(git))
   (= r!git-cachedir* (path testdir "git"))

   (let repo (path testdir "git/github.com_awwx_for-testing.git/foo2")
     (testis (r!git-repo "git://github.com/awwx/for-testing.git!foo2")
             repo)
     (testis
      (filechars (path repo "foo"))
      "version 2\n"))

   (let repo (path testdir "git/github.com_awwx_for-testing.git/8a74b81b27d5d4385ef1")
     (testis (r!git-repo "git://github.com/awwx/for-testing.git!8a74b81b27d5d4385ef1")
             repo)
     (testis
      (filechars (path repo "foo"))
      "version 1\n"))

   (testis (r!git-filepath "git://github.com/awwx/for-testing.git:foo")
           (path testdir "git/github.com_awwx_for-testing.git/master/foo"))

   (testis (~~file-exists (r!git-filepath "git://github.com/awwx/for-testing.git:bar.arc"))
           t)

   (w/cwd (r!git-repo "git://github.com/awwx/for-testing.git")
     (system "git reset --hard HEAD^"))

   (testis (~~file-exists (r!git-filepath "git://github.com/awwx/for-testing.git:bar.arc"))
           nil)

   (r!git-pull "git://github.com/awwx/for-testing.git")

   (testis (~~file-exists (r!git-filepath "git://github.com/awwx/for-testing.git:bar.arc"))
           t)))
