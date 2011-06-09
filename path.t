(use test-by-example)

(example-test (runtime '(path)) #<<.

> (path "abc")
"abc"

> (path "abc" "def")
"abc/def"

> (path "abc" "def" "ghi")
"abc/def/ghi"

> (dirpart "foo")
nil

> (dirpart "a/b/c/foo")
"a/b/c/"

> (dirpart "/a/b/c")
"/a/b/"

> (filepart "/a/b/c/")
nil

> (filepart "/a/b/c/foo.bar")
"foo.bar"

> (filepart "foo")
"foo"

.
)
