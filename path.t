(use test test-by-example)

(let r (runtime '(path))

  (example-test r #<<.

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

> (fullpath "/a/b/c/foo.bar")
"/a/b/c/foo.bar"

> (w/cwd "/tmp" (fullpath "foo.bar"))
"/tmp/foo.bar"

> (fullpath "foo.bar" "/tmp")
"/tmp/foo.bar"

> (fullpath "/tmp/a/b/../foo")
"/tmp/a/foo"

.
  )

  (testis (r!fullpath "~")
          (+ (trim (tostring (system "cd ~; pwd"))) "/")))
