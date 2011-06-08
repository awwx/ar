(use test-by-example)

(example-test (runtime '(cwd)) #<<.

> (system "rm -rf /tmp/yXiFuztDIq")

> (ensure-dir "/tmp/yXiFuztDIq")

> (= cwd "/tmp")

> cwd
"/tmp/"

> (w/cwd "/tmp/yXiFuztDIq" (system "touch foo"))

> cwd
"/tmp/"

> (file-exists "/tmp/yXiFuztDIq/foo")
"/tmp/yXiFuztDIq/foo"

.
)
