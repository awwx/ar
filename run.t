(use test system-code)

(testis
 (tostring
  (check-system "./run" "git://github.com/awwx/for-testing.git" "bar"))
 "hello, this is bar\n")
