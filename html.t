(use runtime test-by-example)

(example-test (runtime '(html print-table)) #<<.

> (color 33 44 55)
#table((b 55) (g 44) (r 33))

> (dehex "41")
65

> (hex>color "414243")
#table((b 67) (g 66) (r 65))

> (gray 127)
#table((b 127) (g 127) (r 127))

> (hexreps 14)
"0e"

> (hexreps 255)
"ff"

> (hexrep orange)
"ff6600"

> (pr-escaped "2 < 5")
prints: 2 &#60; 5

> (tag (a id 'foo href "http://arclanguage.org/") (pr "Arc"))
prints: <a id=foo href="http://arclanguage.org/">Arc</a>

> (tag (body bgcolor orange leftmargin 3) (pr "content"))
prints: <body bgcolor=#ff6600 leftmargin=3>content</body>

> (gentag input name 'foo value "2 < 5")
prints: <input name="foo" value="2 &#60; 5">

> (inputs u username 20 nil
          p password 20 nil)
prints: <table border=0><tr><td>username:</td><td><input type=text name="u" size=20></td></tr><tr><td>password:</td><td><input type=password name="p" size=20></td></tr></table>

> (parafy "one\n\ntwo")
"one\n\n<p>two"

> (valid-url "http://arclanguage.org/")
t

> (valid-url "http://arclanguage.org/<3>")
nil

.
)
