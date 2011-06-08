(use arc)

(mac ret (var val . body)
 `(let ,var ,val ,@body ,var))
