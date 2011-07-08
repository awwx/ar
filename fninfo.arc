(use arc label path)

(deflabel fn-args)
(deflabel fn-body)

;; this would be
;;
;; (mac fn (args . body)
;;   (w/uniq f
;;      (ret ,f (ar-fn ,args ,@body)
;;        (= (fn-args ,f) ',args)
;;        (= (fn-body ,f) ',body))))
;;
;; except that the macro can't itself expand into an "fn" because
;; we'll get an infinite loop.

(def fn-impl (args . body)
  (w/uniq f
    `((ar-fn (,f)
        ((ail-code racket-hash-set!) label-table-fn-args* ,f ',args)
        ((ail-code racket-hash-set!) label-table-fn-body* ,f ',body)
        ,f)
      (ar-fn ,args ,@body))))

(assign fn (annotate 'mac fn-impl))

;; todo this fails to capture anonymous functions in arc and label

(each hack '(arc label)
  ;; todo we should be looking up where arc and label were actually
  ;; loaded from
  (each e (readfile (path arcdir* (string hack ".arc")))
    (when (and (acons e) (is (car e) 'def))
      (let (_ name args . body) e
        (let f (eval name)
          (= (fn-args f) args)
          (= (fn-body f) body))))))
