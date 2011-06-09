(use arc)

(def err-message (message . args)
  (+ message (if args (+ ": " (intersperse " " (map tostring:write args))))))

(redef err (message . args)
  (racket-error (apply err-message message args)))
