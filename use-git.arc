(use git)

(defrule use-load (parse-git-spec item)
  (usepath* (cons (git-repo it) (usepath*))))
