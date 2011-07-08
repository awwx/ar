(use git)

(defrule use-apply (parse-git-spec item)
  (usepath* (cons (git-repo it) (usepath*))))
