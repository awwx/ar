(use strings parsimonious-urlencode path ret cwd system-code)

(= ar-cachedir*  (racket-path->string (racket-expand-user-path "~/.ar-cache")))
(= git-cachedir* (path ar-cachedir* "git"))

(def filenamize (hack)
  (let hack string.hack
    (parsimonious-urlencode
     (subst "_" "/"
      (subst "__" "_" hack)))))

(def parse-git-spec (spec)
  (zap string spec)
  (and (begins spec "git://")
       (iflet p (pos [in _ #\: #\!] spec 6)
         (with (repo (cut spec 0 p)
                rest (cut spec p))
           (if (begins rest "!")
                (iflet p (pos #\: rest)
                  (obj repo     repo
                       revision (cut rest 1 p)
                       file     (cut rest (+ 1 p)))
                  (obj repo     repo
                       revision (cut rest 1)))
                (obj repo repo
                     file (cut rest 1))))
         (obj repo spec))))

(def git-spec (spec)
  (if (isa spec 'table)
       spec
       (or (parse-git-spec spec)
           (err "unable to parse git spec" spec))))

(def git-revision (git)
  (zap git-spec git)
  (or git!revision "master"))

(def git-dir (git)
  (path (filenamize (cut git!repo 6))
        (filenamize (git-revision git))))

(def gitcmd args
  (w/stdout stderr
    (apply check-system "/usr/bin/git" args)))

(def git-repo (git)
  (zap git-spec git)
  (ret gitdir (path git-cachedir* (git-dir git))
    (unless (dir-exists gitdir)
      (let dir (dirpart gitdir)
        (ensure-dir dir)
        (w/cwd dir
          (gitcmd "clone" "--no-checkout" git!repo (filepart gitdir)))
        (w/cwd gitdir
          (gitcmd "checkout" "-q" (git-revision git)))))))

(def git-filepath (git)
  (zap git-spec git)
  (path (git-repo git) git!file))

(def git-pull (git)
  (w/cwd (git-repo git)
    (gitcmd "pull")))

(def git-loc (git)
  (zap git-spec git)
  (string "git://" git!repo (aif git!revision (+ "!" it))))
