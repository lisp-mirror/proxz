;;;; proxz.asd

(asdf:defsystem #:proxz
  :description "A open prozy scraper and tester."
  :author "Michael J. Rossi <dionysius.rossi@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:babel #:bordeaux-threads #:drakma #:quri #:plump #:cl-ppcre
                       #:split-sequence #:lparallel
                       #:unix-opts #:trivial-dump-core #:trivial-features)
  :components ((:file "package")
               (:file "proxz"))
  )

;;  :build-operation "program-op" ;; leave as is
;;  :build-pathname "proxz"
;;  :entry-point "proxz:main")
