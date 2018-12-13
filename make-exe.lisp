;; This has workd on SBCL, CCL, and ABCL.
(format t "Loading dependencies...~%")
(ql:quickload '(:babel :bordeaux-threads drakma quri plump cl-ppcre
                        split-sequence lparallel unix-opts trivial-dump-core trivial-features))
(asdf:load-system :proxz)
(format t "Creating executable...~%")
(in-package :proxz)
(build)
  
