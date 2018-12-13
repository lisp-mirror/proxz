(format t "~%*** Checking for quicklisp...~%")
(if (ignore-errors (package-name :ql))
    (load "build-exe.lisp")
    (progn
      (format t "Please install Quicklisp and try again.~%")
      (quit)))
