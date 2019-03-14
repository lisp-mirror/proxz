#|

proxz.lisp
Michael Rossi
dionysius.rossi@gmail.com

This is a Common Lisp program to scrape open proxies from proxz.com and validate them.
I've tested this on Windows 10 using SBCL & CCL.

How it works?
We first generate a list of IPs & Ports from proxz.com.  This would be straight forward,
but the site tries to trick scrapers like this by encoding the relevant HTML in encoded
Javascript, which itself includes another call to self.document.writeln which an eval
is called upon.  So there's a lot of tweaking the raw data to get the actual results.

Once we have the results, we simple try to validate each proxy by checking our IP on
duckduckgo.com.  If run as a standalone program, the output will be in proxychains
format.

Since I ran into problems with the both Dexador and Drakma hanging for very long periods
of time, despite having their "timeout" values set, I wrote with-timeout-guard macro to 
force a time out.  It creates two threads, the first to perform the body, the second to
sleep and then kill the first if it wakes up before the first thread given a timeout value.
This requires bt-semaphore, but seems to work well for what we want.  See the macro
itself for more info.

TODO:
Green threads instead of lparallel?
Otherwise, add task handler shutdown on lparallel

|#

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:babel :bordeaux-threads drakma quri plump cl-ppcre
                  split-sequence lparallel
                  unix-opts trivial-dump-core trivial-features)))
|#

(in-package #:proxz)

(defmacro with-timeout-guard (timeout &body body)
  "This is a timeout facility using Bordeaux threads to force a timeout and return nil
once a timeout in seconds has been exceeded.  We use this because the we ran into
hanging threads when testing proxies on both Dexador and Drakma.  The macro wraps the
results in ignore-errors, so if you use this, you'll want to design your function body
to return nil if they fail. Also, if you need to print, we have rebound *standard-output*
to top-level.  So you'll need to do (format top-level ...) to print.

Example:

(defun lookup ()
  (with-timeout-guard 3
    (format top-level \"Doing stuff.....\")
    (sleep 10)
    'done))
"
  `(ignore-errors
     (let* ((top-level *standard-output*)
            (lookup-thread
             (bt:make-thread
              (lambda ()
                ,@body)))
            (destroy-thread
             (bt:make-thread
              (lambda ()
                (sleep ,timeout)
                (when (bt:thread-alive-p lookup-thread)
                  (bt:destroy-thread lookup-thread)
                  nil)))))
       (declare (ignore destroy-thread top-level))
       (bt:join-thread lookup-thread))))

(defun print-thread-info ()
  "Helper function to monitor threads."
  (let* ((curr-thread (bt:current-thread))
         (curr-thread-name (bt:thread-name curr-thread))
         (all-threads (bt:all-threads)))
    (format t "Current thread: ~a~%~%" curr-thread)
    (format t "Current thread name: ~a~%~%" curr-thread-name)
    (format t "All threads:~% ~{~a~%~}~%" all-threads))
  nil)

(defun ip-whois (ip)
  "Returns the raw XML of an ARIN lookup.  Not currently used."
  (let* ((cookie-jar (make-instance 'drakma:cookie-jar))
         (uri (multiple-value-bind (body status response-headers uri stream)
                  (drakma:http-request
                   "https://whois.arin.net/ui/query.do"
                   :method :post
                   :parameters
                   `(("flushCache" . "false")
                     ("queryinput" . ,ip)
                     ("whoisSubmitButton" . "+")
                     ("xslt" . "https://localhost:8080/whoisrws/servlet/arin.xsl"))
                   :cookie-jar cookie-jar)
                (declare (ignore body status response-headers stream))
                (format nil "~A" uri))))
    (babel:octets-to-string (drakma:http-request uri :cookie-jar cookie-jar))))

(defun ip-whois-company-name (ip)
  "Returns a list of ARIN WHOIS company names.  Not currently used."
  (let* ((xml (ip-whois ip))
         (parsed (plump:parse xml))
         (names (plump:get-elements-by-tag-name parsed "companyName")))
    (loop for name in names collect
         (plump:text name))))

(defun current-public-ip (&key (proxy-ip nil) (proxy-port 80) (timeout 5))
  "Returns a string of the current public facing IP address by querying Duckduckgo.com.
Takes an optional proxy value like 'http://160.16.73.108:10000'"
  (with-timeout-guard timeout
    (ignore-errors ; since the proxies could be bad
      (let* ((query "https://duckduckgo.com/lite/?q=ip")
             (html (if proxy-ip
                       (drakma:http-request
                        query
                        :proxy `(,proxy-ip ,proxy-port))
                       (drakma:http-request
                        query))))
        (multiple-value-bind (match ip)
            (cl-ppcre:scan-to-strings "Your IP address is (\\d+.\\d+.\\d+.\\d+) in" html)
          (declare (ignore match))
          (when (= (length ip) 1)
            (aref ip 0)))))))

(defun proxz-proxies (url)
  "Returns a list of lists of IPs and Ports on the proxz us anon list.
The site encodes the actual proxy data in Javascript encoding, and prints it
via self.document.writeln.  So we have to decode and then strip, which is why we
have decode-1 and decode-2."
  (ignore-errors ; since we're web scraping....
    (let* ((raw (drakma:http-request url))
           (parsed (plump:parse raw))
           ;; the proxy data is in the 7th table
           (tables (plump:get-elements-by-tag-name parsed "table"))
           (proxy-table (plump:text (nth 7 tables)))
           ;; get the encoded part
           (encoded-proxies (second (split-sequence:split-sequence #\' proxy-table)))
           (decode-1 (quri:url-decode encoded-proxies))
           ;; pull out the data after the self.document.writeln
           (decode-2 (multiple-value-bind (match str)
                         (cl-ppcre:scan-to-strings "\"([^\"]+)" decode-1)
                       (declare (ignore match))
                       (when (= (length str) 1)
                         (aref str 0))))
           ;; get all the rows and table data
           (rows (plump:get-elements-by-tag-name (plump:parse decode-2) "tr"))
           (td (loop for i in rows collect
                    (loop for td in (plump:get-elements-by-tag-name i "td")
                       collect (plump:text td)))))
      ;; finally we remove the "additional info" line and reverse so it's IP then PORT
      (reverse (loop for i in td collect
                    (reverse (rest i)))))))

(defun proxz-us-anon-proxies ()
  "Returns a list of lists of IPs and Ports on the proxz us anon list.
The site encodes the actual proxy data in Javascript encoding, and prints it
via self.document.writeln.  So we have to decode and then strip, which is why we
have decode-1 and decode-2."
  (proxz-proxies "http://www.proxz.com/proxy_list_anonymous_us_0.html"))

(defun all-proxz-proxies ()
  "Returns a unique list of all the proxz proxies but transparent."
  (let ((high (proxz-proxies "http://www.proxz.com/proxy_list_high_anonymous_0.html"))
        (us (proxz-proxies "http://www.proxz.com/proxy_list_anonymous_us_0.html"))
        (uk (proxz-proxies "http://www.proxz.com/proxy_list_uk_0.html"))
        (canada (proxz-proxies "http://www.proxz.com/proxy_list_ca_0.html"))
        (china (proxz-proxies "http://www.proxz.com/proxy_list_cn_ssl_0.html"))
        (japan (proxz-proxies "http://www.proxz.com/proxy_list_jp_0.html"))
        (france (proxz-proxies "http://www.proxz.com/proxy_list_fr_0.html"))
        (std (proxz-proxies "http://www.proxz.com/proxy_list_port_std_0.html"))
        (non-std (proxz-proxies "http://www.proxz.com/proxy_list_port_nonstd_0.html"))
        ;;(trans (proxz-proxies "http://www.proxz.com/proxy_list_transparent_0.html"))
        )
    (remove-duplicates
     (append high us uk canada china japan france std non-std)
     :test #'equal :key #'car)))

(defun test-proxy (ip-port-list &key (timeout 5) (verbose nil))
  "Tests a proxy to try to validate it by checking the current-public-ip.
Returns a list of an ip and port or nil."
  (let* ((ip (first ip-port-list))
         (port (second ip-port-list))
         (proxy (format nil "http://~A:~A" ip port))
         (proxy-ip ip)
         (proxy-port (parse-integer port :junk-allowed t))
         (result (progn
                   (when verbose
                     (format t "[-] Checking ~A~%" proxy))
                   (current-public-ip :proxy-ip proxy-ip :proxy-port proxy-port
                                      :timeout timeout))))
    (when result
      (when verbose
        (format t "[+] Found Proxy: ~A => ~A~%" proxy result))
      ip-port-list)))

(defun multi-test-proxies (proxies &key (threads 8) (timeout 5) (verbose nil))
  "Test the proxies with multiple threads."
  (let ((lparallel:*kernel* (lparallel:make-kernel threads)))
    (when verbose
      (format t "[-] Testing ~A proxies with ~A threads.~%"
              (length proxies) lparallel:*kernel*))
    (let ((results
           (remove nil
                   (lparallel:pmap
                    'list
                    #'(lambda (i) (test-proxy i :verbose verbose :timeout timeout))
                              proxies))))
      (lparallel:end-kernel)
      results)))

(defun valid-proxz-proxies (&key (threads 8) (timeout 5) (verbose nil) (us-only nil))
  "Main proxy lookup and validation function.  Returns a list of lists of IP & PORT."
  (when verbose
    (format t "[-] Loading proxy data from proxz.com...~%"))
  (let ((proxies (if us-only
                     (proxz-us-anon-proxies)
                     (all-proxz-proxies))))
    (when proxies
      (when verbose
        (format t "[-] Successfully loaded proxy data, testing...~%"))
      (let ((results (multi-test-proxies proxies
                                         :threads threads
                                         :timeout timeout
                                         :verbose verbose)))
        (when verbose
          (format t "[+] Verification complete. Validated ~A proxies.~%" (length results))
          (format t "---------------------------------------------------------~%")
          (dolist (i results)
            (format t "#http~C~A~C~A~%" #\tab (first i) #\tab  (second i)))
          (format t "---------------------------------------------------------~%"))
        results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command line args & Building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(opts:define-opts
  (:name :help
           :description "Proxz Help."
           :short #\h
           :long "help")
    (:name :us-only
           :description "Only fetch US proxies."
           :short #\u
           :long "us-only")
    )

(defun main-help (&rest args)
  (declare (ignore args))
  (format t "Usage: proxz [-u US proxies only]~%")
  (opts:exit))

(defun main-run (&key (us-only nil))
  (time (valid-proxz-proxies :threads 10 :verbose t :us-only us-only))
  (terpri)
  #+win32 (progn
            (format t "* Press ENTER to close *~%")
            (read-line)))

(defun main ()
  "Command line default."
  (multiple-value-bind (options free-args)
      (handler-bind ((opts:unknown-option #'main-help) ;; the condition / our function
                     (opts:missing-arg #'main-help)
                     (opts:arg-parser-failed #'main-help)
                     (opts:missing-required-option #'main-help)
                     )
        (opts:get-opts))
    (declare (ignore free-args))
    ;;(format t "Options: ~A~%" options)
    ;;(format t "Free-args: ~A~%" free-args)
    (let ((us-only nil))
      (when (getf options :help)
        (main-help))
      (when (getf options :us-only)
        (setf us-only t)
        (format t "Fetching only US proxies!~%"))        
      ;; Handle ctrl-c interrupt
      (handler-case
          (main-run :us-only us-only)
        (#+sbcl sb-sys:interactive-interrupt
          #+ccl  ccl:interrupt-signal-condition
          #+clisp system::simple-interrupt-condition
          #+ecl ext:interactive-interrupt
          #+allegro excl:interrupt-signal
          ()
          (progn
            (format *error-output* "Abort.~&")
            (opts:exit)))))))

(defun build ()
  "Builds a standalone exe."
  (let ((name (or #+win32
                  "proxz.exe"
                  "proxz")))
    (trivial-dump-core:save-executable name #'proxz::main)))
