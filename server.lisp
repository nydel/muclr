(ql:quickload '(:bordeaux-threads
		:cl-ppcre
		:usocket))

(defpackage :multi-user-common-lisp-repl-server
  (:nicknames :muclr-server :muclr-s)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :start-server :stop-servers))

(in-package :muclr-server)

(defvar *systemname* nil)
(defvar *systemversion* nil)
(defvar *hostname* nil)
(defvar *port* nil)
(defvar *systemlogin* nil)
(defvar *systempassword* nil)
(setf *systemname* "muclr&")
(setf *systemversion* "0.010 alpha")
(setf *hostname* "main.platforms.muclr.org")
(setf *port* 9001)

(defun clean-line (string)
  "get rid of trailing return carraiges on a #'readline string"
  (regex-replace-all "\\r" string ""))

(defun &hr-login-prompt (stream)
  (let ((login1 (progn
		  (format stream "login: ")
		  (force-output stream)
		  (clean-line
		  (read-line stream))))
	(pass1 (progn
		 (format stream "password: ")
		 (force-output stream)
		 (clean-line
		 (read-line stream)))))
	(terpri stream)
	(list login1 pass1)))

(defun &hr-login-do (stream)
  (let ((credentials (&hr-login-prompt stream)))
    (setf *systemlogin* (car credentials))
    (setf *systempassword* (cadr credentials))))

(defun &hr-greet (stream)
  (format stream "~&gateway connect to ~a version ~a on ~a port ~d"
	  *systemname* *systemversion* *hostname* *port*)
  (terpri stream)
  (force-output stream))

(defun handle-request (stream)
;; placeholder repl section of handler function family
  (let* ((line (read-line stream))
	 (line-clean (regex-replace-all "\\r" line "")))
    (format stream "STREAM>> ~a" line-clean)
    (format *standard-output* "STDIO>> ~a~&" line-clean)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)
    (unless (or (string-equal line-clean "quit") (string-equal line-clean ""))
      (handle-request stream))))

(defun &hr-master (stream)
;; all the handling functions in the family called together
  (&hr-greet stream)
  (&hr-login-do stream)
  (handle-request stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun run-server (socket)
  (loop (wait-for-input socket)
     (let ((stream (socket-stream (socket-accept socket))))
       (make-thread (lambda () (with-open-stream (stream stream)
				 (&hr-master stream)))
		    :name "muclr-server-request-handler-thread"))))

(defun start-server (port)
  (let ((socket (socket-listen *wildcard-host* port :reuse-address t)))
    (make-thread (lambda () (unwind-protect
				 (run-server socket)
			      (socket-close socket)))
		 :name "muclr-server-thread")))

(defun stop-servers ()
  (mapcar #'destroy-thread (remove-if-not
			    (lambda (y)
			      (string-equal
			       (slot-value y 'sb-thread::name) "muclr-server-thread"))
			    (all-threads))))
