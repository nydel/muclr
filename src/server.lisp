;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :muclr-server
  (:use :cl :bordeaux-threads :cl-ppcre :ironclad :usocket)
  (:export :start-server :stop-servers))

(in-package :muclr-server)

(defvar *platformname* nil)
(defvar *systemversion* nil)
(defvar *hostname* nil)
(defvar *port* nil)
(defvar *systemlogin* nil)
(defvar *systempassword* nil)
(setf *platformname* "muclr&")
(setf *systemversion* "1.10.101")
(setf *hostname* "main.platforms.muclr.org")
(setf *port* 9912)

;; credential utilities

(defun hash-string (string &key digest-sequence)
  (byte-array-to-hex-string
   (digest-sequence
    (if digest-sequence digest-sequence :md5)
    (ascii-string-to-byte-array string))))

;; utilities for terminal interaction

(defun clean-line (string)
  "get rid of trailing return carraiges on a #'readline string"
  (regex-replace-all "\\r" string ""))

(defun read-line2 (&optional (stream *standard-input*) (sb-impl::eof-error-p t) eof-value recursive-p)
  (clean-line
   (read-line (when stream stream)
	      (when sb-impl::eof-error-p sb-impl::eof-error-p)
	      (when eof-value eof-value)
	      (when recursive-p recursive-p))))

;; handling requests

(defun &hr-login-prompt (stream)
  (let ((login1 (progn
		  (format stream "login: ")
		  (force-output stream)
		  (read-line2 stream)))
	(pass1 (progn
		 (format stream "password: ")
		 (force-output stream)
		 (read-line2 stream))))
	(terpri stream)
	(list login1 pass1)))

(defun &hr-login-string (list)
  (let ((login-string
	 (concatenate 'string
		      (car list) ":"
		      (cadr list) "@"
		      (write-to-string (get-universal-time)))))
    (list login-string)))

(defun &hr-login-do (stream)
  (let* ((credentials (&hr-login-prompt stream))
	 (credentialstring (&hr-login-string credentials)))
    (setf *systemlogin* (car credentials))
    (setf *systempassword* (cadr credentials))))

(defun &hr-greet (stream)
  (format stream "~&gateway connect to ~a version ~a on ~a port ~d"
	  *platformname* *systemversion* *hostname* *port*)
  (terpri stream)
  (force-output stream))

;; example API string
;; evaluate (+ 3 5) 1001 3628285196 0103020119 bnlkZWw6bXVjbHJAMzYyODI4NTc0Mw==
;; example simplized API string
;; evaluate (+ 3 5)

(defun &hr-api/evaluate (string stream)
  (let ((result (eval (read-from-string string))))
    (format stream "STREAM>> evaluating: ~a" string)
    (format *standard-output* "STDIO>> evaluating: ~a~&" string)
    (terpri stream)
    (terpri *standard-output*)
    (format stream ">> ~a" result)
    (format *standard-output* ">> ~a~&" result)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)))

(defun &hr-api/evaluate-p (line)
  (let ((list-command-arg
	 (cl-ppcre:split "\\s+" line :limit 2)))
    (if (and (> (length list-command-arg) 1)
	     (string-equal (car list-command-arg) "evaluate"))
	(cadr list-command-arg)
	nil)))

(defun handle-request (stream)
;; placeholder repl section of handler function family
  (let ((line (read-line2 stream)))
    (format stream "STREAM>> ~a" line)
    (format *standard-output* "STDIO>> ~a~&" line)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)
    (let ((eval-p (&hr-api/evaluate-p line)))
      (when eval-p (progn (&hr-api/evaluate eval-p stream)
			  (handle-request stream))))
    (unless (or (string-equal line "quit") (string-equal line ""))
      (handle-request stream))))

(defun &hr-master (stream)
;; all the handling functions in the family called together
;  (&hr-greet stream)
;  (&hr-login-do stream)
  (handle-request stream))
  
  ;; handlers need to be consolidated

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
