;;; -*- Mode: Lisp; -*-

(defpackage :muclr-server
  (:nicknames :server :ms)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :start-server :stop-servers))

(in-package :muclr-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; massive redesign begins here on 2012-12-25|17:45:30PDT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass muclr-listener ()
  ((handler :initform #'handler-basic :initarg :handler
	    :accessor muclr-listener-handler)
   (address :initform #(0 0 0 0) :initarg :address
	    :accessor muclr-listener-address)
   (port :initform 9902 :initarg :port :accessor muclr-listener-port)
   (socket :initform nil :initarg :socket :accessor muclr-listener-socket)
   (master-thread :initform nil :accessor muclr-listener-master-thread)
   (master-thread-sleep-time :initform 60
			     :accessor muclr-listener-master-thread-sleep-time)
   (threads :initform nil :accessor muclr-listener-threads)
   (information :initform #'new-muclr-information :initarg :information
		:accessor muclr-listener-information)
   (username :initform nil :initarg :username :accessor muclr-listener-username)))

(defstruct muclr-thread
  pid
  last-hit
  (quitting nil)
  state)
;  (state created :type (member created accepting connected finished dead nil)))

(defstruct muclr-information
  login-time
  username
  last-hit)

(defun new-muclr-information (&key login-time username last-hit)
  (make-muclr-information :login-time (if login-time login-time (get-universal-time))
			  :username username
			  :last-hit (if last-hit last-hit (get-universal-time))))

(defvar *platformname* nil)
(defvar *systemversion* nil)
(defvar *hostname* nil)
(defvar *port* nil)
(defvar *systemlogin* nil)
(defvar *systempassword* nil)
(defvar *loginstring* nil)
(setf *platformname* "muclr&")
(setf *systemversion* "1.10.101")
(setf *hostname* "main.platforms.muclr.org")
(setf *port* 9902)
(setf *systemlogin* "guest")
(setf *systempassword* "")
(setf *loginstring* "")

(defvar *server* nil)
(defvar *socket* nil)
(defvar *port* nil)

;; credential utilities

(defun hash-string (string &key digest-sequence)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    (if digest-sequence digest-sequence :md5)
    (ironclad:ascii-string-to-byte-array string))))

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
    (setf *systempassword* (cadr credentials))
    (setf *loginstring* credentialstring)))

(defun &hr-greet (stream)
  (format stream "~&gateway connect to ~a version ~a on ~a port ~d"
	  *platformname* *systemversion* *hostname* *port*)
  (terpri stream)
  (force-output stream))

;; example API string
;; evaluate (+ 3 5) 1001 3628285196 0103020119 bnlkZWw6bXVjbHJAMzYyODI4NTc0Mw==
;; example simplized API string
;; evaluate (+ 3 5)

(defvar *api-alist* nil)
(setf *api-alist* '(("connect" . &hr-api/connect)
		    ("evaluate" . &hr-api/evaluate)))

(defvar *blacklist* nil)
(setf *blacklist* '("null" "192 232 210 163" "foobar"))

(defun &hr-api-lookup (string)
  (assoc string *api-alist* :test #'string-equal))

(defun &hr-api (line stream)
  (let* ((list-command-arg
	 (cl-ppcre:split "\\s+" line :limit 2))
	(hr-api-lookup
	 (&hr-api-lookup (car list-command-arg))))
    (when hr-api-lookup
      (funcall (cdr hr-api-lookup)
	       (cadr list-command-arg)
	       stream))))

(defun &hr-api/connect (ipaddr stream)
  (let ((allowed
	 (if (member ipaddr *blacklist* :test #'string-equal) nil t)))
    (format stream "STREAM>> connect returned ~a" allowed)
    (format *standard-output* "STDIO>> connect returned ~a" allowed)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)))
  


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
    (&hr-api line stream)
;    (let ((eval-p (&hr-api/evaluate-p line)))
;      (when eval-p (progn (&hr-api/evaluate eval-p stream)
;			  (handle-request stream))))
    (unless (or (string-equal line "quit") (string-equal line ""))
      (handle-request stream))))

(defvar *my-stream* nil)

(defun &hr-master (stream)
;; all the handling functions in the family called together
;  (&hr-greet stream)
;  (&hr-login-do stream)
  (handle-request stream))

(defun &hr-master-init (stream)
  (setf *my-stream* stream)
  (&hr-master stream))
  
  ;; handlers need to be consolidated


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; muclr handler overhaul of 2014-12-25|17:53:40PDT begins here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handler-basic (stream listener)
  (let* ((line (read-line2 stream)))
    (format stream "~&~a:~a => ~a"
	    (muclr-listener-address listener)
	    (muclr-listener-port listener)
	    line)
    (terpri stream)
    (force-output stream)
    (unless (string-equal line "quit")
      (handler-basic stream listener))))

(defun &handler-basic (stream listener)
  (format stream "~&basic handler.")
  (format stream "~&you've connected to ~a on port ~a."
	  (muclr-listener-address listener)
	  (muclr-listener-port listener))
  (format stream "~&this has been the generic muclr listener. goodbye.~%")
  (values)
  (force-output stream)
  (terpri stream)
  (handler-basic stream listener))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-muclr-server (socket)
  (loop
     (wait-for-input socket)
     (let ((stream (socket-stream (socket-accept socket)))
	   (listener (make-instance 'muclr-listener :port *port* :socket socket)))
       (make-thread (lambda ()
		      (with-open-stream (stream stream)
			(&handler-basic stream listener)))
		    :name (format nil "muclr request handler for ~a" socket)))))


(defun run-server (socket)
  (loop (wait-for-input socket)
     (let ((stream (socket-stream (socket-accept socket))))
       (make-thread (lambda () (with-open-stream (stream stream)
				 (&hr-master-init stream)))
		    :name "muclr-server-request-handler-thread"))))

(defun start-server (port)
  (let ((socket (socket-listen *wildcard-host* port :reuse-address t)))
    (setf *socket* socket)
    (setf *port* port)
    (setf *server*
	  (make-thread
	   (lambda ()
	     (unwind-protect
		  (run-muclr-server socket)
	       (socket-close socket)))
	   :name (format nil "port ~a muclr server" port)))))

(defun stop-servers ()
  (mapcar #'destroy-thread (remove-if-not
			    (lambda (y)
			      (string-equal
			       (subseq (slot-value y 'sb-thread::name) 0 5) "muclr"))
			    (all-threads)))
  (mapcar #'destroy-thread (remove-if-not
			     (lambda (y)
			       (and
				(string-equal
				 (subseq (slot-value y 'sb-thread::name) 0 4) "port")
				(string-equal
				 (subseq (slot-value y 'sb-thread::name) (- (length (slot-value y 'sb-thread::name)) 12))
				 "muclr server")))
			    (all-threads))))
