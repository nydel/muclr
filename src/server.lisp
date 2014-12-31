;;;; -*- Mode: Lisp; -*-

(defpackage :muclr-server
  (:nicknames :server)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :*master-socket*
	   :*connections*
	   :*clos-connections*
	   :connection
	   #:start-server
	   #:start-master-socket))

(in-package :muclr-server)

(defclass connection ()
  ((socket :initarg :socket
	   :initform nil
	   :accessor connection-socket)
   (hostname :initarg :hostname
	     :initform nil
	     :accessor connection-hostname)
   (port :initarg :port
	 :initform nil
	 :accessor connection-port)
   (stream :initarg :stream
	   :initform nil
	   :accessor connection-stream)
   (thread :initarg :thread
	   :initform nil
	   :accessor connection-thread)
   (timestamp :initarg :timestamp
	      :initform (get-universal-time)
	      :accessor connection-timestamp)
   (username :initarg :username
	     :initform nil
	     :accessor connection-username)))

(defun build-connection (&key socket hostname port stream thread timestamp)
  (make-instance 'connection :socket socket :hostname hostname :port port :stream stream :thread thread :timestamp timestamp :username nil))

(defclass platform ()
  ((hostname :initarg :hostname
	     :initform nil
	     :accessor platform-hostname)
   (port :initarg :port
	 :initform nil
	 :accessor platform-port)
   (title :initarg :title
	  :initform nil
	  :accessor platform-title)
   (description :initarg :description
		:initform nil
		:accessor platform-description)
   (users :initarg :users
	  :initform nil
	  :accessor platform-users)
   (number-users :initarg :number-users :initform 0 :accessor platform-number-users)
   (max-users :initarg :max-users
	      :initform 19
	      :accessor platform-max-users)
   (timestamp :initarg :timestamp
	      :initform (get-universal-time)
	      :accessor platform-timestamp)
   (lease :initarg :lease
	  :initform nil
	  :accessor platform-lease)))

(defvar *master-socket* nil)
(defvar *connections* nil)
(defvar *clos-connections* nil)

(defvar *users* nil)
(setf *users* '(("nydel" . "password")("guest" . "guest")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(defun clean-line (string)
;  (regex-replace-all "\\r" string ""))

(defun clean-line (line)
  (string-right-trim (string #\Return) line))

(defun read-line-no-cr (&optional
			    (stream *standard-input*)
			    (sb-impl::eof-error-p t)
			    eof-value recursive-p)
  (clean-line
   (read-line (when stream stream)
	      (when sb-impl::eof-error-p sb-impl::eof-error-p)
	      (when eof-value eof-value)
	      (when recursive-p recursive-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun force-output-to-connection (string con &optional (terpri-before-p t)(terpri-after-p t))
  (let ((stream (connection-stream con))
	(hostname (connection-hostname con))
	(port (connection-port con)))
    (when terpri-before-p (terpri stream) (force-output stream))
    (format stream "STREAM@~s:~s>> ~a" hostname port string)
    (force-output stream)
    (when terpri-after-p (terpri stream) (force-output stream))))

;(defun api/greet (stream con)
;  (format stream "~%~80;~%

(defun api/valid-user (user pass)
  (let ((userpass (assoc user *users* :test #'string-equal)))
    (when (string-equal pass (cdr userpass)) t)))

(defun api/login (stream con)
  (if (connection-username con)
      (force-output-to-connection "you're already logged in!" con t t)
      (let ((login (progn
		     (format stream "login: ") (force-output stream)
		     (read-line-no-cr stream)))
	    (pass (progn
		    (format stream "password: ") (force-output stream)
		    (read-line-no-cr stream))))
	(when (api/valid-user login pass)
	  (setf (connection-username con) login)))))

(defun request-handler (stream &optional con login-p)
  (when login-p (api/login stream con))
  (let ((line (read-line-no-cr stream))
	(con (if con con
		 (car (remove-if-not
		       (lambda (y)
			 (equal (connection-stream y) stream)) *clos-connections*)))))
    (format stream "~a@~s:~s>> ~a"
	    (if (connection-username con) (connection-username con) (string "STREAM"))
	    (connection-hostname con) (connection-port con) line)
    (format *standard-output* "STDIO>> ~a~&" line)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)
    (when (string-equal line "/login")
      (request-handler stream con t))
    (unless (or (string-equal line "quit") (string-equal line ""))
      (request-handler stream con))))

(defun handle-client-input (socket)
  (let* ((stream (socket-stream socket))
	 (con (build-connection :socket socket :stream stream :timestamp (get-universal-time)
			       :hostname (get-peer-address socket) :port (get-peer-port socket)
			       :thread nil)))
    (setf (connection-thread con)
	  (make-thread
	   (lambda ()
	     (with-open-stream (stream stream)
	       (request-handler stream)))
	   :name (format nil "muclr stream at ~d" (get-universal-time))))
    (push con *clos-connections*)))

(defun handle-client-connect (socket)
    (handle-client-input socket))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-server ()
  (loop
     (loop :for s :in (wait-for-input *connections* :ready-only t) :do
	(if (eq s *master-socket*)
	    (let ((new (socket-accept s)))
	      (setf *connections* (nconc *connections* `(,s)))
	      (handle-client-connect new))
	    (handle-client-input s)))))


(defun &start-master-socket (port)
  (let ((socket (socket-listen *wildcard-host* port :reuse-address t)))
    socket))

(defun start-master-socket (port)
  (setf *master-socket* (&start-master-socket port))
  (setf *connections* (list *master-socket*)))

(defun start-server (port)
  (start-master-socket port)
  (make-thread
   (lambda ()
     (run-server))
   :name (format nil "port ~d muclr server" port)))
