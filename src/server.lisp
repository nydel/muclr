;;;; -*- Mode: Lisp; -*-

(defpackage :muclr-server
  (:nicknames :server)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :*master-socket*
	   :*server*
	   :*muclr-servers*
	   :*connections*
	   :*clos-connections*
	   :muclr-server
	   :connection
	   #:start-server
	   #:stop-server
	   #:start-master-socket))

(in-package :muclr-server)

(defvar *muclr-servers* nil)

(defclass muclr-server ()
  ((hostname :initarg :hostname
	     :initform nil
	     :accessor muclr-server-hostname)
   (ip-addr :initarg :ip-addr
	    :initform nil
	    :accessor muclr-server-ip-addr)
   (port :initarg :port
	 :initform nil
	 :accessor muclr-server-port)
   (socket :initarg :socket
	   :initform nil
	   :accessor muclr-server-socket)
   (passphrase :initarg :passphrase
	       :initform nil
	       :accessor muclr-server-passphrase)))

(defun build-muclr-server (&key hostname ip-addr port socket)
  (make-instance 'muclr-server
		 :hostname hostname
		 :ip-addr ip-addr
		 :port port
		 :socket socket))

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
  (make-instance 'connection :socket socket :hostname hostname :port port
                             :stream stream :thread thread :timestamp timestamp
                             :username nil))

(defclass platform ()
  ((server :initarg :server
	   :initform nil
	   :accessor platform-server)
   (hostname :initarg :hostname
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

(defun build-platform (&key server hostname port title description users number-users max-users timestamp lease)
  (make-instance 'platform
		 :server server
		 :hostname (if hostname hostname (machine-instance))
		 :port (if port port (get-local-port (car (server-socket *server*))))
		 :title (if title title (format nil "MUCLR on ~a" (machine-instance)))
		 :description (if description description "Platform Administrator Should Add a Description!")
		 :users (if users users nil)
		 :number-users (if number-users number-users 0)
		 :max-users (if max-users max-users 19)
		 :timestamp (if timestamp timestamp (local-time:now))
		 :lease (if lease lease nil)))

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

(defmacro output-and-echo (destination control-string &rest format-arguments)
  `(let ((stream (make-broadcast-stream ,destination *standard-output*)))
     (format stream ,control-string ,format-arguments)
     (force-output stream)))

(defun force-output-to-connection (string con &optional (local-echo t)(terpri-before-p t)(terpri-after-p t))
  (unless (thread-alive-p (connection-thread con))
    (return-from force-output-to-connection "connection specified is of null state!"))
  (let* ((stream (connection-stream con))
	 (hostname (connection-hostname con))
	 (port (connection-port con))
	 (username (connection-username con))
	 (broadcast (when local-echo (make-broadcast-stream stream *standard-output*))))
    (when broadcast (setq stream broadcast))
    (when terpri-before-p (terpri stream) (force-output stream))
    (format stream "~a@~s:~s>> ~a" (if username username "STREAM") hostname port string)
    (force-output stream)
    (when terpri-after-p (terpri stream) (force-output stream))))

(defun &clean-connections ()
  (remove-if-not
   (lambda (y)
     (thread-alive-p (connection-thread y)))
   *clos-connections*))

(defun clean-connections ()
  (setf *clos-connections* (&clean-connections)))

;(defun api/greet (stream con)
;  (format stream "~%~80;~%

(defun api/valid-user (user pass)
  (let ((userpass (assoc user *users* :test #'string-equal)))
    (when (string-equal pass (cdr userpass)) t)))

(defun api/prompt (stream con prompt)
  (force-output-to-connection prompt con t t t)
  (read-line-no-cr stream))

(defun api/login (stream con)
  (if (connection-username con)
      (force-output-to-connection "you're already logged in!" con t t t)
      (let ((login (api/prompt stream con "login: "))
	    (pass (api/prompt stream con "password: ")))
	(if (api/valid-user login pass)
	    (progn
	      (setf (connection-username con) login)
	      (force-output-to-connection (format nil "muclr: you're logged in as ~a" login) con t t t))
	    (force-output-to-connection (format nil "muclr/error: ~a is an invalid login" login) con t t t)))))

(defun api/evaluate-p (string)
  (when (> (length string) 8)
    (when (string-equal (subseq string 0 8) "evaluate")
      (subseq string 9))))

(defun api/evaluate (con arg)
  (let ((result (eval (read-from-string arg))))
    (force-output-to-connection (format nil "evaluating ~a..." arg) con t t t)
    (force-output-to-connection result con t t t)))

(defun api/exit (con)
  (force-output-to-connection "exiting!" con t t t)
  (setf *clos-connections* (remove con *clos-connections*)))

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
    (format *standard-output* "~a@~s:~s>> ~a"
	    (if (connection-username con) (connection-username con) (string "STDIO"))
	    (connection-hostname con) (connection-port con) line)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)
    (when (string-equal line "/login")
      (request-handler stream con t))
    (let ((arg (api/evaluate-p line)))
      (when arg (api/evaluate con arg)))
    (when (string-equal line "exit")
      (api/exit con))
    (unless (or (string-equal line "quit") (string-equal line "") (string-equal line "exit"))
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

(defvar *server* nil)

(defclass server ()
  ((socket :initarg :socket
	   :initform nil
	   :accessor server-socket)
   (thread :initarg :thread
	   :initform nil
	   :accessor server-thread)
   (passphrase :initarg :passphrase
	       :initform nil
	       :accessor server-passphrase)))

(defun build-server (&key socket thread passphrase)
  (make-instance 'server
		 :socket socket
		 :thread thread
		 :passphrase passphrase))

(defun start-server (port &key passphrase force)
  (when *server*
    (unless force
      (return-from start-server "already running a server. use key :FORCE to override.")))
  (setf *server*
	(build-server :socket
		      (start-master-socket port)
		      :thread
		      (make-thread
		       (lambda ()
			 (run-server))
		       :name (format nil "port ~d muclr server" port))
		      :passphrase
		      (when passphrase passphrase))))

;(defun stop-server ()
;  (let ((server (shiftf *server* nil)))
;    (when server
;      (destroy-thread server))))

(defun stop-server ()
  (when *server*
    (socket-close (car (server-socket *server*)))
    (destroy-thread (server-thread *server*))
    (setf *server* nil)))
