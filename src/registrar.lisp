;;; -*- Mode: Lisp; -*-

;(ql:quickload '(:bordeaux-threads
;		:cl-ppcre
;		:usocket))

;(load "server.lisp")

(defpackage :muclr-registrar
  (:nicknames :registrar)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :lease
	   #:initialize))

(in-package :muclr-registrar)

(defun initialize ()
  (format t "okay..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun clean-line (line)
  (string-right-trim (string #\Return) line))

(defun clean-line2 (line)
  (regex-replace-all "\\r" line ""))

(defun clean-line3 (line)
  (subseq line 0 (- (length line) 1)))

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

(defun get-local-ip-address ()
  (clean-line3
   (drakma:http-request "http://muclr.org/.myip.shtml")))

;(defun string-ip-to-vector (string)
;  (Let ((list (split "\\." string)))
    

(defclass lease ()
  ((created :initarg :created
	    :initform (get-universal-time)
	    :accessor lease-created)
   (from-ip :initarg :from-ip
	    :initform nil
	    :accessor lease-from-ip)
   (minutes :initarg :minutes
	    :initform nil
	    :accessor lease-minutes)
   (renewed :initarg :renewed
	    :initform nil
	    :accessor lease-renewed)
   (hash :initarg :hash
	 :initform nil
	 :accessor lease-hash)))

(defun passphrase-p (string)
  (when (string-equal string "muclr") t))

(defun sha256-hash (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array string))))

(defun generate-lease-hash (lease passphrase)
  (let ((created (lease-created lease))
	(from-ip (lease-from-ip lease)))
    (sha256-hash (format nil "~a@~d:~a" from-ip created passphrase))))

(defun &build-lease (&key created from-ip hours)
  (make-instance 'lease
		 :created (if created created (get-universal-time))
		 :from-ip from-ip
		 :minutes (* hours 60)
		 :renewed (if created created (get-universal-time))
		 :hash nil))

(defun set-lease-hash (lease passphrase)
  (when (passphrase-p passphrase)
    (setf (lease-hash lease)
	  (generate-lease-hash lease passphrase))))

(defun build-lease (&key created from-ip hours passphrase)
  (let ((lease
	 (&build-lease :created created :from-ip from-ip :hours hours)))
    (set-lease-hash lease passphrase)
    lease))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *master-registrar-socket* nil)
(defvar *registrar-connections* nil)
(defvar *clos-registrar-connections* nil)

(defclass registrar-connection ()
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

(defun build-registrar-connection (&key socket hostname port stream thread timestamp)
  (make-instance 'connection :socket socket :hostname hostname :port port
                             :stream stream :thread thread :timestamp timestamp
                             :username nil))

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
   *clos-registrar-connections*))

(defun clean-connections ()
  (setf *clos-registrar-connections* (&clean-connections)))

(defun registrar-request-handler (stream &optional con)
  (let ((line (read-line-no-cr stream))
	(con (if con con
		 (car (remove-if-not
		       (lambda (y)
			 (equal (connection-stream y) stream)) *clos-registrar-connections*)))))
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
    (unless (or (string-equal line "quit") (string-equal line "") (string-equal line "exit"))
      (registrar-request-handler stream con))))
    

(defun handle-incoming-input (socket)
  (let* ((stream (socket-stream socket))
	 (con (build-registrar-connection :socket socket :stream stream :timestamp (get-universal-time)
					  :hostname (get-peer-address socket) :port (get-peer-port socket)
					  :thread nil)))
    (setf (connection-thread con)
	  (make-thread
	   (lambda ()
	     (with-open-stream (stream stream)
	       (registrar-request-handler stream))
	     :name (format nil "muclr incoming registrar stream at ~d" (get-universal-time)))))
    (push con *clos-registrar-connections*)))

(defun handle-incoming-connect (socket)
  (handle-incoming-input socket))

(defun run-registrar ()
  (loop
     (loop :for s :in (wait-for-input *registrar-connections* :ready-only t) :do
	(if (eq s *master-registrar-socket*)
	    (let ((new (socket-accept s)))
	      (setf *registrar-connections* (nconc *registrar-connections* `(,s)))
	      (handle-incoming-connect new))
	    (handle-incoming-input s)))))

(defun &start-master-registrar (port)
  (let ((socket (socket-listen *wildcard-host* port :reuse-address t)))
    socket))

(defun start-master-registrar (port)
  (setf *master-registrar-socket* (&start-master-registrar port))
  (setf *registrar-connections* (list *master-registrar-socket*)))


(defvar *registrar* nil)

(defclass registrar ()
  ((socket :initarg :socket
	   :initform nil
	   :accessor registrar-socket)
   (thread :initarg :thread
	   :initform nil
	   :accessor registrar-thread)))

(defun build-registrar (&key socket thread)
  (make-instance 'registrar
		 :socket socket
		 :thread thread))

(defun start-registrar (port)
  (setf *registrar*
	(build-registrar :socket
			 (start-master-registrar port)
			 :thread
			 (make-thread
			  (lambda ()
			    (run-registrar))
			  :name (format nil "port ~d muclr registrar" port)))))


;(defun api-server/lease-request ()
  

;(defun &hr-parse-platform (stream)
;  (let ((line (muclr-s::read-line2 stream)))
    ;;then do something with the platform info
;))

;(defun handle-request (stream)
;  (let ((line (muclr-s::read-line2 stream)))
;    (format stream "STREAM>~a" line)
;    (format *standard-output* "STDIO>~a~&" line)
;    (terpri stream)
;    (terpri *standard-output*)
;    (force-output stream)
;    (force-output *standard-output*)
;    (unless (string-equal line "/platform")
;      (&hr-parse-platform stream))
;    (unless (or (string-equal line "quit") (string-equal line ""))
;      (handle-request stream))))

;(defun &hr-master (stream)
;  (handle-request stream))


;(defun run-server (socket)
;  (loop (wait-for-input socket)
;     (let ((stream (socket-stream (socket-accept socket))))
;       (make-thread (lambda () (with-open-stream (stream stream)
;				 (&hr-master stream)))
;		    :name "muclr-registrar-request-handler-thread"))))

;(defun start-server (port)
;  (let ((socket (socket-listen *wildcard-host* port :reuse-address t)))
;    (make-thread (lambda () (unwind-protect
;				 (run-server socket)
;			      (socket-close socket)))
;		 :name "muclr-registrar-thread")))

;(defun stop-servers ()
;  (mapcar #'destroy-thread (remove-if-not
;			    (lambda (y)
;			      (string-equal
;			       (slot-value y 'sb-thread::name) "muclr-registrar-thread"))
;			    (all-threads))))
