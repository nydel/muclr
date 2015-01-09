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
