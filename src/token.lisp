(ql:quickload '(:bordeaux-threads :cl-ppcre :local-time :usocket))

(defpackage :muclr-subsystem-token
  (:nicknames :muclr-token)
  (:use :cl :bordeaux-threads :cl-ppcre :local-time :usocket)
  (:export :token-pieces
	   #:&build-token-pieces
	   #:build-token-pieces))

(in-package :muclr-subsystem-token)

(defclass token-pieces ()
  ((username :initarg :username
	     :initform nil
	     :accessor token-username)
   (hostname :initarg :hostname
	     :initform nil
	     :accessor token-hostname)
   (operating-system :initarg :operating-system
		     :initform nil
		     :accessor token-operating-system)
   (int-ip-addr :initarg :int-ip-addr
		:initform nil
		:accessor token-int-ip-addr)
   (ext-ip-addr :initarg :ext-ip-addr
		:initform nil
		:accessor token-ext-ip-addr)
   (passphrase :initarg :passphrase
	       :initform nil
	       :accessor token-passphrase)
   (timestamp :initarg :timestamp
	      :initform nil
	      :accessor token-timestamp)))

(defun &build-token-pieces (&key username hostname operating-system int-ip-addr ext-ip-addr passphrase timestamp)
  (make-instance 'token-pieces
		 :username username :hostname hostname
		 :operating-system operating-system
		 :int-ip-addr int-ip-addr :ext-ip-addr ext-ip-addr
		 :passphrase passphrase :timestamp timestamp))

(defun build-token-pieces (&key username hostname operating-system int-ip-addr ext-ip-addr passphrase timestamp)
  (&build-token-pieces :username (if username username (sb-posix::getenv "USER"))
		       :hostname (if hostname hostname (machine-instance))
		       :operating-system (if operating-system operating-system (software-type))
		       :int-ip-addr (if int-ip-addr int-ip-addr (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name "localhost")))
		       :ext-ip-addr (if ext-ip-addr ext-ip-addr nil)
		       :passphrase passphrase
		       :timestamp (if timestamp timestamp (local-time:clock-now t))))

;(defun build-token (pieces)

;(defclass token (token-pieces)
