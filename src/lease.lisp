(defpackage :muclr-lease
  (:nicknames :lease)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :lease
	   #:lease-created
	   #:lease-from-ip
	   #:lease-minutes
	   #:lease-renewed
	   #:lease-hash))

(in-package :muclr-lease)

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
