(defpackage :muclr-credentials
  (:nicknames :credentials)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export #:init-credentials))

(in-package :muclr-credentials)

(defparameter *db-credentials-path* #P"data/credentials.db")
(defvar *db-credentials* nil)

(defclass user-credential ()
  ((uid :initarg :uid
	:initform nil
	:accessor user-credential-uid)
   (username :initarg :username
	     :initform nil
	     :accessor user-credential-username)
   (hash :initarg :hash
	 :initform nil
	 :accessor user-credential-hash)))

(defun split-user-credential (uc)
  (format t "~a,~a,~a"
	  (user-credential-uid uc)
	  (user-credential-username uc)
	  (user-credential-hash uc)))

(defun &load-credentials-db ()
  (with-open-file (@db *db-credentials-path* :direction :input)
    (loop for line = (read-line @db nil 'eof)
       until (equal line 'eof)
	 collect line)))

(defun load-credential-db ()
  (setf *db-credentials* (&load-credentials-db)))

(defun save-credentials-db ()
  (with-open-file (@db *db-credentials-path* :direction :output
		       :if-exists :rename :if-does-not-exist :create)
    (when *db-credentials*
      (loop for i in *db-credentials* do
	   (format @db "~{~a~}" (mapcar (lambda (y) (split-user-credentials y)) *db-credentials*))))))

(defun digest-hash (string &key digest)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    (if digest digest :sha1)
    (ironclad:ascii-string-to-byte-array string))))

(defun &build-user-credential (&key uid username pw)
  (make-instance 'user-credential
		 :uid (if uid uid (get-universal-time))
		 :username username
		 :hash (digest-hash pw :digest :md5)))

(defun build-user-credential (&key uid username pw)
  (push
   (&build-user-credential :uid uid
			   :username username
			   :pw pw)
   *db-credentials*))

(defun &rebuild-user-credential (string)
  (split "\\," string))

;(defun rebuild-user-credentials (string)
;  (let ((creds (&rebuild-user-credential string)))
;    (build-user-credential :uid (first creds)
;			   :username (second creds)
;			   :pw

(defun init-credentials ())
