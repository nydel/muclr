(defpackage :muclr-credentials
  (:nicknames :credentials)
  (:use :cl)
  (:export :init-credentials
	   :add-user
	   :push-user
	   :valid-p
	   :user))

(in-package :muclr-credentials)

(defparameter *db-credentials-path* #P"data/credentials.db")
(defvar *db-credentials* nil)

(defun digest-hash (string &key digest)
  (setq string (string-downcase string))
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    (if digest digest :sha1)
    (ironclad:ascii-string-to-byte-array string))))

(defstruct (user (:type list))
  uname pwhash)

(defun build-user (&key uname password hashp)
  (make-user :uname (if hashp uname
			(digest-hash uname :digest :sha1))
	     :pwhash (if hashp password
			 (digest-hash password :digest :md5))))

(defun write-user-db ()
  (with-open-file (@db *db-credentials-path*
		       :direction :output
		       :if-exists :overwrite
		       :if-does-not-exist :create)
      (format @db "~{~a~^~&~}" *db-credentials*)
      (force-output @db)))

(defun &load-user-db ()
  (with-open-file (@db *db-credentials-path*
		       :direction :input
		       :if-does-not-exist :create)
    (loop for user = (read-line @db nil 'eof nil)
	 until (eq user 'eof)
	 collect user)))

(defun load-user-db ()
  (let ((users (&load-user-db)))
    (setf *db-credentials*
	  (mapcar
	   (lambda (y)
	     (list (string-downcase (car y))
		   (string-downcase (cadr y))))
		   users))))


(defun &push-user (&key uname password hashp)
  (push
   (build-user :uname uname :password password :hashp hashp)
   *db-credentials*))

(defun push-user (&key uname password hashp)
  (&push-user :uname uname :password password :hashp hashp)
  (write-user-db)
  (load-user-db))

(defun add-user (&key uname password)
  (push-user :uname uname :password password))

(defun valid-p (uname pass)
;  (when (stringp uname) (setq uname (read-from-string uname)))
  (setq uname (digest-hash (string uname) :digest :sha1))
  (let ((userp
	 (remove-if-not
	  (lambda (y)
	    (equal uname (string (car y))))
	  *db-credentials*)))
    (when (string-equal (digest-hash (string pass) :digest :md5)
			(string (cadar userp)))
      t)))

(defun init-credentials ()
  (load-user-db))
