;;;; -*- Mode: Lisp; -*-

(in-package :cl-user)

(defpackage :muclr-client
  (:nicknames :client)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export #:connect
	   #:open-read-thread
	   #:force-to-stream
	   #:repl-like-thing
	   #:with-muclr-stream
	   :*socket*
	   :*stream*))

(in-package :muclr-client)

(defvar *socket* nil)
(defvar *stream* nil)

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

(defun connect (host port)
  (let* ((socket (usocket:socket-connect host port))
	 (stream (usocket:socket-stream socket)))
    (setf *socket* socket)
    (setf *stream* stream)))

(defun force-to-stream (string stream)
  (format stream "~a" string)
  (terpri stream)
  (force-output stream))

(defun open-read-thread (stream)
  (bt:make-thread
   (lambda ()
     (loop for line = (read-line-no-cr stream)
	  when line do
	  (format *standard-output* line)
	  (terpri *standard-output*)
	  (force-output *standard-output*)))))

(defun open-read-thread2 (stream)
  (bt:make-thread
   (lambda ()
     (loop for line = (read stream)
	  when line do
	  (format *standard-output* line)
	  (terpri *standard-output*)
	  (force-output *standard-output*)))))

(defun repl-like-thing (&optional stream loop-p)
  (unless stream (setq stream *stream*))
  (unless loop-p (open-read-thread stream))
  (let ((line (read-line-no-cr *standard-input*)))
    (force-to-stream line stream)
    (unless (or (string-equal line "quit") (string-equal line "exit"))
      (repl-like-thing stream t))))

(defun with-muclr-stream (&key stream api arg)
  (format stream "~a~C~a~C" api #\Space arg #\Newline))
