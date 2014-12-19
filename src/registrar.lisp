(ql:quickload '(:bordeaux-threads
		:cl-ppcre
		:usocket))
(load "server.lisp")

(defpackage :multi-user-common-lisp-repl-registrar
  (:nicknames :muclr-registrar :muclr-reg :muclr-r)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :initialize))

(in-package :muclr-registrar)

(defun initialize ()
  (format t "okay..."))

(defun &hr-parse-platform (stream)
  (let ((line (muclr-s::read-line2 stream)))
    ;;then do something with the platform info
))

(defun handle-request (stream)
  (let ((line (muclr-s::read-line2 stream)))
    (format stream "STREAM>~a" line)
    (format *standard-output* "STDIO>~a~&" line)
    (terpri stream)
    (terpri *standard-output*)
    (force-output stream)
    (force-output *standard-output*)
    (unless (string-equal line "/platform")
      (&hr-parse-platform stream))
    (unless (or (string-equal line "quit") (string-equal line ""))
      (handle-request stream))))

(defun &hr-master (stream)
  (handle-request stream))


(defun run-server (socket)
  (loop (wait-for-input socket)
     (let ((stream (socket-stream (socket-accept socket))))
       (make-thread (lambda () (with-open-stream (stream stream)
				 (&hr-master stream)))
		    :name "muclr-registrar-request-handler-thread"))))

(defun start-server (port)
  (let ((socket (socket-listen *wildcard-host* port :reuse-address t)))
    (make-thread (lambda () (unwind-protect
				 (run-server socket)
			      (socket-close socket)))
		 :name "muclr-registrar-thread")))

(defun stop-servers ()
  (mapcar #'destroy-thread (remove-if-not
			    (lambda (y)
			      (string-equal
			       (slot-value y 'sb-thread::name) "muclr-registrar-thread"))
			    (all-threads))))
