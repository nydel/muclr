;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; $Id: client.lisp 110101 2014-12-23 02:27:00 PDT nydel $
;;;; $URL https://github.com/nydel/muclr/blob/master/src/client.lisp $
;;;; $HOMEPAGE http://www.muclr.org $

(in-package :cl-user)

(defpackage :muclr-client
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :muclr-client-test))

(in-package :muclr-client)

(defun muclr-client-test ()
  (format t "~%just a test function!~%"))

(defun send-to-platform (string host port)
  (let* ((socket (socket-connect host port :local-port 0))
	 (stream (socket-stream socket)))
    (write-line string stream)
    (force-output stream)
    (let ((result (read-line stream)))
      (write-line "quit" stream)
      (force-output stream)
      (close stream)
      (socket-close socket)
      result)))
