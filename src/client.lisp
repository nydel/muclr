;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :muclr-client
;  (:nicknames :muclr-client)
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export :muclr-client-test))

(in-package :muclr-client)

(defun muclr-client-test ()
  (format t "~%just a test function!~%"))
