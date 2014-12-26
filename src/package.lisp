;;; -*- Mode: Lisp; -*-

(in-package :cl-user)

(defpackage :muclr
  (:use :cl)
  (:export :muclr-version))

(in-package :muclr)

(defun muclr-version ()
  (format t "~%a simple test function!~%"))
