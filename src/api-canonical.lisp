;;;; -*- Mode: Lisp; -*-

(defpackage :muclr-server.api.canonical
  (:use :cl :bordeaux-threads :cl-ppcre :usocket)
  (:export #:muclr-api))

(in-package :muclr-server.api.canonical)

(defparameter *api-alist* '(("evaluate" . #'api/evaluate)
			    ("add" . #'+)
			    ("exit" . #'api/exit)))

(defun split-api-command-rest (string)
  (cl-ppcre:split "\\s+" string :limit 2))

(defun muclr-api (command &optional rest)
  (let* ((api-cell (assoc command *api-alist* :test #'string-equal))
	 (api-function (when api-cell (cdr api-cell))))))
    
