;;;; -*- Mode: Lisp; -*-
;;;; $Id: muclr.asd 110101 2014-12-22 14:38:30 PDT nydel $
;;;; $URL https://github.com/nydel/muclr/blob/master/muclr.asd $
;;;; $HOMEPAGE http://www.muclr.org $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(defpackage :muclr-asd
  (:use :cl :asdf))

(in-package :muclr-asd)

(defsystem :muclr
  :serial t
  :version "1.10.101"
  :author "<nydel@muclr.org>"
  :license "LLGPL v3.0 Modified"
  :description "Multi-User Common Lisp REPL aka MUCLR is a project management
  system involving a server and end-users of different privilege levels.  Its
  goal is ultimately to incorporate an abstract social aspect, and its
  implementation, right into the Common Lisp REPL.  This will include a number of
  inherent prospects canonically yet will feature an expansion mechanism allowing
  users to modify existing and create new widgets whose purposes fit into the
  scope of MUCLR. Also featured will be siple chat and file sharing, both facets
  including encryption transparent to the user and configurable to suit the
  cryptographic tastes of the individual. A MUCLR instance, known as a Platform,
  will be an object created inside a daemonized Common Lisp - or possibly a
  binary - process allocated as a thread on the serving computer system. The
  object is essentially a shared Common Lisp REPL instance that can be
  manipulated and discussed by multiple users."
  :depends-on (:bordeaux-threads
	       :cl-ppcre
	       :drakma
	       :ironclad
	       :local-time
	       :usocket)
  :components ((:module "src"
		:serial t
		:components ((:file "server")
			     (:file "registrar")
			     (:file "client")
			     (:file "package")))))
