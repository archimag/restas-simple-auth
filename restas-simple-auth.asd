;;;; restas-simple-auth.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem #:restas-simple-auth
  :defsystem-depends-on (#:closure-template)
  :depends-on (#:restas #:ironclad #:split-sequence)
  :pathname "src"
  :components ((:module "templates"
                        :components ((:closure-template "forgot")
                                     (:closure-template "login")
                                     (:closure-template "register")))
               (:file "defmodule" :depends-on ("templates"))
               (:file "storage" :depends-on ("defmodule"))
               (:file "cookie" :depends-on ("storage"))
               (:file "sendmail" :depends-on ("defmodule"))
               (:file "simple-auth" :depends-on ("cookie" "sendmail"))))
