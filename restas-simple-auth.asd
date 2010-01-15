;;;; restas-simple-auth.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem restas-simple-auth
  :depends-on (#:restas #:closure-template #:ironclad)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "storage" :depends-on ("packages"))
                                     (:file "cookie" :depends-on ("storage"))
                                     (:file "init" :depends-on ("storage" "cookie"))
                                     (:file "sendmail" :depends-on ("packages"))
                                     (:file "simple-auth" :depends-on ("cookie" "sendmail"))))))