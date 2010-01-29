;;;; restas-simple-auth.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-simple-auth
  :depends-on (#:restas #:closure-template #:ironclad)
  :components ((:module "src"
                        :components ((:file "defplugin")
                                     (:file "storage" :depends-on ("defplugin"))
                                     (:file "cookie" :depends-on ("storage"))
                                     (:file "sendmail" :depends-on ("defplugin"))
                                     (:file "simple-auth" :depends-on ("cookie" "sendmail"))))))