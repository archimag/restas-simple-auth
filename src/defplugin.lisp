;;;; defplugin.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-plugin #:restas.simple-auth
  (:use #:cl #:restas.optional #:iter)
  (:export #:*finalize-page*
           #:*cookie-auth-name*
           #:*cookie-cipher-key*
           #:*storage*
           #:storage-check-user-password
           #:storage-check-email-exist
           #:storage-check-user-exist
           #:storage-create-forgot-mark
           #:storage-create-forgot-mark
           #:storage-forgot-mark-exist-p))

(in-package #:restas.simple-auth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iter (for tmpl in '("login" "register" "forgot"))
      (closure-template:compile-template :common-lisp-backend
                                         (merge-pathnames (print (format nil "src/templates/~A.tmpl" tmpl))
                                                          (asdf:component-pathname (asdf:find-system '#:restas-simple-auth)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prefences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *reCAPTCHA.publick-key* "6LdZjAcAAAAAAGh_MzHcHfJWp6rpI0XUNghGQB1f")

(defparameter *reCAPTCHA.privake-key* "6LdZjAcAAAAAAKJ2GPWTHPh1H1Foc0kyfbwgrFgO")

(defparameter *noreply-email* "noreply@example.com")

(defparameter *storage* nil)

(defparameter *sendmail*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(defparameter *cookie-auth-name* "userauth")

(defparameter *cookie-cipher-key* (ironclad:ascii-string-to-byte-array "Specify the secure key"))

(defvar *user-auth-cipher*)

(defparameter *finalize-page* #'closure-template.standard:xhtml-strict-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                          
(restas:define-initialization (context)
  (restas:context-add-variable context
                               '*user-auth-cipher*
                               (ironclad:make-cipher :blowfish 
                                                     :mode :ecb
                                                     :key (restas:context-symbol-value context
                                                                                       '*cookie-cipher-key*))))
