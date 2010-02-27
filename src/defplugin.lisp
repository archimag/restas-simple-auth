;;;; defplugin.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:restas.simple-auth
  (:use #:cl #:restas.optional #:iter)
  (:export #:*reCAPTCHA.publick-key*
           #:*reCAPTCHA.privake-key*
           #:*reCAPTCHA.theme*
           #:*sendmail*
           #:*noreply-email*
           #:*re-email-check* 
           #:*finalize-page*
           #:*cookie-auth-name*
           #:*cookie-cipher-key*
           #:*storage*
           #:*host*

           #:storage-check-user-password
           #:storage-email-exist-p
           #:storage-user-exist-p
           #:storage-create-invite
           #:storage-invite-exist-p
           #:storage-create-account
           #:storage-create-forgot-mark
           #:storage-forgot-mark-exist-p
           #:storage-change-password))

(in-package #:restas.simple-auth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load templates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((basepath (merge-pathnames "src/templates/"
                                 (asdf:component-pathname (asdf:find-system '#:restas-simple-auth)))))
  (iter (for tmpl in '("login" "register" "forgot"))
        (closure-template:compile-template :common-lisp-backend
                                           (merge-pathnames (format nil "~A.tmpl" tmpl)
                                                            basepath))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sendmail*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(defparameter *re-email-check* 
  "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$")

(defparameter *reCAPTCHA.publick-key* "6LdZjAcAAAAAAGh_MzHcHfJWp6rpI0XUNghGQB1f")

(defparameter *reCAPTCHA.privake-key* "6LdZjAcAAAAAAKJ2GPWTHPh1H1Foc0kyfbwgrFgO")

(defparameter *reCAPTCHA.theme* nil)

(defparameter *noreply-email* "noreply@example.com")

(defparameter *cookie-auth-name* "userauth")

(defparameter *cookie-cipher-key* (ironclad:ascii-string-to-byte-array "Specify the secure key"))

(defvar *user-auth-cipher*)

(defparameter *finalize-page* #'closure-template.standard:xhtml-strict-frame)

(defparameter *host* "example.com")

(defparameter *storage* nil)

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
