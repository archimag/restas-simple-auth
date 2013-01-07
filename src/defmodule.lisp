;;;; defplugin.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:restas.simple-auth
  (:use #:cl #:iter)
  (:export #:*reCAPTCHA.publick-key*
           #:*reCAPTCHA.privake-key*
           #:*reCAPTCHA.theme*
           #:*sendmail*
           #:*noreply-email*
           #:*re-email-check* 
           #:*finalize-page*
           #:*cookie-auth-name*
           #:*cookie-cipher-key*
           #:*datastore*
           #:*host*

           ;; #:storage-check-user-password
           ;; #:storage-email-exist-p
           ;; #:storage-user-exist-p
           ;; #:storage-create-invite
           ;; #:storage-invite-exist-p
           ;; #:storage-create-account
           ;; #:storage-create-forgot-mark
           ;; #:storage-forgot-mark-exist-p
           ;; #:storage-change-password
           ))

(in-package #:restas.simple-auth)

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

;; (defparameter *storage* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod restas:initialize-module-instance ((module (eql #.*package*)) context)
  (restas:context-add-variable 
   context
   '*user-auth-cipher*
   (ironclad:make-cipher :blowfish 
                         :mode :ecb
                         :key (restas:context-symbol-value context
                                                           '*cookie-cipher-key*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; md5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calc-md5-sum (val)
  "Calc sha1 sum of the val (string)"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
                             (babel:string-to-octets val :encoding :utf-8))))

(defun calc-sha1-sum (val)
  "Calc sha1 sum of the val (string)"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1
                             (babel:string-to-octets val :encoding :utf-8))))
