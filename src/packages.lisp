;;;; packages.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(restas:define-plugin #:restas.simple-auth
  (:use #:cl #:restas.optional)
  (:export #:*finalize-page*
           #:*cookie-auth-name*
           #:*cookie-cipher-key*
           #:*storage*
           #:storage-check-user-password
           #:storage-check-email-exist
           #:storage-check-user-exist
           #:storage-create-forgot-mark
           #:storage-create-forgot-mark))

(in-package #:restas.simple-auth)

(defparameter *reCAPTCHA.publick-key* "6LdZjAcAAAAAAGh_MzHcHfJWp6rpI0XUNghGQB1f")

(defparameter *reCAPTCHA.privake-key* "6LdZjAcAAAAAAKJ2GPWTHPh1H1Foc0kyfbwgrFgO")

(defparameter *noreply-email* "noreply@example.com")

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/simple-auth.tmpl"
                                                    (asdf:component-pathname (asdf:find-system '#:restas-simple-auth))))

