;;;; simple-auth.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.simple-auth)

(defparameter *finalize-page* #'closure-template.standard:xhtml-strict-frame)

(defun finalize-page (content title)
  (funcall *finalize-page*
           (list :title title
                 :body content)))

(defun logged-on-p ()
  (compute-user-login-name))

(defun not-logged-on-p ()
  (not (logged-on-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route login ("login")
  (finalize-page (restas.simple-auth.view:login nil)
                 "Вход"))

(define-route login/post ("login"
                          :method :post
                          :requirement #'not-logged-on-p)
  (let ((name (hunchentoot:post-parameter "name"))
        (password-md5 (calc-md5-sum (hunchentoot:post-parameter "password")))
        (done (hunchentoot:get-parameter "done")))
    (if (check-user-password *storage* name password-md5)
        (progn
          (run-login name password-md5)
          (restas:redirect (if done
                               (hunchentoot:url-decode done)
                               "/")))
        (restas:redirect 'login))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; logout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route logout ("logout"
                      :requirement #'logged-on-p)
  (run-logout)
  (restas:redirect (or (hunchentoot:header-in :referer hunchentoot:*request*)
                       'login)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route register ("register"
                        :requirement #'not-logged-on-p)
  (finalize-page (restas.simple-auth.view:register nil)
                 "Регистрация"))

(defun form-field-value (field)
  (hunchentoot:post-parameter field))

(defun form-field-empty-p (field)
  (string= (form-field-value field)
           ""))

(defparameter *re-email-check* 
  "^[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?$")


(defun check-register-form ()
  (let ((bads nil))
    (flet ((form-error-message (field message)
             (push message bads)
             (push field bads)))
      (cond
        ((form-field-empty-p "name")
         (form-error-message :bad-name "empty"))
        ((check-user-exist *storage* (form-field-value "name"))
         (form-error-message :bad-name "exist")))
      
      (cond
        ((form-field-empty-p "email") (form-error-message :bad-email "empty"))
        ((not (ppcre:scan *re-email-check*
                          (string-downcase (form-field-value "email"))))
              (form-error-message :bad-email
                                  "bad"))
        ((check-email-exist *storage* (form-field-value "email"))
         (form-error-message :bad-email
                             "exist")))

      (cond
        ((form-field-empty-p "password")
         (form-error-message :bad-password
                             "empty"))
        ((< (length (form-field-value "password")) 8)
         (form-error-message :bad-password
                             "short")))
      
      (unless (string= (form-field-value "password")
                       (form-field-value "re-password"))
        (form-error-message :bad-re-password
                            "bad")))
      bads))

(define-route register/post ("register"
                             :method :post
                             :requirement #'not-logged-on-p)
  (let ((form-bads (check-register-form)))
    (if form-bads
        (finalize-page
         (restas.simple-auth.view:register (list* :name (form-field-value "name")
                                                  :email (form-field-value "email")
                                                  :password (form-field-value "password")
                                                  :re-password (form-field-value "re-password")
                                                  form-bads))
         "Регистрация"))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; forgot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route forgot ("forgot"
                      :requirement #'not-logged-on-p)
  (finalize-page (restas.simple-auth.view:forgot nil)
                 "Восстановление пароля"))
  