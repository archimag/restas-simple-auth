;;;; simple-auth.lisp
;;;;
;;;; This file is part of the restas-simple-auth library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.simple-auth)

(defun finalize-page (content title)
  (funcall *finalize-page*
           (list :title title :body content)))

(defun logged-on-p ()
  (compute-user-login-name))

(defun not-logged-on-p ()
  (not (logged-on-p)))

(defun password-cache (password)
  (calc-md5-sum password))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; login
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route login ("login")
  (finalize-page (restas.simple-auth.view:login `(:forgot-href ,(genurl 'forgot)))
                 "Вход"))

(define-route login/post ("login"
                          :method :post
                          :requirement #'not-logged-on-p)
  (let ((name (hunchentoot:post-parameter "name"))
        (password-md5 (password-cache (hunchentoot:post-parameter "password")))
        (done (hunchentoot:get-parameter "done")))
    (if (check-user-password name password-md5)
        (progn
          (run-login name password-md5)
          (restas:redirect (if done
                               (hunchentoot:url-decode done)
                               "/")))
        (restas:redirect 'login))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; logout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route logout ("logout" :requirement #'logged-on-p)
  (run-logout)
  (restas:redirect (or (hunchentoot:header-in :referer hunchentoot:*request*)
                       'login)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route register ("register" :requirement #'not-logged-on-p)
  (finalize-page (restas.simple-auth.view:register-form nil)
                 "Регистрация"))

(defun form-field-value (field)
  (hunchentoot:post-parameter field))

(defun form-field-empty-p (field)
  (string= (form-field-value field)
           ""))

(defun check-register-form ()
  (let ((bads nil))
    (flet ((form-error-message (field message)
             (push message bads)
             (push field bads)))
      (cond
        ((form-field-empty-p "name")
         (form-error-message :bad-name "empty"))
        ((user-exist-p (form-field-value "name"))
         (form-error-message :bad-name "exist")))
      
      (cond
        ((form-field-empty-p "email") (form-error-message :bad-email "empty"))
        ((not (ppcre:scan *re-email-check*
                          (string-downcase (form-field-value "email"))))
              (form-error-message :bad-email
                                  "bad"))
        ((email-exist-p (form-field-value "email"))
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
  (let ((form-bads (check-register-form))
        (login (form-field-value "name"))
        (email (form-field-value "email"))
        (password (form-field-value "password")))
    (finalize-page (if form-bads
                       (restas.simple-auth.view:register-form (list* :name login
                                                                     :email email
                                                                     :password password
                                                                     :re-password (form-field-value "re-password")
                                                                     form-bads))
                       (let ((invite (create-invite login email (password-cache password)))
                             (to (list email)))
                         (send-mail to
                                    (restas.simple-auth.view:confirmation-mail
                                     (list :to to
                                           :noreply-mail *noreply-email*
                                           :subject (prepare-subject "Потверждение регистрации")
                                           :host "lisper.ru"
                                           :link (genurl 'accept-invitation :invite invite))))
                         (restas.simple-auth.view:register-send-mail nil)))
                   "Регистрация")))

(defun accept-invitation-form ()
  (finalize-page (restas.simple-auth.view:confirmation (list :recaptcha-pubkey *reCAPTCHA.publick-key*
                                                                 :theme *reCAPTCHA.theme*))
                     "Потверждение регистрации"))

(define-route accept-invitation ("register/confirmation/:(invite)" :requirement #'not-logged-on-p)
  (if (invite-exist-p invite)
      (accept-invitation-form)
      hunchentoot:+HTTP-NOT-FOUND+))

(define-route accept-invitation/post ("register/confirmation/:(invite)"
                                      :method :post
                                      :requirement #'not-logged-on-p)
  (if (invite-exist-p invite)
      (if (cl-recaptcha:verify-captcha (hunchentoot:post-parameter "recaptcha_challenge_field")
                                        (hunchentoot:post-parameter "recaptcha_response_field")
                                        (hunchentoot:real-remote-addr)
                                        :private-key *reCAPTCHA.privake-key*)
          (let ((account (create-account invite)))
            (run-login (first account)
                       (third account))
            (finalize-page (restas.simple-auth.view:success-registration nil)
                           "Регистрация завершена"))
          (accept-invitation-form))
      hunchentoot:+HTTP-NOT-FOUND+))
        
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; forgot password
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route forgot ("forgot" :requirement #'not-logged-on-p)
  (finalize-page (restas.simple-auth.view:forgot nil)
                 "Восстановление пароля"))

(define-route forgot/post ("forgot"
                           :method :post
                           :requirement #'not-logged-on-p)
  (let ((email-or-login (hunchentoot:post-parameter "email-or-login")))
    (if (or (not email-or-login)
            (string= email-or-login ""))
        (restas:redirect 'forgot)
        (multiple-value-bind (mark login email) (create-forgot-mark email-or-login)
          (declare (ignore login))
          (if mark
              (progn
                (send-mail (list email)
                           (restas.simple-auth.view:forgot-mail (list :to (list email)
                                                                      :noreply-mail *noreply-email*
                                                                      :subject (prepare-subject "Восстановление пароля")
                                                                      :link (restas:genurl-with-host 'reset-password :mark mark))))
                (finalize-page (restas.simple-auth.view:forgot-send-mail nil)
                               "Восстановление пароля"))
              (finalize-page (restas.simple-auth.view:forgot (list :bad t))
                             "Восстановление пароля"))))))

(define-route reset-password ("reset-password/:(mark)"
                              :requirement #'not-logged-on-p)
  (if (forgot-mark-exist-p mark)
      (finalize-page (restas.simple-auth.view:reset-password-form nil)
                     "Изменение пароля")
      hunchentoot:+HTTP-NOT-FOUND+))