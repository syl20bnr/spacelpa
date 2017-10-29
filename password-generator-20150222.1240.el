;;; password-generator.el --- Password generator for humans. Good, Bad, Phonetic passwords included.

;; Copyright (C) 2015 Zargener

;; Author: Zargener <zargener@gmail.com>
;; URL: http://github.com/zargener/emacs-password-genarator
;; Package-Version: 20150222.1240
;; Version: 1.01

;;; Commentary:
     
;; Generate a password and insert it in-place. Such functions provided:

;; password-generator-numeric  - generate PIN-code or any other numeric password.
;; password-generator-simple   - simple password for most websites.
;; password-generator-phonetic - easy to remember password.
;; password-generator-strong   - strong password and still suitable for most web sites with strange password requirements to used special chars.

;; Use C-u <length> password-generator-simple to specify length of generated password. This works with other functions too.

;; See full docs here: http://github.com/zargener/emacs-password-genarator

;;; Code:

;;;###autoload
(defun password-generator-random (max)
  "Feel free to rewrite this random. Just don't make it too slow."
  (let* 
      ((random-value (random max)))

    random-value))

;;;###autoload
(defun password-generator-get-random-string-char (string)
  "You pass here string. You get random character from it."
  (let* ((random-symbol 0)) 
    (progn
      (setq random-symbol (password-generator-random (length string)))
      (substring string random-symbol (+ 1 random-symbol)))))


;;;###autoload
(defun password-generator-generate-internal (symbols-for-pass pass-length)
  "Generates the password with given vocabulary and length"
  (let* 
      ((iter 0)
       (password ""))
    (while (< iter pass-length) 
      (progn
        (setq 
         password 
         (concat password (password-generator-get-random-string-char symbols-for-pass)))
         (setq iter (+ 1 iter))))
    password))


;;;###autoload
(defun password-generator-simple (&optional pre-len return)
  "Minimal viable password for most of web systems. It is not secure but allows to register."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 8))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (setq password (password-generator-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


;;;###autoload
(defun password-generator-strong (&optional pre-len return)
  "The best password you can get. Some symbols does not included to make you free from problems which occurs when your shell tries interpolate $, \\ and others."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 12))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ_@!.^%,&-"))
    (setq password (password-generator-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


;;;###autoload
(defun password-generator-paranoid (&optional pre-len return)
  "Good thing to use if you really care about bruteforce. Not all applications handle special characters presented in such password properly. Be ready to escape special characters if you will pass such password via ssh command or so."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 20))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+=/?,.><[]{}~"))
    (setq password (password-generator-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


;;;###autoload
(defun password-generator-numeric (&optional pre-len return)
  "Yeah, there are sill reasons to use numeric passwords like credit card PIN-code"
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg 4))
         (symbols-for-pass "0123456789"))
    (setq password (password-generator-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))


;;;###autoload
(defun password-generator-phonetic (&optional pre-len return)
  "It will be easy to remeber, because of fonetic, but not so secure..."
  (interactive)
  (let*
      ((password "")
       (letters-A "eyuioa")
       (letters-B "wrtpsdfghjkzxcvbnm")
       (letters-N "123456789")
       (pass-length (or pre-len current-prefix-arg 8))
       (iter 0) 
       (max-iter (+ 1 (/ pass-length 3)))
       (password ""))
    (while (< iter max-iter)
      (progn 
        (setq password (concat password
                               (password-generator-get-random-string-char letters-B)
                               (password-generator-get-random-string-char letters-A)
                               (password-generator-get-random-string-char letters-N)))
        (setq iter (+ 1 iter))))
    (setq password (substring password 0 pass-length))
    (cond ((equal nil return) (insert password)) (t password))))

(provide 'password-generator)

;;; password-generator.el  ends here

;;; password-generator.el ends here
