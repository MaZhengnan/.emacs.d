;;; init.el --- -*- lexical-binding: t; -*-

(require 'multi-emacs
         (expand-file-name "multi-emacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))
(multi-emacs-load-user-init)

;; this must be here to keep the package system happy, normally you do
;; `package-initialize' for real in your own init.el
;; (package-initialize)
