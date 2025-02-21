;;; early-init.el --- -*- lexical-binding: t; -*-

(require 'multi-emacs
         (expand-file-name "multi-emacs.el"
                           (file-name-directory
                            (file-truename load-file-name))))
(multi-emacs-load-user-early-init)
