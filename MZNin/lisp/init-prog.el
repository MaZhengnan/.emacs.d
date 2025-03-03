;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; General programming configurations.
;;
;;; Code:

(eval-when-compile
  (require 'init-custom))

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist mzneon-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter support
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :init (setq treesit-auto-install 'prompt)))

;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish
  :config
  (when (childframe-workable-p)
    (use-package eldoc-box
      :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
      :custom
      (eldoc-box-lighter nil)
      (eldoc-box-only-multi-line t)
      (eldoc-box-clear-with-C-g t)
      :custom-face
      (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
      (eldoc-box-body ((t (:inherit tooltip))))
      :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
      :config
      ;; Prettify `eldoc-box' frame
      (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
            (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; Cross-referencing commands
(use-package xref
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

;; Misc. programming modes
(use-package csv-mode)
(unless emacs/>=29p
  (use-package csharp-mode))
(use-package cask-mode)
(use-package cmake-mode)
(use-package lua-mode)
(use-package powershell)
(use-package scala-mode)
(use-package v-mode)
(use-package vimrc-mode)
(use-package yaml-mode)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
