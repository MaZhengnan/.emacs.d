;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Language Server Protocol (LSP) configurations.

;;; Code:

(use-package eglot
  :ensure nil
  :hook
  ((c-mode . eglot-ensure)          ;; C
   (python-mode . eglot-ensure)     ;; Python
   (go-mode . eglot-ensure)         ;; Go
   (js-mode . eglot-ensure)         ;; JavaScript
   (html-mode . eglot-ensure)       ;; HTML
   (css-mode . eglot-ensure)        ;; CSS
   (vue-mode . eglot-ensure))       ;; Vue.js
  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs
               '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs
               '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(css-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vls")))
)

(add-hook 'before-save-hook 'eglot-format-buffer)

(use-package consult-eglot
  :ensure t
  :defer t)

;; (use-package eglot-booster
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
