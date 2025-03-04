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
  ((c-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (html-mode . eglot-ensure)
   (css-mode . eglot-ensure)
   (web-mode . eglot-ensure)) ;; Vue 也用 web-mode
  :init
  (setq read-process-output-max (* 1024 1024) ; 1MB
        eglot-autoshutdown t
        eglot-events-buffer-size 0)
  :config
  (setq eglot-server-programs
        '(((c-mode c++-mode) . ("clangd"))
          (python-mode . ("pylsp"))
          (go-mode . ("gopls"))
          (js-mode . ("typescript-language-server" "--stdio"))
          (html-mode . ("vscode-html-language-server" "--stdio"))
          (css-mode . ("vscode-css-language-server" "--stdio"))
          (web-mode . ("vls"))))) ;; Vue 使用 web-mode


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
