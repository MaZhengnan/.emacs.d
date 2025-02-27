;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma 

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; General keybindings configurations.
;;
;;; Code:

;; Optionally use the `orderless' completion style.
(use-package vertico
  :init
  (vertico-mode)
  :bind 
  (:map minibuffer-local-map
        ("C-j" . next-line)
        ("C-k" . previous-line)
   :map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  :config
  ;;from https://github.com/tumashu/pyim
  (defun my-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))

  (advice-add 'orderless-regexp :around #'my-orderless-regexp))

(use-package vertico-posframe
  :after vertico
  :init
  (vertico-posframe-mode)
  (setq vertico-posframe-poshandler
        #'posframe-poshandler-frame-center  ;; 居中显示
        vertico-posframe-parameters
        '((left-fringe  . 8)
          (right-fringe . 8)
          (width        . 0.6)              ;; 设置宽度为屏幕宽度的 60%
          (min-width    . 60)               ;; 最小宽度 60 字符
          (max-width    . 100)              ;; 最大宽度 100 字符
          (height       . 0.4)              ;; 设置高度为屏幕高度的 40%
          (min-height   . 10)               ;; 最小高度 10 行
          (max-height   . 20))))               ;; 最大高度 20 行


(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("C-l" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section 
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)
   ("M-y" . consult-yank-pop)
   ("C-x b" . consult-buffer))
  :custom
  (consult-preview-key 'any) ;;预览键设置为任何按键，以便在搜索时保持当前缓冲区的位置
  (consult-async-min-input 2) ;; 设置异步搜索的最小输入字符数
  (consult-narrow-key "<") ;; 设置缩小范围的键为 "<"
  (consult-project-root-function
   (lambda ()
     (when-let (project (project-current))
       (car (project-roots project))))))

  (advice-add 'consult--buffer-action :after #'my/buffer-switch-advice)
  (advice-add 'find-file-at-point :after #'my/buffer-switch-advice)

  ;; https://emacs-china.org/t/xxx-thing-at-point/18047/18
  (defun consult-delete-default-contents()
    (remove-hook 'pre-command-hook 'consult-delete-default-contents)
    (cond ((member this-command '(self-insert-command))
           (delete-minibuffer-contents))
          (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))
  (consult-customize
   consult-line
   :initial (when-let ((string (thing-at-point 'word)))
              (add-hook 'pre-command-hook 'consult-delete-default-contents)
              (propertize string 'face 'shadow)))
  )

;;(use-package wgrep)

(use-package helpful
:bind
   ([remap describe-key]      . helpful-key)
   ([remap describe-command]  . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-function] . helpful-callable))

;; Auto completion
(use-package corfu
  :init
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

;;(use-package corfu-terminal
;;  :hook (global-corfu-mode . corfu-terminal-mode)))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
