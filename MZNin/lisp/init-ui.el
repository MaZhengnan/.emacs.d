;;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; UI configurations.
;;
;;; Code:

(eval-when-compile
  (require 'init-base))

;; theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))
  ;; :init (load-theme 'doom-one t))

;; `doom-modeline'
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Display ugly ^L page breaks as tidy horizontal lines
;; Keybinding is `C-q+C-l'
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))

;; Use fixed pitch where it's sensible
(use-package mixed-pitch :diminish)

;; `nerd-icons'
(use-package nerd-icons
  :ensure t
  :init
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Ligatures support
(use-package composite
  :ensure nil
  :init (defvar composition-ligature-table (make-char-table nil))
  :hook (((prog-mode
           conf-mode nxml-mode markdown-mode help-mode
           shell-mode eshell-mode term-mode vterm-mode)
          . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
    ;; support ligatures, some toned down to prevent hang
  (let ((alist
         '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
           (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
           (36  . ".\\(?:\\(>\\)>?\\)")
           (37  . ".\\(?:\\(%\\)%?\\)")
           (38  . ".\\(?:\\(&\\)&?\\)")
           (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
           ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
           (43  . ".\\(?:\\([>]\\)>?\\)")
           ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
           (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
           ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
           (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
           (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
           ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
           (48  . ".\\(?:x[a-zA-Z]\\)")
           (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
           (59  . ".\\(?:\\(;\\);?\\)")
           (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
           (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
           (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
           (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
           (91  . ".\\(?:\\(|\\)[]|]?\\)")
          ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
           (94  . ".\\(?:\\(=\\)=?\\)")
           (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
           (119 . ".\\(?:\\(ww\\)w?\\)")
           (123 . ".\\(?:\\(|\\)[|}]?\\)")
           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
           (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table))


;; Fonts
(defun mzneon-setup-fonts ()
  "Setup fonts based on OS type."
  (when (display-graphic-p)
    ;; Set default font based on OS
    (set-face-attribute 'default nil
                        :family (cond
                                 (sys/macp "Fira Code")       ;; macOS
                                 (sys/win32p "Fira Code")  ;; Windows
                                 (gnu/linux "Fira Code")  ;; Linux
                                 (t "Monospace"))  ;; 其他系统默认
                        :height (cond
                                 (sys/macp 180)   ;; macOS
                                 (sys/win32p 120) ;; Windows
                                 (gnu/linux 110) ;; Linuxx
                                 (t 100)))       ;; 默认值
    ;; Set fixed-pitch font based on OS
    (set-face-attribute 'fixed-pitch nil
                        :family (cond
                                 (sys/macp "Fira Code")       ;; macOS
                                 (sys/win32p "Fira Code")  ;; Windows
                                 (gnu/linux "Fira Code")  ;; Linux
                                 (t "Monospace"))  ;; 其他系统默认
                        :height (cond
                                 (sys/macp 180)   ;; macOS
                                 (sys/win32p 120) ;; Windows
                                 (gnu/linux 110) ;; Linuxx
                                 (t 100)))       ;; 默认值

    ;; Set default font based on OS
    (set-face-attribute 'variable-pitch nil
                        :family (cond
                                 (sys/macp "Cantarell")       ;; macOS
                                 (sys/win32p "Cantarell")  ;; Windows
                                 (gnu/linux "Cantarell")  ;; Linux
                                 (t "Monospace"))  ;; 其他系统默认
                        :height (cond
                                 (sys/macp 180)   ;; macOS
                                 (sys/win32p 120) ;; Windows
                                 (gnu/linux 110) ;; Linuxx
                                 (t 100)))       ;; 默认值



    ;; Unicode symbols
    (set-fontset-font t 'symbol (font-spec :family
                                           (cond
                                            (sys/macp "Apple Symbols")
                                            (sys/win32p "Segoe UI Symbol")
                                            (gnu/linux "Symbola")
                                            (t "Symbol"))) nil 'prepend)

    ;; Emoji fonts
    (set-fontset-font t (if (< emacs-major-version 28) 'symbol 'emoji)
                      (font-spec :family
                                 (cond
                                  (sys/macp "Apple Color Emoji")
                                  (sys/win32p "Segoe UI Emoji")
                                  (gnu/linux "Noto Color Emoji")
                                  (t "Noto Color Emoji"))) nil 'prepend)

    ;; Chinese fonts
    (let ((chinese-font (cond
                         (sys/macp "PingFang SC")
                         (sys/win32p "Microsoft YaHei UI")
                         (gnu/linux "LXGW WenKai Mono")
                         (t "LXGW Neo Xihei"))))
      (when (find-font (font-spec :name chinese-font))
        (setq face-font-rescale-alist `((,chinese-font . 1.3)))
        (set-fontset-font t 'han (font-spec :family chinese-font))))))

(mzneon-setup-fonts)
(add-hook 'window-setup-hook #'mzneon-setup-fonts)
(add-hook 'server-after-make-frame-hook #'mzneon-setup-fonts)

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
