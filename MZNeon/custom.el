;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:
(setq mzneon-icon t)  
(setq mzneon-prettify-symbols-alist t)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq mzneon-prettify-org-symbols-alist t)  ; Alist of symbol prettifications for `org-mode'
;; Fonts
(defun mzneon-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (set-face-attribute 'default nil
			:font "Fira Code"
			:height (cond (sys/macp 130)
                                        (sys/win32p 110)
                                        (t 100))
			:weight 'light)
   (message "font set is run") 
    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
             return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             return (set-fontset-font t
                                      (if (< emacs-major-version 28)'symbol 'emoji)
                                      (font-spec :family font) nil 'prepend))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("LXGW Neo Xihei" "WenQuanYi Micro Hei Mono" "LXGW WenKai Screen"
                           "LXGW WenKai Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t 'han (font-spec :family font))))))

(mzneon-setup-fonts)
(add-hook 'window-setup-hook #'mzneon-setup-fonts)
(add-hook 'server-after-make-frame-hook #'mzneon-setup-fonts)
