;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:


(eval-when-compile
  (require 'init-custom))

(require 'org)

(defun mzneon-browse-url (url)
  "Open URL using a configurable method.
See `browse-url' for more details."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: ")))
  (if (and (featurep 'xwidget-internal) (display-graphic-p))
      (mzneon-webkit-browse-url url t)
    (browse-url url)))

(defun mzneon-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.
  POP-BUFFER specifies whether to pop to the buffer.
  NEW-SESSION specifies whether to create a new xwidget-webkit session.
  Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: ")))
  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

(defun mzn/org-font-setup ()
  (interactive)
  (variable-pitch-mode 1)
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.4)
                  (org-level-3 . 1.3)
                  (org-level-4 . 1.2)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)
		    )))
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))


;; Org mode use variable-petch-mode
(add-hook 'org-mode-hook 'mzn/org-font-setup)

;; Display images after executing org-babel
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)


(add-hook 'org-mode-hook
          (lambda ()
            "Beautify org symbols."
            (when mzneon-prettify-org-symbols-alist
              (if prettify-symbols-alist
                  (push mzneon-prettify-org-symbols-alist prettify-symbols-alist)
                (setq prettify-symbols-alist mzneon-prettify-org-symbols-alist)))
            (prettify-symbols-mode 1)))

;; (add-hook 'org-indent-mode-hook
;;           (lambda ()
;;             (diminish 'org-indent-mode)
;;             ;; HACK: Prevent text moving around while using brackets
;;             ;; @see https://github.com/seagle0128/.emacs.d/issues/88
;;             (make-variable-buffer-local 'show-paren-mode)
;;             (setq show-paren-mode nil)))

;; ;; To speed up startup, don't put to init section
(setq org-modules nil                 ; Faster loading
      org-directory mzneon-org-directory
      org-capture-templates
      `(("i" "Idea" entry (file ,(concat org-directory "/idea.org"))
         "*  %^{Title} %?\n%U\n%a\n")
        ("t" "Todo" entry (file ,(concat org-directory "/gtd.org"))
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("n" "Note" entry (file ,(concat org-directory "/note.org"))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+olp+datetree
                              ,(concat org-directory "/journal.org"))
         "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("b" "Book" entry (file+olp+datetree
                           ,(concat org-directory "/book.org"))
	     "* Topic: %^{Description}  %^g %? Added: %U"))

      org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
        (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
      org-todo-keyword-faces '(("HANGUP" . warning)
                               ("❓" . warning))
      org-priority-faces '((?A . error)
                           (?B . warning)
                           (?C . success))

      ;; Agenda styling
      org-agenda-files (list mzneon-org-directory)
      org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────"

      org-tags-column -80
      org-log-done 'time
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-ellipsis " ⤵"
      org-pretty-entities nil
      org-hide-emphasis-markers t
      ;; Babel
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
;; Add new template
(add-to-list 'org-structure-template-alist '("n" . "note"))
;; Add md/gfm backends
(add-to-list 'org-export-backends 'md)


;; Org-rich-yank, toc, preview html.
(use-package org-rich-yank
    :after org
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-preview-html
    :after org
    :diminish
    :bind (:map org-mode-map
                ("C-c C-h" . org-preview-html-mode))
    :init (when (and (featurep 'xwidget-internal) (display-graphic-p))
            (setq org-preview-html-viewer 'xwidget)))

;; Making the file in the middle of the frame.
(defun org-mode-visual-fill ()
    (setq visual-fill-column-width  (if sys/win32p 110 90))
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :after org
    :hook (org-mode . org-mode-visual-fill))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")  ; 自定义标题符号
        org-superstar-item-bullet "■"  ; 自定义项目符号
        org-superstar-remove-leading-stars t))  ; 移除标题中的星号（*）

;;Quickly create a code block in org file.

;; | Typing the below + TAB | Expands to ...                          |
;; |------------------------+-----------------------------------------|
;; | <a                     | '#+BEGIN_EXPORT ascii' … '#+END_EXPORT  |
;; | <c                     | '#+BEGIN_CENTER' … '#+END_CENTER'       |
;; | <C                     | '#+BEGIN_COMMENT' … '#+END_COMMENT'     |
;; | <e                     | '#+BEGIN_EXAMPLE' … '#+END_EXAMPLE'     |
;; | <E                     | '#+BEGIN_EXPORT' … '#+END_EXPORT'       |
;; | <h                     | '#+BEGIN_EXPORT html' … '#+END_EXPORT'  |
;; | <l                     | '#+BEGIN_EXPORT latex' … '#+END_EXPORT' |
;; | <q                     | '#+BEGIN_QUOTE' … '#+END_QUOTE'         |
;; | <s                     | '#+BEGIN_SRC' … '#+END_SRC'             |
;; | <v                     | '#+BEGIN_VERSE' … '#+END_VERSE'         |

(require 'org-tempo)


;; Roam
(use-package org-roam
  :diminish
  :functions mzneon-browse-url
  :defines org-roam-graph-viewer
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-directory (file-truename mzneon-org-directory)
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        org-roam-graph-viewer #'mzneon-browse-url)
  :config
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory))
  (add-to-list 'org-agenda-files (format "%s/%s" org-roam-directory "roam"))

  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  :bind ("C-c n u" . org-roam-ui-mode)
  :init (setq org-roam-ui-browser-function #'mzneon-browse-url))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
