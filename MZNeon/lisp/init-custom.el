;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;;; Commentary:
;;
;; Customization.
;;
;;; Code:

(defgroup mzneon nil
  "MZNeon Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/MaZhengnan/.emacs.d"))

(defcustom mzneon-logo (expand-file-name
                         (if (display-graphic-p) "image/logo.png" "image/banner.txt")
                         user-emacs-directory)
  "Set MZNeon logo. nil means official logo."
  :group 'mzneon
  :type 'string)

(defcustom mzneon-full-name user-full-name
  "Set user full name."
  :group 'mzneon
  :type 'string)

(defcustom mzneon-mail-address user-mail-address
  "Set user email address."
  :group 'mzneon
  :type 'string)

(defcustom mzneon-org-directory (expand-file-name "~/org")
  "Set org directory."
  :group 'mzneon
  :type 'string)

(defcustom mzneon-icon t
  "Display icons or not."
  :group 'mzneon
  :type 'boolean)

(defcustom mzneon-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-vibrant)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-palenight)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'mzneon
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom mzneon-auto-themes '(("8:00"  . doom-one-light)
				                 ("19:00" . doom-one))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if the option `calendar-latitude' and option `calendar-longitude' are set.
For example:
  \\='((:sunrise . doom-one-light)
    (:sunset  . doom-one))"
  :group 'mzneon
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defcustom mzneon-system-themes '((light . doom-one-light)
				                   (dark  . doom-one))
  "List of themes related the system appearance.

It's only available on macOS currently."
  :group 'mzneon
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))

(defcustom mzneon-theme 'default
  "The color theme."
  :group 'mzneon
  :type `(choice (const :tag "Auto" auto)
                 (const :tag "Random" random)
                 (const :tag "System" system)
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    mzneon-theme-alist)
                 symbol))

(defcustom mzneon-completion-style 'childframe
  "Completion display style."
  :group 'mzneon
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom mzneon-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'mzneon
  :type 'boolean)

(defcustom mzneon-lsp 'eglot
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'mzneon
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom mzneon-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'mzneon
  :type 'boolean)

(defcustom mzneon-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'mzneon
  :type 'boolean)

(defcustom mzneon-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'mzneon
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom mzneon-chinese-calendar nil
  "Enable Chinese calendar or not."
  :group 'mzneon
  :type 'boolean)

(defcustom mzneon-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'mzneon
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom mzneon-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:"       . ?📧)
    ("#+HEADERS"      . ?☰)
    ("#+OPTIONS:"     . ?⚙)
    ("#+SETUPFILE:"   . ?⚒)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"    . ?✎)
    ("#+END_SRC"      . ?□)
    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'mzneon
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
