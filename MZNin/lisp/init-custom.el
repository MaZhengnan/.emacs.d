;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma 

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; Define custom things.
;;
;;; Code:

(defgroup mzneon nil
  "MZNeon Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/MaZhengnan/.emacs.d"))

(defcustom mzneon-logo (expand-file-name
                         (if (display-graphic-p) "images/logo.png" "images/banner.txt")
                         user-emacs-directory)
  "Set MZNeon logo. nil means official logo."
  :group 'mzneon
  :type 'string)

(defcustom mzneon-org-directory (expand-file-name "~/org")
  "Set org directory."
  :group 'mzneon
  :type 'string)

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

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
