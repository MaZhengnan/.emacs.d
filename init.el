(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

