;;; multi-emacs.el --- -*- lexical-binding: t; -*-

;;; Code:
(defvar multi-emacs-version "2.0")
(defvar config-home (or (getenv "XDG_CONFIG_HOME") "~/.config"))
(defvar multi-emacs-profiles-paths (list "~/.emacs.d/multi-profiles.el" (format "%s/%s" config-home "multi-emacs/profiles.el")))
(defvar multi-emacs-default-profile-paths (list "~/.emacs.d/.emacs-profile" (format "%s/%s" config-home "multi-emacs/profile")))
(defvar multi-emacs-profile-env-var "CHEMACS_PROFILE")

;; Copy `seq' library's `seq-filter' to avoid requiring it, see note above.
(defun multi-emacs--seq-filter (pred sequence)
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (mapcar (lambda (elt)
                            (if (funcall pred elt)
                                elt
                              exclude))
                          sequence))))

(defvar multi-emacs-profiles-path (or (car (multi-emacs--seq-filter #'file-exists-p multi-emacs-profiles-paths))
                                  (car multi-emacs-profiles-paths)))
(defvar multi-emacs-default-profile-path (or (car (multi-emacs--seq-filter #'file-exists-p multi-emacs-default-profile-paths))
                                         (car multi-emacs-default-profile-paths)))

(defun multi-emacs-handle-command-line (args &optional pos)
  "Handles either --with-profile profilename or --with-profile=profilename.
Removes them from the command-line-args variable, and returns the
selected profile (if any)."
  (when args
    (or pos (setq pos 0))
    (let ((s (split-string (car args) "=")))
      (cond ((equal (car args) "--with-profile")
             ;; remove 2 args and return the second of them
             (multi-emacs-remove-command-line-args pos 2)
             (cadr args))

            ((equal (car s) "--with-profile")
             ;; remove 1 arg and return the second part of it
             (multi-emacs-remove-command-line-args pos 1)
             (mapconcat 'identity (cdr s) "="))

            (t (multi-emacs-handle-command-line (cdr args) (1+ pos)))))))

(defun multi-emacs-remove-command-line-args (position number)
  "Removes NUMBER elements from the `command-line-args' variable, starting on position POSITION."
  (setf (nthcdr position command-line-args)
        (nthcdr (+ position number) command-line-args)))

(defvar multi-emacs--with-profile-value
  (let* ((value (multi-emacs-handle-command-line command-line-args))
         (read-value (read value)))
    (when value
      (if (listp read-value)
          read-value
        value))))

(defvar multi-emacs-literal-profile-provided
  (and multi-emacs--with-profile-value
       (listp multi-emacs--with-profile-value)))

(unless (or (file-exists-p multi-emacs-profiles-path)
            (and multi-emacs--with-profile-value
                 (listp multi-emacs--with-profile-value)))
  (error "[multi-emacs] %s does not exist." multi-emacs-profiles-path))

(defvar multi-emacs-default-profile-name
  (if (file-exists-p multi-emacs-default-profile-path)
      (with-temp-buffer
        (insert-file-contents multi-emacs-default-profile-path)
        (goto-char (point-min))
        ;; (buffer-string))
        (symbol-name (read (current-buffer))))
    "default"))


(defvar multi-emacs-profile-name
  (let ((env-profile-value (getenv multi-emacs-profile-env-var)))
    (cond ((and multi-emacs--with-profile-value
                (stringp multi-emacs--with-profile-value))
           multi-emacs--with-profile-value)
          (env-profile-value env-profile-value)
          (t multi-emacs-default-profile-name))))

(defvar multi-emacs-profiles
  (with-temp-buffer
    (insert-file-contents multi-emacs-profiles-path)
    (goto-char (point-min))
    (condition-case err
        (read (current-buffer))
      (error
       (error "Failed to parse %s: %s" multi-emacs-profiles-path (error-message-string err))))))

(defvar multi-emacs-profile
  (if (and multi-emacs--with-profile-value
           (listp multi-emacs--with-profile-value))
      multi-emacs--with-profile-value
      (cdr (assoc multi-emacs-profile-name multi-emacs-profiles))))

(unless multi-emacs-profile
  (error "No profile `%s' in %s" multi-emacs-profile-name multi-emacs-profiles-path))

(defun multi-emacs-profile-get (key &optional default)
  (alist-get key multi-emacs-profile default))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-emacs-directory (file-name-as-directory
                            (multi-emacs-profile-get 'user-emacs-directory)))

;; Allow multiple profiles to each run their server
;; use `emacsclient -s profile_name' to connect
(let ((name (multi-emacs-profile-get 'server-name)))
  (when name (setq server-name name)))

;; Set environment variables, these are visible to init-file with
;; getenv
(mapcar (lambda (env)
          (setenv (car env) (cdr env)))
        (multi-emacs-profile-get 'env))

;; Insinuate a nix elisp dependency bundle, if specified. In Emacs 27, the
;; startup.el looks through the load-path for any directory named site-lisp with
;; a subdirectory named elpa. If found, it activates them as packages. Starting
;; in Emacs 28, it instead relies on package-directory-list. It seems we can
;; distinguish these by whether package-directory-list is already bound in
;; early-init.
(let ((dir (multi-emacs-profile-get 'nix-elisp-bundle)))
  (when dir
    (if (boundp 'package-directory-list)
	(add-to-list 'package-directory-list
		     (expand-file-name "share/emacs/site-lisp/elpa" dir))
      (add-to-list 'load-path
		   (expand-file-name "share/emacs/site-lisp" dir)))
    (when (boundp 'native-comp-eln-load-path)
      (add-to-list 'native-comp-eln-load-path
		   (expand-file-name "share/emacs/native-lisp/" dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun multi-emacs-load-user-early-init ()
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
    (load early-init-file t t)))

(defun multi-emacs-load-user-init ()
  (when (multi-emacs-profile-get 'straight-p) (multi-emacs-load-straight))
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
    (load init-file t t)
    ;; Prevent customize from changing ~/.emacs (this file), but if
    ;; init.el has set a value for custom-file then don't touch it.
    (let ((multi-emacs-custom-file (multi-emacs-profile-get 'custom-file init-file)))
      (when (not custom-file)
        (setq custom-file multi-emacs-custom-file)
        (unless (equal custom-file init-file)
          (unless (file-exists-p custom-file)
            (with-temp-buffer (write-file custom-file)))
          (load custom-file))))))

(defun multi-emacs-load-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(provide 'multi-emacs)
