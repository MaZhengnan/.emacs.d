(add-to-list 'load-path "~/.emacs.d/scripts/")

(require 'elpaca-setup)  ;; The Elpaca Package Manager
(require 'buffer-move)   ;; Buffer-move for better window management
;;(require 'app-launchers) ;; Use emacs as a run launcher like dmenu (experimental)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-file-watchers nil
        lsp-log-io nil
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-enable-snippet nil)) 

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package cmake-ide
  :ensure t
  :after projectile
  :config
  (cmake-ide-setup))

(defun run-cmake ()
  "Run CMake to generate compile commands and configure the project."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (build-dir "build/"))                   
    (unless (file-directory-p (concat project-dir build-dir))
      (make-directory (concat project-dir build-dir) t))
    (compile (format "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -B%s -H%s" 
                     (concat project-dir build-dir) project-dir))))

(defun find-compilers ()
  "Find available compilers on the system and let the user choose one."
  (interactive)
  (let* ((cmd (cond ((eq system-type 'windows-nt)
                     "where gcc.exe; where cl.exe") 
                    (t
                     "which gcc; which clang"))) 
         (result (shell-command-to-string cmd))
         (compilers (split-string result "\n" t)))
    (ivy-read "Select compiler: " compilers
              :action (lambda (compiler)
                        (set-compiler compiler)))))

(defun set-compiler (compiler)
  "Set the chosen compiler for CMake."
  (let ((project-dir (projectile-project-root))
        (build-dir (concat (projectile-project-root) "build/")))
    (unless (file-exists-p build-dir)
      (make-directory build-dir t))
    (compile (format "cmake -DCMAKE_C_COMPILER=%s -DCMAKE_CXX_COMPILER=%s -B%s -H%s" 
                     compiler compiler build-dir project-dir))))

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (cond
   ((eq system-type 'windows-nt)
      (setq dashboard-startup-banner "~/.emacs.d/images/emacs-dash.png"))
   ((eq system-type 'darwin)
      (setq dashboard-startup-banner "~/.config/.emacs.d/images/emacs-dash.png"))
   ((eq system-type 'gnu/linux)
      (setq dashboard-startup-banner "~/.config/.emacs.d/images/emacs-dash.png")))

  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package diminish)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode))
  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))
  (use-package evil-tutor)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 110
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC") 

  (dt/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "TAB" '(comment-line :wk "Comment lines")
    "f r" '(counsel-recentf :wk "Find recent files"))

  (cond
   ((eq system-type 'windows-nt)
    (dt/leader-keys
      "f c" '(lambda () (interactive) (find-file "~/.emacs.d/config.org") :wk "Edit emacs config")))
   ((eq system-type 'darwin)
    (dt/leader-keys
      "f c" '(lambda () (interactive) (find-file "~/.config/.emacs.d/config.org") :wk "Edit emacs config")))
   ((eq system-type 'gnu/linux)
    (dt/leader-keys
      "f c" '(lambda () (interactive) (find-file "~/.config/.emacs.d/config.org") :wk "Edit emacs config"))))

  (dt/leader-keys
    "b" '(:ignore t :wk "Buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (dt/leader-keys
    "c" '(:ignore t :wk "Cmake")
    "c b" '(cmake-ide-compile :wk "Build project")
    "c c" '(run-cmake :wk "Configure and generate")
    "c s" '(find-compilers :wk "Search and choose compiler"))

  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))

  (dt/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e s" '(eshell :which-key "Eshell"))

  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '(reload-init-file :wk "Reload emacs config"))

  (dt/leader-keys
    "l" '(:ignore t :wk "Lsp")
    "l f" '(:ignore t :wk "Format")
    "l f f" '(lsp-format-buffer :wk "Format buffer")
    "l f r" '(lsp-format-region :wk "Format region")
    "l s" '(:ignore t :wk "Search")
    "l s a" '(xref-find-apropos :wk "Xref find apropos")
    "l s d" '(lsp-find-declaration :wk "Find declaration")
    "l s i" '(lsp-find-implementation :wk "Find implementation")
    "l s r" '(lsp-find-references :wk "Find references")
    "l p" '(:ignore t :wk "Peek find")
    "l p d" '(lsp-ui-peek-find-definitions :wk "UI peek find definitions")
    "l p i" '(lsp-ui-peek-find-implementation :wk "UI peek find implementation")
    "l p r" '(lsp-ui-peek-find-references :wk "UI peek find references")
    "l p w" '(lsp-ui-peek-find-workspace-symbol :wk "UI peek find workspace symbol")
    "l w" '(:ignore t :wk "Workspace")
    "l w a" '(lsp-workspace-folders-add :wk "Workspace folders add")
    "l w b" '(lsp-workspace-blocklist-remove :wk "Workspace blocklist remove")
    "l w l" '(lsp-workspace-show-log :wk "Workspace show log")
    "l w o" '(lsp-workspace-folders-open :wk "Workspace folders open")
    "l w t" '(lsp-workspace-restart :wk "Workspace restart")
    "l w s" '(lsp-workspace-shortdown :wk "Workspace shutdown")
    "l w f r" '(lsp-workspace-folders-remove :wk "Workspace folders remove")
    "l w r a" '(lsp-workspace-remove-all-folders :wk "Workspace remove all folders"))

  (dt/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (dt/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (dt/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (dt/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))

  (dt/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right"))
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(use-package counsel
   :after ivy
   :diminish
   :config (counsel-mode))

 (use-package ivy
   :bind
   ;; ivy-resume resumes the last Ivy-based completion.
   (("C-c C-r" . ivy-resume)
    ("C-x B" . ivy-switch-buffer-other-window))
   :diminish
   :custom
   (setq ivy-use-virtual-buffers t)
   (setq ivy-count-format "(%d/%d) ")
   (setq enable-recursive-minibuffers t)
   :config
   (ivy-mode))

 (use-package all-the-icons-ivy-rich
   :ensure t
   :init (all-the-icons-ivy-rich-mode 1))

 (use-package ivy-rich
   :after ivy
   :ensure t
   :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
   :custom
   (ivy-virtual-abbreviate 'full
    ivy-rich-switch-buffer-align-virtual-buffer t
    ivy-rich-path-style 'abbrev)
   :config
   (ivy-set-display-transformer 'ivy-switch-buffer
                                'ivy-rich-switch-buffer-transformer))

(use-package haskell-mode)
(use-package lua-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-autorefresh t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action)
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
            (lambda (_)
              (with-current-buffer (get-buffer neo-buffer-name)
                (setq truncate-lines t)
                (setq word-wrap nil)
                (make-local-variable 'auto-hscroll-mode)
                (setq auto-hscroll-mode nil)))))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))

(require 'org-tempo)

(use-package projectile
  :config
  (projectile-mode 1))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

  (use-package eshell-syntax-highlighting
    :after esh-mode
    :config
    (eshell-syntax-highlighting-global-mode +1))

;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
;; eshell-aliases-file -- sets an aliases file for the eshell.
  
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package sudo-edit
  :config
    (dt/leader-keys
      "fu" '(sudo-edit-find-file :wk "Sudo find file")
      "fU" '(sudo-edit :wk "Sudo edit file")))

(cond
     ((eq system-type 'windows-nt)
      (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/"))
     ((eq system-type 'darwin)
        (add-to-list 'custom-theme-load-path "~/.config/.emacs.d/themes/"))
     ((eq system-type 'gnu/linux)
        (add-to-list 'custom-theme-load-path "~/.config/.emacs.d/themes/")))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Sets the default theme to load!!! 
  (load-theme 'doom-one t)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;;(load-theme 'dtmacs t)

(add-to-list 'default-frame-alist '(alpha-background . 80)) ; For all new frames henceforth

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	which-key-sort-order #'which-key-key-order-alpha
      which-key-allow-imprecise-window-fit nil
	which-key-sort-uppercase-first nil
	which-key-add-column-padding 1
	which-key-max-display-columns nil
	which-key-min-display-lines 6
	which-key-side-window-slot -10
	which-key-side-window-max-height 0.25
	which-key-idle-delay 0.8
	which-key-max-description-length 25
	which-key-allow-imprecise-window-fit nil
	which-key-separator " → " ))
