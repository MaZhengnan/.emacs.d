;; Basic UI Configuration ------------------------------------------------------

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 130)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Font Configuration ----------------------------------------------------------

(set-face-attribute 'default nil :font "FiraMono Nerd Font" :height runemacs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "FiraMono Nerd Font" :height 130)

;; Set the variable pitch face
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140 :weight 'regular)

;;(load-theme 'wombat)
(column-number-mode)
(global-display-line-numbers-mode t)
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package ivy
  ;;:ensure t
  :demand t
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(use-package restart-emacs)

;; 绑定快捷键 C-c r
(global-set-key (kbd "C-c r") 'restart-emacs)

(defun open-init-file ()
  "Open Emacs Configuration file quickly."
  (interactive)
  (find-file "~/.emacs.d/init.el"))  ;; 用你实际的配置路径替换

;; 例如绑定到 F2 键
(global-set-key (kbd "<f2>") 'open-init-file)


;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-gruvbox))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package general
  :config
  (general-create-definer leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1)
  (setq evil-escape-key-sequence "jk")     ;; Define the key sequence
  (setq evil-escape-delay 0.3)  ;; 将0.3秒改为你需要的时间间隔
  evil-escape-unordered-key-sequence t) ;; Allow unordered key sequence if desired

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; 定义变量保存项目路径
(defvar my-projectile-paths nil
  "Project paths specific to each operating system.")

;; 创建函数来设置项目路径
(defun set-projectile-paths ()
  "Set Projectile project search paths based on the operating system."
  (setq my-projectile-paths
        (cond ((eq system-type 'windows-nt) '("L:/work/16_Persional_Project/"))
              ((eq system-type 'darwin) '("~/projects/" "~/work/")) ;; macOS
              ((eq system-type 'gnu/linux) '("~/projects/" "~/work/")))) ;; Linux
  (setq projectile-project-search-path my-projectile-paths))

;; 调用此函数来设置路径
(set-projectile-paths)

;; 安装和配置 Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode)
  ;; 调用配对快捷键
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; 使用 Ivy 作为补全系统
  (setq projectile-completion-system 'ivy)
  ;; 额外路径配置插入点
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  ;; 设置在切换项目后的默认动作为dired
  (setq projectile-switch-project-action #'projectile-dired)
)
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started

(use-package forge)


;; Org Mode Configuration ------------------------------------------------------
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))



(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "FiraMono Nerd Font" :weight 'regular :height (cdr face)))
      ;;(set-face-attribute (car face) nil :height (cdr face))) 
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))
