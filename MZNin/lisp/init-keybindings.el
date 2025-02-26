;; init-keybindings.el --- Initialize keybindings configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2025 Zhengnan Ma 

;; Author: Zhengnan Ma <mzn83644365@gmail.com>
;; URL: https://github.com/MaZhengnan/.emacs.d

;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;; General keybindings configurations.
;;
;;; Code:

;; `general'
(use-package general
  :config
  (general-evil-setup)
  
  ;; set up 'SPC' as the global leader key
  (general-create-definer mzn/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  
  ;; usually used keybindings
  (mzn/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))
  
  ;; buffers[b] 
  (mzn/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b b" '(ivy-switch-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers"))
  
  ;; bookmarks[B]
  (mzn/leader-keys
    "B" '(:ignore t :wk "Bookmarks")
    "B d" '(bookmark-delete :wk "Delete bookmark")
    "B l" '(list-bookmarks :wk "List bookmarks")
    "B m" '(bookmark-set :wk "Set bookmark")
    "B w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))
  
  ;; code[c] TODO

  
  ;;
  
)

(provide 'init-keybindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keybindings.el ends here

