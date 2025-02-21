(setq centaur-logo nil)                        ; Logo file or nil (official logo)
(setq centaur-full-name "user name")           ; User full name
(setq centaur-mail-address "user@email.com")   ; Email address
(setq centaur-proxy "127.0.0.1:1087")          ; HTTP/HTTPS proxy
(setq centaur-socks-proxy "127.0.0.1:1086")    ; SOCKS proxy
(setq centaur-server t)                        ; Enable `server-mode' or not: t or nil
(setq centaur-icon t)                          ; Display icons or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, bfsu, iscas, netease, sjtu, tencent, tuna or ustc
(setq centaur-theme 'auto)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
(setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
(setq centaur-dashboard nil)                   ; Display dashboard at startup or not: t or nil
(setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
(setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode python-mode markdown-mode)) ; Ignore format on save for some languages
(setq centaur-tree-sitter nil)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
(setq centaur-chinese-calendar nil)            ; Support Chinese calendar or not: t or nil
(setq centaur-player t)                        ; Enable players or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications. Nil to use font supports ligatures.
(setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
;; 使用代理
;;(setq url-proxy-services
;;      '(("http"     . "127.0.0.1:1080")
;;      ("https"     . "127.0.0.1:1080")))
;;(setq centaur-proxy "127.0.0.1:1080") 
;; 或者换源
;;(setq centaur-package-archives 'emacs-china)
(setq centaur-package-archives 'ustc)
(proxy-http-enable)
(enable-proxy)
(provide 'custom)