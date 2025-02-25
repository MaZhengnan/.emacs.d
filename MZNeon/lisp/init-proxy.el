;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;; MZNeon Emacs - A Fancy and Fast Emacs Configuration.
;;; Code:


;; 设置 HTTP/HTTPS 代理
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
        ("http" . "proxy.com:8080")
        ("https" . "proxy.com:8080")))
;; 设置 SOCKS 代理
(require 'socks)
(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1080 5))
(defvar default-proxy "127.0.0.1:1080")

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,default-proxy)
          ("https" . ,default-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)"))))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks)
  (setq socks-server `("Default server" ,(car (split-string default-proxy ":")) ,(string-to-number (cadr (split-string default-proxy ":"))) 5)))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (eq url-gateway-method 'socks)
      (proxy-socks-disable)
    (proxy-socks-enable)))

;; 默认开启 HTTP 和 SOCKS 代理
;;(proxy-http-enable)
;;(proxy-socks-enable)
(provide 'init-proxy)
