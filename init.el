(setq inhibit-startup-message t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; No startup screen
(setq inhibit-splash-screen t)
(tool-bar-mode -1)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))


(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path settings-dir)

(load "setup-el-get")


;; Conclude init by setting up specifics for the current user
(when (file-exists-p settings-dir)
(mapc 'load (directory-files settings-dir nil "^[^#].*el$")))






;; No passwords show in shell
(add-hook 'comint-output-filter-functions
   'comint-watch-for-password-prompt)





;;; turn on syntax highlighting
(global-font-lock-mode 1)



(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-chromium))
 '(httpd-port 8808))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(global-set-key  (kbd "C-z")    'undo)
(global-set-key (kbd "C-,") 'backward-paragraph)
(global-set-key (kbd "C-.") 'forward-paragraph) 
(global-set-key  (kbd "C-w")    'kill-this-buffer)
(global-set-key  (kbd "C-§")    'shell-command)
(global-set-key  (kbd "M-§")    'shell-command-on-region)
(global-set-key  (kbd "M-<left>")  'pop-global-mark) ;; Navigate back


(global-set-key (kbd "s-<return>") 'set-rectangular-region-anchor)
(global-set-key (kbd "s-c") 'mc/edit-lines)
(global-set-key (kbd "s-a") 'mc/mark-all-like-this)
(global-set-key (kbd "s-.") 'mc/mark-next-like-this)
(global-set-key (kbd "s-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-m") 'mc/mark-more-like-this-extended)


(global-unset-key (kbd "C-<SPC>"))
(global-set-key (kbd "C-<SPC>") 'set-mark-command)

(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)

; Moving from window to window using arrows
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)
