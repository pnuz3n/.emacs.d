
(setq inhibit-startup-message t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; No startup screen
(setq inhibit-splash-screen t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq lisp-dir
      (expand-file-name "lisp" user-emacs-directory))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t
  backup-by-copying t
)

(server-start)

(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path settings-dir)

(load "setup-el-get")

;; (load "/home/vagrant/.emacs.d/ladatut/robot-mode/robot-mode.el")


;; Conclude init by setting up specifics for the current user
(when (file-exists-p settings-dir)
  (mapc 'load (directory-files settings-dir nil "^[^#].*el$")))



;; No passwords show in shell
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)





;;; turn on syntax highlighting
(global-font-lock-mode 1)

;; Mustache mode
(add-to-list 'load-path "~/.emacs.d/ladatut")
(require 'mustache-mode)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun previous-line-insert-newline ()
  "Moves line up and creates empty line"
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  )

(defun next-line-insert-newline ()
  "Moves line up and creates empty line"
  (interactive)
  (end-of-line)
  (newline-and-indent)
  )

(custom-set-variables
 '(browse-url-browser-function (quote browse-url-chromium))
)

(setq line-move-visual nil)
(put 'scroll-left 'disabled nil)

;; (global-set-key (kbd "C-,") 'backward-paragraph)
;; (global-set-key (kbd "C-.") 'forward-paragraph)
;; (global-set-key  (kbd "C-`")    'shell-command)
;; (global-set-key  (kbd "M-`")    'shell-command-on-region)

;; These need some more tought sou they dont`t override org-mode keys
;; (global-set-key (kbd "C-c <return>") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "C-c c") 'mc/edit-lines)
;; (global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-c .") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c m") 'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-c f") 'iwb)

;; Moving from window to window using arrows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "M-n") 'next-line-insert-newline)
(global-set-key (kbd "M-p") 'previous-line-insert-newline)



(global-unset-key (kbd "C-<SPC>"))
(global-set-key (kbd "C-<SPC>") 'set-mark-command)



(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)

(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
