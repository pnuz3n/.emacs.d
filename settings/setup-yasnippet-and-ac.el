(el-get-bundle yasnippet)
(el-get-bundle auto-complete)
(el-get-bundle auto-complete-yasnippet)

(require 'yasnippet)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)

(ac-config-default)
;(global-set-key (kbd "C-<tab>")  'yas-expand)

(setq ac-source-yasnippet nil)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "<tab>")



;; (setq-default ac-sources
;;       '(
;;         ;; ac-source-semantic
;;         ac-source-yasnippet
;;         ac-source-abbrev
;;         ac-source-words-in-buffer
;;         ac-source-words-in-all-buffer
;;         ;; ac-source-imenu
;;         ac-source-files-in-current-dir
;;         ac-source-filename
;;         )
;;       )

(yas-global-mode 1)
(global-auto-complete-mode 1)

 (defadvice ac-fallback-command (around no-yasnippet-fallback activate)
      (let ((yas-fallback-behavior nil))
        ad-do-it))
