(setq local-pre-init-file "~/.emacs.d/local-pre-init.el")
(if (file-exists-p local-pre-init-file)
(load local-pre-init-file)
)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(let ((elements (split-string (getenv "PATH") ":")))
  (dolist (e elements)
    (add-to-list 'exec-path e)))

(add-to-list 'exec-path "~/bin")

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; read gpg-agent environment

(defun read-env-line (line)
  "read a env line and post to environment"
  (let ((key-value-pair (split-string line "=" t)))
    (setenv (car key-value-pair) (car (last key-value-pair))))
  )
(defvar gpg-agent-info-file)
(setq gpg-agent-info-file (concat (getenv "HOME") "/.gpg-agent-info"))

(when (and
       (eq system-type 'darwin)
       (file-exists-p gpg-agent-info-file))
      (with-temp-buffer
        (progn
          (insert-file-contents gpg-agent-info-file)
          (mapc 'read-env-line (split-string (buffer-string) "\n" t)))
        )
  )

(setq-default indent-tabs-mode nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t
  backup-by-copying t
)

(when (eq system-type 'darwin)
   (setq browse-url-browser-function 'browse-url-default-macosx-browser))

(server-start)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; No passwords show in shell
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(global-unset-key (kbd "C-q"))

(el-get-bundle org)

(setq org-directory "~/org")
(setq org-agenda-files (concat org-directory "/agenda"))
(setq org-default-notes-file (concat org-directory "/refile.org.gpg"))

(setq org-archive-location (concat org-directory "/archive.org.gpg::* From %s"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (makefile . t)
   (shell . t)
   (js . t)
   (plantuml . t)
   (emacs-lisp . t)
   (ditaa . t)
   ))

(setq org-log-into-drawer t)
(setq org-todo-keywords
'((sequence "IN(i!)" "SOMEDAY(s!)" "WAIT(w@/!)" "TODO(t!)" "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-log-done 'time)

(setq org-tags-exclude-from-inheritance '("PROJECT" "TARGET"))
(setq org-stuck-projects
           '("+PROJECT/-MAYBE-DONE" ("TODO")))

(setq org-capture-templates
      '(
        ("t"
         "Task"
         entry
         (file+headline org-default-notes-file "Tasks")
         "* IN %^{Title}\n  CREATED: %U\n  %i"
         :empty-lines 1)

        ("j" "Journal" entry (file+datetree "~/org/diary.org.gpg")
       "* %^{Title}\n\n%?\n%U" :clock-in t :clock-resume t)
  
      ))

(setq org-refile-targets '(
                           (org-agenda-files . (:tag . "PROJECT"))
                           (org-agenda-files . (:tag . "TARGET"))
                           ))

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

(el-get-bundle ace-jump-mode)

(add-hook 'comint-mode-hook
               (lambda ()
                 (define-key comint-mode-map (kbd "C-.") 'ace-jump-mode)
                 (define-key comint-mode-map (kbd "<C-return>") 'comint-accumulate)
                ))

               
;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-q q") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-.") 'ace-jump-mode)



;; Load CEDET
(el-get-bundle cedet)

;; CSV Mode
(el-get-bundle csv-mode)
(require 'csv-mode)

(el-get-bundle dockerfile-mode)
;;; Dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(el-get-bundle edit-server)
(edit-server-start)

(defun erl-exists () "Tests wether go is installed or not" 
  (= (call-process "which" nil nil nil "erl") 0)
)


(defun erl-setup () "Install erlang environment with el-get"
       (el-get-bundle erlang-mode)
)

(if (erl-exists) (erl-setup))

(el-get-bundle expand-region)
(require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

(defun system-has-go () "Tests wether go is installed or not" 
       (condition-case nil
           (progn
             (start-process "" nil "go")
             t
             )
         (error nil))
       )
(defun makeinfo-version () "Make info version"
       (with-temp-buffer
          (call-process "makeinfo" nil t nil "--version")
          (goto-char (point-min))
          (re-search-forward "[0-9]\\{1,2\\}\\(\\.[0-9]\\{1,2\\}\\)\\{1,2\\}")
          (let ((s (match-beginning 0)) (e (point)))
            (mapcar
             'string-to-number
             (split-string (buffer-substring s e) "\\.")))))




(defun setup-go () "Install go environment with el-get"
       (el-get-bundle go-mode)
       (el-get-bundle dash)
       
       ;; Require makeinfo which major version is 5 or more
       (if (< 4 (car (makeinfo-version)))
           (progn
           (el-get-bundle flycheck)   
           ;; go get github.com/dougm/goflymake
           (add-to-list 'load-path "~/src/github.com/dougm/goflymake")
           (require 'go-flycheck))
           ))

       

       ;; go get github.com/nsf/gocode
       (el-get-bundle go-autocomplete)
       (require 'go-autocomplete)

       (add-hook 'go-mode-hook 
                 (lambda ()
                         (add-hook 'before-save-hook 'gofmt-before-save))
                 )

(if (system-has-go) (setup-go))

;; Idea related shortcuts

(defun idea-open-file (s) "Opens file in idea"
       (interactive
        (list (idea-open-file (buffer-substring (region-beginning) (region-end)))))
       (start-process "" nil "idea" s)
       )

(el-get-bundle org-jira)
(setq org-jira-working-dir "~/org/jira")

(el-get-bundle multiple-cursors)

;; Allows changing port used to connect MySQL-database
;(setq sql-mysql-login-params (append sql-mysql-login-params '(port)))
;(setq sql-port 3306)

;; Donwload plantuml.jar if missing and use it.
(let ((plantuml-jar "~/.emacs.d/plantuml.jar"))
  (if (not (file-exists-p plantuml-jar))
      (progn
        (url-copy-file "http://downloads.sourceforge.net/project/plantuml/plantuml.jar?r=http%3A%2F%2Fplantuml.com%2Fdownload.html&ts=1441279540&use_mirror=netix" plantuml-jar)
        ))
  (setq org-plantuml-jar-path  (expand-file-name plantuml-jar))
)

;; Don`t confirm plant uml runs for conviency.
(lexical-let ((default-confirm org-confirm-babel-evaluate))
 (defun my-org-confirm-babel-evaluate (lang body)
           (if (string= lang "plantuml") nil default-confirm))
 (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
)

(el-get-bundle tramp)

(el-get-bundle yaml-mode)

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

(global-set-key (kbd "C-z")  'mode-line-other-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

 (setq ibuffer-saved-filter-groups
         (quote (("default"
                  ("dired" (mode . dired-mode))
                  ("org" (or
                          (mode . org-mode)
                          (mode . org-agenda-mode)                           
                          ))
                  ("emacs" (or
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")))
                  ("shell" (or
                           (mode . shell-mode)
                           (mode . term-mode)
                           ))))))

(el-get-install 'idea-darkula-theme)
(push (substitute-in-file-name "~/.emacs.d/el-get/idea-darkula-theme/") custom-theme-load-path)
(load-theme 'idea-darkula t)

(add-hook 'text-mode-hook 'variable-pitch-mode)

(defun my-adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (require 'cl-lib)
    (cl-adjoin element list)))

  (mapc
    (lambda (face)
      (set-face-attribute
       face nil
       :inherit
       (my-adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code 'org-block 'org-table 'org-meta-line))

(set-face-attribute 'variable-pitch nil :height 1.3 :family "Calibri")
(set-face-attribute 'fixed-pitch nil :height 0.8 :family "Consolas")

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-hide-leading-stars t)
(setq line-spacing 0.25)
(set-face-attribute 'org-tag nil :weight 'normal :height 0.8)
(set-face-attribute 'org-todo nil :weight 'normal :height 150)
(set-face-attribute 'org-priority nil :weight 'normal :height 100)
(set-face-attribute 'org-todo nil :weight 'normal :height 100)
(set-face-attribute 'org-done nil :weight 'normal :height 100)
(set-face-attribute 'org-special-keyword nil :height 90)
(set-face-attribute 'org-level-1 nil :height 1.3)
(set-face-attribute 'org-level-2 nil :height 1.2)
(set-face-attribute 'org-level-3 nil :height 1.1)

(setq local-init-file "~/.emacs.d/local-init.el")
(if (file-exists-p local-init-file)
(load local-init-file)
)
