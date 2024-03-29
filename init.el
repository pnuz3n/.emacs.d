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
(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(when (eq system-type 'darwin)
(setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.gnupg/S.gpg-agent.ssh")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
(url-retrieve
 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (goto-char (point-max))
   (eval-print-last-sexp))))

;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(el-get-install 'idea-darkula-theme)
(push (substitute-in-file-name "~/.emacs.d/el-get/idea-darkula-theme/") custom-theme-load-path)
(add-hook 'after-init-hook (lambda () (load-theme 'idea-darkula t)))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)

(transient-mark-mode 0)

(put 'narrow-to-region 'disabled nil)

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
   (emacs-lisp  . t)
   (ditaa . t)
   ))

(setq org-log-into-drawer t)
(setq org-todo-keywords '((sequence  "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))

(setq org-capture-templates
      '(
        ("t" "Task" entry (file "")
          "* TODO %?\n:LOGBOOK:\n- Created %U\n:END:\n%i\n"
         :empty-lines 1)

        ("n" "Note" entry (file "")
         "* %? :muistiinpano:\n:LOGBOOK:\n- Created %U\n:END:\n%i\n"
         :empty-lines 1)

        ("m" "Meeting" entry (file "")
         "* %u %? :tapaminen:\n:LOGBOOK:\n- Created %U\n:END:\n"
         :clock-in t :clock-resume t  :empty-lines 1)

        ("c" "Phone call" entry (file "")
         "* %U %? :puhelu:\n:LOGBOOK:\n- Created %U\n:END:\n"
         :clock-in t :clock-resume t  :empty-lines 1)

        ("j" "Journal" entry (file+datetree "~/org/diary.org.gpg")
         "* %U\n\n%?")

        ("e" "Event" entry (file "")
         "* %? :tapahtuma:\n <%(org-read-date)> \n"
         )))

(require 'org-protocol)

(setq org-agenda-custom-commands
              '(
                ("A" "All tasks (for export)" todo ""
                (
                (org-agenda-overriding-header "Export")
                (org-agenda-with-colors nil)
                ) ("~/org/exported-lists/all.org"))
                ("Gw" "Agenda työ"
                 (
                   (agenda "" ((org-agenda-span 1)             ; daily agenda
                      (org-deadline-warning-days 7)            ; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
                  (tags-todo "+työ-@web" ((org-agenda-overriding-header "Työt") (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                  (tags-todo "+työ+@web" ((org-agenda-overriding-header "Selain") (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                  (tags-todo "+työ+puhelu" ((org-agenda-overriding-header "Puhelut") (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                  )
                 (
                 (org-agenda-remove-tags t)
                 (org-agenda-todo-keyword-format "")                  
                 (org-agenda-prefix-format "  %?-12t% s")
                 (org-agenda-compact-blocks t)
                 ))
                ("Gh" "Agenda koti"
                 (
                   (agenda "" ((org-agenda-span 1)             ; daily agenda
                      (org-deadline-warning-days 7)            ; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
                  (tags-todo "-työ-@web" ((org-agenda-overriding-header "Työt") (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                  (tags-todo "-työ+@web" ((org-agenda-overriding-header "Selain") (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                  (tags-todo "-työ+puhelu" ((org-agenda-overriding-header "Puhelut") (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
                  )
                 (
                 (org-agenda-remove-tags t)
                 (org-agenda-todo-keyword-format "")                  
                 (org-agenda-prefix-format "  %?-12t% s")
                 (org-agenda-compact-blocks t)
                 ))
                ))


    (defun my-org-agenda-skip-all-siblings-but-first ()
      "Skip all but the first non-done entry."
      (let (should-skip-entry)
        (unless (org-current-is-todo)
          (setq should-skip-entry t))
        (save-excursion
          (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
              (setq should-skip-entry t))))
        (when should-skip-entry
          (or (outline-next-heading)
              (goto-char (point-max))))))

(defun org-current-is-todo ()
      (string= "TODO" (org-get-todo-state)))

(defun my-org-scheduled-for-today ()
       (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (time-to-days (current-time))))
       (and scheduled-day
            (not (= scheduled-day now))))))

(defun my-org-agenda-skip-if-not-scheduled-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
(ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (time-to-days (current-time))))
       (and scheduled-day
            (not (= scheduled-day now))
            subtree-end))))

(setq org-refile-targets '(("gtd.org.gpg"  :maxlevel . 1)
                           (org-agenda-files :tag . "prj")
                           ("polar-gtd.org.gpg" :maxlevel . 1)
                           ("someday.org.gpg" :maxlevel . 2)                             
                           ("polar-someday.org.gpg" :maxlevel . 2)))

(setq org-tags-exclude-from-inheritance '("prj" "puhelu" "muistiinpano" "tapaaminen"))
  (setq org-stuck-projects '("+prj/-MAYBE-DONE-CANCELLED"
                             ("TODO" "WAIT") ()))

  (defun my-org-with-entry ()
    (interactive)
    (save-excursion
      (org-entry-put nil "with" (read-string "With: " (org-entry-get nil "with")))))
  (global-set-key "\C-cw" 'my-org-with-entry)

(global-set-key (kbd "C-S-s-o") 'org-capture)

(setq diary-file "~/org/diary")

(calendar-set-date-style 'european)

(setq calendar-week-start-day 1
      calendar-view-diary-initially-flag t
      calendar-mark-diary-entries-flag t)

(add-hook 'diary-display-hook 'diary-fancy-display)

(setq server-socket-dir "~/.emacs.d/server")
(server-start)

(global-unset-key (kbd "C-q"))

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
(set-face-attribute 'fixed-pitch nil :height 0.8 :family "DejaVu Sans Mono")

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
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

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;; Mustache mode
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


(global-set-key (kbd "C-c f") 'iwb)

;; Moving from window to window using arrows
(global-set-key (kbd "<left>")  'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>")    'windmove-up)
(global-set-key (kbd "<down>")  'windmove-down)

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

(global-set-key (kbd "C-q o")  'find-file-at-point)

(global-set-key (kbd "C-q i")  'quoted-insert)

(defun new-shell ()
  (interactive)

  (let (
        (currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*"))
       )
   (generate-new-buffer newbuf)
   (set-window-dedicated-p currentbuf nil)
   (set-window-buffer currentbuf newbuf)
   (shell newbuf)
  )
)

(global-set-key (kbd "C-q s")  'new-shell)

(defun pw/shell-cd-to-vc-root ()
"Jumps to the root directory of version controled directory structure."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
	 (started-at-pmark (= (point) (marker-position pmark)))         
         (root (vc-root-dir))
         (cmd (concat "cd " root)))
    (save-excursion
      (goto-char pmark)
      (unless comint-process-echoes     
         (insert cmd) (insert "\n"))
      (sit-for 0)			; force redisplay      
      (cd root)
      (comint-send-string proc cmd)
      (comint-send-string proc "\n")
      (set-marker pmark (point))
      )
    
    (if started-at-pmark (goto-char (marker-position pmark)))))

(defun pw/dired-cd-to-vc-root ()
(interactive)
(find-file (vc-root-dir)))

(add-hook 'comint-mode-hook (lambda ()
                            (define-key comint-mode-map (kbd "C-q c r") 'pw/shell-cd-to-vc-root)
                            ))

(add-hook 'dired-mode-hook (lambda ()
                            (define-key dired-mode-map (kbd "C-q c r") 'pw/dired-cd-to-vc-root)
                            ))

;; No passwords show in shell
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

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

(el-get-bundle dockerfile-mode)
;;; Dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(el-get-bundle yaml-mode)

(el-get-bundle expand-region)
(require 'expand-region)
(global-set-key (kbd "C-q C-e") 'er/expand-region)

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
                   (define-key go-mode-map (kbd "C-q j") 'godef-jump)
                   (define-key go-mode-map (kbd "C-q q") 'pop-global-mark)
                   (add-hook 'before-save-hook 'gofmt-before-save))
                 )

(if (system-has-go) (setup-go))

(el-get-bundle multiple-cursors)

(global-set-key (kbd "C-c m <return>") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m .") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m m") 'mc/mark-more-like-this-extended)

(el-get-bundle yasnippet)
(el-get-bundle auto-complete)


(require 'yasnippet)
(require 'auto-complete)
(require 'auto-complete-config)
;;(require 'auto-complete-yasnippet)

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

(el-get-bundle 'wcheck-mode)
(global-set-key (kbd "C-x w") 'wcheck-mode)
(add-hook 'wcheck-mode-hook
          (lambda ()
            (define-key wcheck-mode-map (kbd "C-q l") 'wcheck-change-language)
            (define-key wcheck-mode-map (kbd "C-q c") 'wcheck-actions)
            (define-key wcheck-mode-map (kbd "C-q n") 'wcheck-jump-forward)
            (define-key wcheck-mode-map (kbd "C-q p") 'wcheck-jump-backward)
            ))

(setq wcheck-language-data
      '(
        ("British English"
         (program . "/usr/bin/enchant")
         (args "-l" "-d" "british")
         (action-program . "/usr/bin/ispell")
         (action-args "-a" "-d" "british")
         (action-parser . wcheck-parser-ispell-suggestions))

        ("Finnish"
         (program . "/usr/bin/enchant")
         (args "-l" "-d" "fi")
         (action-program . "/usr/bin/enchant")
         (action-args "-a" "-d" "fi")
         (action-parser . wcheck-parser-ispell-suggestions))

        ))

(wcheck-change-language "British English" 'GLOBAL)

(el-get-bundle 'magit)

(setq default-fill-column 80)

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(defun open-buffer-curent-idea ()
  ""
  (interactive)
  (call-process "idea" nil nil nil (buffer-file-name))
  (if (string-equal system-type "darwin")
      (ns-do-applescript "tell application \"IntelliJ Idea\" to activate")
    )
)

(global-set-key (kbd "C-q C-o")  'open-buffer-curent-idea)

(defun pw/toggle-transparency ()
  "Toggles frame transparency."
  (interactive)
  (if (equal '(100 100) (frame-parameter (selected-frame) 'alpha))
      (set-frame-parameter (selected-frame) 'alpha '(10 10))
    (set-frame-parameter (selected-frame) 'alpha '(100 100))))

(global-set-key (kbd "C-x C-t")  'pw/toggle-transparency)

(defun pw/download-plantuml-jar-if-needed () ""
       (let ((plantuml-jar "~/.emacs.d/plantuml.jar"))
         (if (not (file-exists-p plantuml-jar))
             (progn
               (url-copy-file "http://downloads.sourceforge.net/project/plantuml/plantuml.jar?r=http%3A%2F%2Fplantuml.com%2Fdownload.html&ts=1441279540&use_mirror=netix" plantuml-jar)
               ))
         (expand-file-name plantuml-jar)))

(setq org-plantuml-jar-path  (pw/download-plantuml-jar-if-needed))

;; Don`t confirm plant uml runs for conviency.
(lexical-let ((default-confirm org-confirm-babel-evaluate))
  (defun my-org-confirm-babel-evaluate (lang body)
    (if (string= lang "plantuml") nil default-confirm))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  )

(el-get-install 'plantuml-mode)
(setq org-plantuml-jar-path (pw/download-plantuml-jar-if-needed))

(setq local-init-file "~/.emacs.d/local-init.el")
(if (file-exists-p local-init-file)
(load local-init-file)
)

(el-get-install 'markdown-mode)
