;;; -*- lexical-binding: t -*-
(setq local-pre-init-file "~/.emacs.d/local-pre-init.el")
(if (file-exists-p local-pre-init-file)
(load local-pre-init-file)
)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package all-the-icons
:straight t)

(use-package nerd-icons
  :straight t
  :config
  (let ((font-dest (concat (or (getenv "XDG_DATA_HOME")
                               (concat (getenv "HOME") "/.local/share"))
                           "/fonts/"
                           nerd-icons-fonts-subdirectory)))
    (unless (cl-every (lambda (font)
                        (file-exists-p (expand-file-name font font-dest)))
                      nerd-icons-font-names)
      (nerd-icons-install-fonts t))))

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-hook 'text-mode-hook 'variable-pitch-mode)

(use-package doom-themes
  :straight t
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-acario-dark t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
:straight t
:straight t
:init (doom-modeline-mode 1))

(setq window-divider-default-right-width 4
      window-divider-default-bottom-width 4)
(set-face-foreground 'window-divider "#2E3044")
(window-divider-mode 1)

(use-package vertico-posframe
  :straight t
  :after vertico
  :init
  (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
  )

(use-package treemacs
  :straight t
  :bind
  ("C-x t t" . treemacs)
  :config
  (setq treemacs-width 30))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package docker
  :straight t
  :straight t
  :bind ("C-c d" . docker))

(let ((elements (split-string (getenv "PATH") ":")))
  (dolist (e elements)
    (add-to-list 'exec-path e)))

(add-to-list 'exec-path "~/bin")
(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(when (eq system-type 'darwin)
(setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.gnupg/S.gpg-agent.ssh")))

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

(use-package vertico
:straight t
:straight t
:init
(vertico-mode))

(use-package marginalia
:straight t
:straight t
:init
(marginalia-mode))

(use-package consult
:straight t
:straight t
:bind
(("C-s" . consult-line)
 ("C-x b" . consult-buffer)
 ("M-y" . consult-yank-pop)))

(use-package orderless
:straight t
:init
(setq completion-styles '(orderless)))

(use-package corfu
  :straight t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq completion-category-overrides
      '((file (styles basic partial-completion))
        (eglot (styles orderless))
        (lsp-capf (styles orderless))))

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

(defun ps/open-new-shell ()
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
(defun ps/open-new-local-shell ()
  "Open a new shell buffer in home directory."
  (interactive)
  (let ((default-directory "~/"))
        (ps/open-new-shell)))

(global-set-key (kbd "C-q s")  'ps/open-new-shell)
(global-set-key (kbd "C-q C-s")  'ps/open-new-local-shell)

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

(defvar ps/fish-executable (executable-find "fish")
  "Path to the fish shell, or nil if not installed.")

(defun ps/fish-completion-at-point ()
  "Completion-at-point function that queries fish for annotated candidates."
  (when (get-buffer-process (current-buffer))
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (line (buffer-substring-no-properties
                  (save-excursion (comint-bol) (point))
                  end))
           (raw (with-output-to-string
                  (with-current-buffer standard-output
                    (call-process ps/fish-executable nil t nil
                                  "-c" (format "complete -C%s"
                                               (shell-quote-argument line))))))
           (results (mapcar (lambda (l)
                              (let ((parts (split-string l "\t")))
                                (cons (car parts) (cadr parts))))
                            (split-string raw "\n" t))))
      (when results
        (list start end
              (mapcar #'car results)
              :annotation-function
              (lambda (c)
                (when-let* ((desc (cdr (assoc c results))))
                  (concat "  " desc))))))))

(if ps/fish-executable
    (add-hook 'shell-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions
                          #'ps/fish-completion-at-point nil t)))
  (display-warning 'fish-completion
                   "fish shell not found; install it for shell completion descriptions"
                   :warning))

(defvar ps/gptel-shell-context-lines 100
  "Lines of recent shell output to include as context for gptel autocomplete.")

(defvar ps/gptel-shell-idle-threshold 1.0
  "Seconds since last output before the shell is considered idle.")

(defvar-local ps/shell-last-output-time nil
  "Time of last comint output in this buffer.")

(defun ps/shell-track-output (&rest _)
  "Record the time of the latest shell output."
  (setq ps/shell-last-output-time (current-time)))

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-output-filter-functions
                      #'ps/shell-track-output nil t)))

(defun ps/gptel-shell-idle-prompt-p ()
  "Non-nil when point is at an idle shell prompt."
  (and (derived-mode-p 'shell-mode)
       (comint-after-pmark-p)
       (or (null ps/shell-last-output-time)
           (> (float-time (time-since ps/shell-last-output-time))
              ps/gptel-shell-idle-threshold))))

(defun ps/gptel-shell-guard (orig &rest args)
  "In shell-mode, only trigger gptel autocomplete at an idle prompt."
  (if (derived-mode-p 'shell-mode)
      (when (ps/gptel-shell-idle-prompt-p)
        (apply orig args))
    (apply orig args)))

(defun ps/gptel-shell-context (orig &rest args)
  "Override context line count for shell-mode buffers."
  (if (derived-mode-p 'shell-mode)
      (let ((gptel-autocomplete-before-context-lines ps/gptel-shell-context-lines)
            (gptel-autocomplete-after-context-lines 0))
        (apply orig args))
    (apply orig args)))

(advice-add 'gptel--post-command :around #'ps/gptel-shell-guard)
(advice-add 'gptel-complete :around #'ps/gptel-shell-context)

;; No passwords show in shell
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(use-package avy
 :straight t
 :bind (
        ("C-." . avy-goto-char)
        ("C-q q". avy-pop-mark)))

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

(use-package magit
  :straight t
  :bind ("C-x g" . magit-status))

(use-package treemacs-magit  :straight t :after (treemacs magit))

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t
        lsp-enable-symbol-highlighting t
        lsp-enable-links t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-headerline-breadcrumb-enable t
        lsp-modeline-code-actions-enable t
        lsp-diagnostics-provider :auto
        lsp-completion-provider :none
        lsp-idle-delay 0.5)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover t))

(use-package lsp-treemacs
  :straight t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

(use-package consult-lsp
  :straight t
  :after (consult lsp-mode)
  :commands (consult-lsp-symbols consult-lsp-diagnostics consult-lsp-file-symbols))

(defvar lsp-booster-install-dir
    (expand-file-name "lsp-booster" user-emacs-directory)
    "Directory where emacs-lsp-booster binary will be installed.")

  (defun lsp-booster--github-asset-name ()
    "Return the expected release asset name suffix for this platform."
    (let ((arch (car (split-string system-configuration "-"))))
      (cond
       ((and (eq system-type 'gnu/linux) (string= arch "x86_64"))
        "x86_64-unknown-linux-musl.zip")
       ((and (eq system-type 'darwin) (string= arch "x86_64"))
        "x86_64-apple-darwin.zip")
       (t nil))))

  (defun lsp-booster--ensure-installed ()
    "Download and install emacs-lsp-booster if not already on PATH.
Queries the GitHub releases API for the latest version, downloads
the appropriate pre-built binary, and extracts it to `lsp-booster-install-dir'."
    (when (and (not (executable-find "emacs-lsp-booster"))
               (lsp-booster--github-asset-name))
      (let ((install-dir lsp-booster-install-dir))
        (unless (file-directory-p install-dir)
          (make-directory install-dir t))
        (unless (member install-dir exec-path)
          (add-to-list 'exec-path install-dir)
          (setenv "PATH" (concat install-dir ":" (getenv "PATH"))))
        (message "emacs-lsp-booster not found, downloading from GitHub...")
        (url-retrieve
         "https://api.github.com/repos/blahgeek/emacs-lsp-booster/releases/latest"
         (lambda (_status)
           (goto-char url-http-end-of-headers)
           (let* ((release (json-parse-buffer :object-type 'alist))
                  (assets (alist-get 'assets release))
                  (suffix (lsp-booster--github-asset-name))
                  (asset (seq-find
                          (lambda (a)
                            (and (string-suffix-p suffix (alist-get 'name a))
                                 (not (string-suffix-p ".sha256sum" (alist-get 'name a)))))
                          assets))
                  (url (alist-get 'browser_download_url asset)))
             (if (not url)
                 (message "emacs-lsp-booster: no matching release asset found for %s" suffix)
               (let ((zip-file (expand-file-name "emacs-lsp-booster.zip" temporary-file-directory)))
                 (url-copy-file url zip-file t)
                 (call-process "unzip" nil nil nil "-o" zip-file "-d" lsp-booster-install-dir)
                 (let ((binary (expand-file-name "emacs-lsp-booster" lsp-booster-install-dir)))
                   (set-file-modes binary #o755)
                   (delete-file zip-file)
                   (message "emacs-lsp-booster installed to %s" binary))))))
         nil t))))

  (lsp-booster--ensure-installed)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-java
  :straight t
  :after lsp-mode
  :preface
  (defun my/detect-java-runtimes ()
    "Return a vector for `lsp-java-configuration-runtimes' by scanning
common JVM install roots.  Each detected JDK becomes a JavaSE-<version>
entry; the newest version is marked as default."
    (let* ((roots '("/usr/lib/jvm"
                    "/usr/java"
                    "/Library/Java/JavaVirtualMachines"))
           (re "\\`\\(?:java-\\|jdk-?\\|openjdk-\\)\\(?:1\\.\\)?\\([0-9]+\\)")
           (best (make-hash-table :test 'eql)))
      (dolist (root roots)
        (when (file-directory-p root)
          (dolist (entry (directory-files root t "\\`[^.]"))
            (let ((base (file-name-nondirectory entry)))
              (when (and (file-directory-p entry)
                         (not (file-symlink-p entry))
                         (string-match re base))
                (let* ((ver (string-to-number (match-string 1 base)))
                       (mac-home (expand-file-name "Contents/Home" entry))
                       (home (if (file-directory-p mac-home) mac-home entry))
                       (java-bin (expand-file-name "bin/java" home))
                       (existing (gethash ver best)))
                  (when (and (file-executable-p java-bin)
                             (or (null existing)
                                 (< (length home) (length existing))))
                    (puthash ver home best))))))))
      (let* ((versions (sort (hash-table-keys best) #'<))
             (newest (car (last versions))))
        (apply #'vector
               (mapcar (lambda (v)
                         (let ((name (format "JavaSE-%s" (if (= v 8) "1.8" v)))
                               (path (gethash v best)))
                           (if (= v newest)
                               (list :name name :path path :default t)
                             (list :name name :path path))))
                       versions)))))

  (defun my/detect-java-path ()
    "Path of the newest `java' executable detected by
`my/detect-java-runtimes', or nil when no JDK is found."
    (let ((default (seq-find (lambda (r) (plist-get r :default))
                             (append (my/detect-java-runtimes) nil))))
      (when default
        (expand-file-name "bin/java" (plist-get default :path)))))
  :config
  (setq lsp-java-server-install-dir "~/.emacs.d/lsp-java/"
        lsp-java-workspace-dir (expand-file-name "~/.emacs.d/lsp-java-workspace/")
        lsp-java-import-gradle-enabled t
        lsp-java-import-maven-enabled t
        lsp-java-maven-download-sources t
        lsp-java-autobuild-enabled t
        lsp-java-format-enabled t
        lsp-java-format-settings-url nil
        lsp-java-format-settings-profile nil
        lsp-java-code-generation-use-blocks t)
  (let ((runtimes (my/detect-java-runtimes))
        (java-path (my/detect-java-path)))
    (when (> (length runtimes) 0)
      (setq lsp-java-configuration-runtimes runtimes))
    (when java-path
      (setq lsp-java-java-path java-path)))
  :hook
  (java-mode . (lambda ()
                 (require 'lsp-java)
                 (lsp-deferred))))

(setq default-fill-column 80)

(global-set-key (kbd "C-x k")
              (lambda () (interactive) (kill-this-buffer)))

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

(defun ps/toggle-transparency ()
  "Toggles frame transparency."
  (interactive)
  (if (equal '(100 100) (frame-parameter (selected-frame) 'alpha))
      (set-frame-parameter (selected-frame) 'alpha '(10 10))
    (set-frame-parameter (selected-frame) 'alpha '(100 100))))

(global-set-key (kbd "C-q t")  'ps/toggle-transparency)

(defun pw/download-plantuml-jar-if-needed () ""
       (let ((plantuml-jar "~/.emacs.d/plantuml.jar"))
         (if (not (file-exists-p plantuml-jar))
             (progn
               (url-copy-file "http://downloads.sourceforge.net/project/plantuml/plantuml.jar?r=http%3A%2F%2Fplantuml.com%2Fdownload.html&ts=1441279540&use_mirror=netix" plantuml-jar)
               ))
         (expand-file-name plantuml-jar)))

(use-package deflate
  :straight (deflate :type git :host github :repo "skuro/deflate"))

  (use-package plantuml-mode
    :straight (plantuml-mode :type git :host github :repo "skuro/plantuml-mode")
    :mode ("\\.plantuml\\'" "\\.puml\\'")
    :config
    (setq plantuml-default-exec-mode 'jar
          org-plantuml-jar-path (pw/download-plantuml-jar-if-needed)
          plantuml-jar-path (pw/download-plantuml-jar-if-needed)))

;; Don`t confirm plant uml runs for conviency.
(let ((default-confirm org-confirm-babel-evaluate))
  (defun my-org-confirm-babel-evaluate (lang body)
    (if (string= lang "plantuml") nil default-confirm))
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  )

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(setq org-startup-with-inline-images t)
(setq plantuml-indent-level 2)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

;; Ollama host configuration
(unless (boundp 'my/ollama-host-name)
  (defvar my/ollama-host-name "127.0.0.1"))
(unless (boundp 'my/ollama-port)
  (defvar my/ollama-port 11434))

;; Ollama model names
(unless (boundp 'my/ollama-chat-model)
  (defvar my/ollama-chat-model "gpt-oss:20b"))
(unless (boundp 'my/ollama-fast-model)
  (defvar my/ollama-fast-model "llama3.1:8b"))
(unless (boundp 'my/ollama-utility-model)
  (defvar my/ollama-utility-model "qwen2.5-coder:14b"))
(unless (boundp 'my/ollama-embedding-model)
  (defvar my/ollama-embedding-model "nomic-embed-text"))

;; Derived variable
(defvar my/ollama-host
  (format "%s:%d" my/ollama-host-name my/ollama-port))

;; Backend selection defaults
(unless (boundp 'my/use-bedrock-gptel)
  (defvar my/use-bedrock-gptel t))  ; Default to Bedrock
(unless (boundp 'my/use-ollama-autocomplete)
  (defvar my/use-ollama-autocomplete t))

;; Bedrock configuration defaults
(unless (boundp 'my/bedrock-region)
  (defvar my/bedrock-region "us-east-1"))
(unless (boundp 'my/bedrock-model-region)
  (defvar my/bedrock-model-region 'us))
(unless (boundp 'my/bedrock-model)
  (defvar my/bedrock-model 'claude-sonnet-4-20250514))

;; Claude API configuration defaults
(unless (boundp 'my/claude-api-key)
  (defvar my/claude-api-key nil))  ; Should be set in local-pre-init.el
(unless (boundp 'my/claude-model)
  (defvar my/claude-model 'claude-sonnet-4-20250514))

(define-prefix-command 'my/ai-map)
(global-set-key (kbd "C-q a") #'my/ai-map)

(use-package gptel
  :straight t
  :straight t
  :commands (gptel gptel-send gptel-rewrite)
  :init
  ;; Always set up Ollama backend (might be used for autocomplete)
  (setq my/gptel-ollama
        (gptel-make-ollama
         "Ollama"
         :host my/ollama-host
         :stream t
         :models (mapcar #'intern
                        (list my/ollama-chat-model
                              my/ollama-fast-model
                              my/ollama-utility-model))))

  ;; Set up the main backend based on configuration
  (if my/use-bedrock-gptel
      ;; Use AWS Bedrock
      (progn
        (setq my/gptel-main-backend
              (gptel-make-bedrock
               "Claude-Bedrock"
               :stream t
               :region 'eu-west-1'
               :model-region 'eu
               :models '(claude-opus-4-6
                 claude-sonnet-4-20250514
                 claude-haiku-4-5-20251001)))               

        (setq my/gptel-main-model my/bedrock-model))
    ;; Use Claude API directly
    (progn
      (setq my/gptel-main-backend
            (gptel-make-anthropic
             "Claude"
             :stream t
             :key my/claude-api-key))  ; Can also use 'gptel-api-key for auth-source
      (setq my/gptel-main-model my/claude-model)))

  :config
  (setq gptel-backend my/gptel-main-backend)
  (setq gptel-model my/gptel-main-model)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-track-response nil)

  (define-key my/ai-map (kbd "c") #'gptel)
  (define-key my/ai-map (kbd "s") #'gptel-send)
  (define-key my/ai-map (kbd "r") #'gptel-rewrite))

(use-package aidermacs
  :straight t
  :commands (aidermacs-transient-menu aidermacs-run-aider)
  :init
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-use-voice nil)
  :config
  (setq aidermacs-default-model (exec-path-from-shell-getenv "AIDER_MODEL"))
  (setq aidermacs-weak-model (exec-path-from-shell-getenv "AIDER_WEAK_MODEL"))
  (define-key my/ai-map (kbd "a") #'aidermacs-transient-menu))

(defun my/git-diff-staged-to-buffer ()
  "Show staged git diff in a temp buffer."
  (interactive)
  (let ((buf (get-buffer-create "*git-staged-diff*")))
    (with-current-buffer buf
      (erase-buffer)
      (call-process "git" nil buf nil "diff" "--staged")
      (diff-mode))
    (pop-to-buffer buf)))

(define-key my/ai-map (kbd "d") #'my/git-diff-staged-to-buffer)

(use-package llm
  :straight t
  :straight t)

(use-package ellama
  :straight t
  :straight t
  :after llm
  :config
  (require 'llm-ollama)

  (setq ellama-provider
        (make-llm-ollama
         :host my/ollama-host-name
         :port my/ollama-port
         :chat-model my/ollama-utility-model
         :embedding-model my/ollama-embedding-model))


  (setq ellama-language "English")
  (setq ellama-naming-provider ellama-provider)
  (setq ellama-summarization-provider ellama-provider)

  (define-key my/ai-map (kbd "e") #'ellama)
  (define-key my/ai-map (kbd "i") #'ellama-improve-wording)
  (define-key my/ai-map (kbd "m") #'ellama-summarize))

;; Set up autocomplete backend based on configuration
(when my/use-ollama-autocomplete
  (setq my/gptel-autocompletion-backend
        (gptel-make-ollama
         "autocomplete"
         :host my/ollama-host
         :models (list my/ollama-utility-model))))

;; If not using Ollama, use the main backend
(unless (boundp 'my/gptel-autocompletion-backend)
  (setq my/gptel-autocompletion-backend my/gptel-main-backend))

(use-package gptel-autocomplete
  :straight '(:type git :host github :repo "JDNdeveloper/gptel-autocomplete")
  :config
  (require 'gptel-autocomplete)
  (setq gptel-autocomplete-before-context-lines 100
        gptel-autocomplete-after-context-lines 20
        gptel-autocomplete-temperature 0.1
        gptel-autocomplete-use-context t
        gptel-autocomplete-idle-delay 0)
  (keymap-set gptel-autocomplete-completion-map "C-e" #'gptel-accept-completion)
  (keymap-set gptel-autocomplete-completion-map "<tab>" #'gptel-accept-completion)
  (keymap-set gptel-autocomplete-completion-map "M-f" #'gptel-accept-word)
  (define-key my/ai-map (kbd "<tab>") #'gptel-autocomplete-mode)

  (defun my/gptel-autocomplete-backend (orig &rest args)
    (let ((gptel-backend my/gptel-autocompletion-backend))
      (apply orig args)))
  (advice-add 'gptel-complete :around #'my/gptel-autocomplete-backend))

(use-package vterm
  :straight t
  :commands (vterm vterm-other-window)
  :init
  ;; Optional: pick a convenient key
  (global-set-key (kbd "C-c t") #'vterm)
  :hook
  ;; Claude Code draws spinners and progress indicators whose width
  ;; fluctuates. Without this Emacs visually wraps long lines onto the
  ;; next row and the TUI jitters constantly.
  (vterm-mode . (lambda () (setq-local truncate-lines t))))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-q a t" . claude-code-ide-menu)
  :config
  ;; Copy environment variables from shell
  (exec-path-from-shell-copy-env "AWS_BEARER_TOKEN_BEDROCK")
  (exec-path-from-shell-copy-env "CLAUDE_CODE_USE_BEDROCK")
  (exec-path-from-shell-copy-env "ANTHROPIC_API_KEY")
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-focus-claude-after-ediff nil)
  (claude-code-ide-emacs-tools-setup)
  ;; Silently revert an already-open buffer before diffing so that
  ;; find-file-noselect does not prompt "revert buffer?" in the
  ;; minibuffer and block the diff view.
  (define-advice claude-code-ide-mcp--create-diff-buffers
      (:before (old-file-path &rest _) auto-revert)
    "Silently revert buffer visiting OLD-FILE-PATH before diffing."
    (when-let ((buf (find-buffer-visiting old-file-path)))
      (with-current-buffer buf
        (revert-buffer t t)))))

(straight-use-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "AWS_REGION")
(exec-path-from-shell-copy-env "AWS_PROFILE")

(setq local-init-file "~/.emacs.d/local-init.el")
(if (file-exists-p local-init-file)
(load local-init-file)
)
