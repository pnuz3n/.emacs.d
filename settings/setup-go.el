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
       (el-get-bundle let-alist)
       (el-get-bundle dash)
       
       ;; Require makeinfo which major version is 5 ore more
       (if (< 4 (car (makeinfo-version)))
           (
           (el-get-bundle flycheck)   
         ;; go get github.com/dougm/goflymake
         (add-to-list 'load-path "~/src/github.com/dougm/goflymake")
         (require 'go-flycheck))
       )

       

       ;; go get github.com/nsf/gocode
       (el-get-bundle go-autocomplete)
       (require 'go-autocomplete)

       (add-hook 'go-mode-hook 
                 (lambda ()
                         (add-hook 'before-save-hook 'gofmt-before-save))
                 ))

(if (system-has-go) (setup-go))
