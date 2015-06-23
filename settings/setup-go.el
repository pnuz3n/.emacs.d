(defun system-has-go () "Tests wether go is installed or not" 
       (condition-case nil
           (progn
             (start-process "" nil "go")
             t
             )
         (error nil))
       )
(defun setup-go () "Install go environment with el-get"
       (el-get-bundle go-mode)
       (el-get-bundle let-alist)
       (el-get-bundle dash)
       (el-get-bundle flycheck)
       ;; go get github.com/dougm/goflymake
       (add-to-list 'load-path "~/src/github.com/dougm/goflymake")
       (require 'go-flycheck)


       ;; go get github.com/nsf/gocode
       (el-get-bundle go-autocomplete)
       (require 'go-autocomplete)

       (add-hook 'go-mode-hook 
                 (lambda ()
                         (add-hook 'before-save-hook 'gofmt-before-save))
                 ))

(if (system-has-go) (setup-go))
