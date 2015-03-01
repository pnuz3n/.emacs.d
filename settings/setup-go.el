
(el-get-bundle go-mode)

(el-get-bundle let-alist)
(el-get-bundle dash)
(el-get-bundle flycheck)
;; go get github.com/dougm/goflymake
(add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
(require 'go-flycheck)


(el-get-bundle go-autocomplete)
(require 'go-autocomplete)


(add-hook 'before-save-hook 'gofmt-before-save)
