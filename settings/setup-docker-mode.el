(el-get-bundle dockerfile-mode)
;;; Dockerfile mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
