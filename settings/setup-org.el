(el-get-bundle org)
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (makefile . t)
   (sh . t)
   (js . t)
   (emacs-lisp . nil)
   ))
