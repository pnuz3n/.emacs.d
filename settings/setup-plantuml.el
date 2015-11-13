
;; Donwload plantuml.jar if missing and use it.
(let ((plantuml-jar "~/.emacs.d/plantuml.jar"))
  (if (not (file-exists-p plantuml-jar))
      (progn
        (url-copy-file "http://downloads.sourceforge.net/project/plantuml/plantuml.jar?r=http%3A%2F%2Fplantuml.com%2Fdownload.html&ts=1441279540&use_mirror=netix" plantuml-jar)
        ))
  (setq org-plantuml-jar-path  (expand-file-name plantuml-jar))
)

;; Don`t confirm plant uml runs for conviency.
(let ((default-confirm org-confirm-babel-evaluate))
 (defun my-org-confirm-babel-evaluate (lang body)
           (if (string= lang "plantuml") nil default-confirm))
 (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
)
