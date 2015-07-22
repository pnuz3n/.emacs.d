;; Idea related shortcuts

(defun idea-open-file (s) "Opens file in idea"
       (interactive
        (list (idea-open-file (buffer-substring (region-beginning) (region-end)))))
       (start-process "" nil "idea" s)
       )

