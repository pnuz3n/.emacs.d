(defun pw/org-calculate-pert-effort-for-current-element () ""
       (interactive)
       (let (
             (o (org-entry-get (point) "EFFORT_O"))
             (m (org-entry-get (point) "EFFORT_M"))
             (p (org-entry-get (point) "EFFORT_P"))
             )
         (if (and o m p)
             (org-entry-put (point) "Effort"
                            (org-duration-from-minutes
                             (/ (+
                                 (org-duration-string-to-minutes o)
                                 (* 4 (org-duration-string-to-minutes m))
                                 (org-duration-string-to-minutes p))
                                6)
                             '(("d" . nil) ("h" . t)))))))

(defun pw/org-calculate-pert-effort-for-all-elements () ""
       (interactive)
       (save-mark-and-excursion
        (set-mark (point-min))
        (goto-char (point-max))
        (org-map-entries 'pw/org-calculate-pert-effort-for-current-element)
        (org-columns-redo)))
