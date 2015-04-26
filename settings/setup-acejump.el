(el-get-bundle ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(add-hook 'comint-mode-hook
               (lambda ()
                 (define-key comint-mode-map (kbd "C-c SPC") 'ace-jump-mode)
                 (define-key comint-mode-map (kbd "<C-return>") 'comint-accumulate)
                ))

                 

