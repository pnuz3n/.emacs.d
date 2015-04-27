(el-get-bundle ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(add-hook 'comint-mode-hook
               (lambda ()
                 (define-key comint-mode-map (kbd "C-c SPC") 'ace-jump-mode)
                 (define-key comint-mode-map (kbd "<C-return>") 'comint-accumulate)
                ))

                 
;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
