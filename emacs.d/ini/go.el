;;; ==================================================================
;;; File:    ini-go.el
;;; Purpose: Setup for Go mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - The Go Programming Language

;; TODO dump message into *msg* buffer to remind to setq

(when (require-soft 'go-mode-load)
  (add-hook 'go-mode-hook
            (lambda ()
	       (add-hook 'before-save-hook 'gofmt-before-save)))
  (add-hook 'go-mode-hook
	    (lambda ()
               (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook
	    (lambda ()
               (local-set-key (kbd "C-c i") 'go-goto-imports)))
  (add-hook 'go-mode-hook
	    (lambda ()
               (local-set-key (kbd \"M-.\") 'godef-jump))))

