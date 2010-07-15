;;; ==================================================================
;;; File:    ini-uniquify
;;; Purpose: Setup uniquify
;;; ==================================================================

(when (require-soft 'uniquify) ;; make buffer names more unique
  (setq
    uniquify-buffer-name-style 'post-forward
    uniquify-separator ":"
    uniquify-after-kill-buffer-p t
    uniquify-ignore-buffers-re "^\\*"))
