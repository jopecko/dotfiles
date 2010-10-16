(require 'mercurial)

;; http://www.joshmatthews.net/blog/2010/03/dealing-with-mercurial-patch-queue-rejects-in-emacs/
(defun switch-hg-reject ()
  (interactive)
  (let ((other-file
     (if (string= (substring (buffer-file-name) -4 nil) ".rej")
         (substring (buffer-file-name) 0 -4)
       (concat (buffer-file-name) ".rej"))))
    (if (file-exists-p other-file)
    (save-selected-window
      (switch-to-buffer-other-window (find-file-noselect other-file)))
      (message "No alternate reject file found"))))
(defun kill-hg-reject ()
  (interactive)
  (let ((reject-file (concat (buffer-file-name) ".rej")))
    (kill-buffer
     (find-buffer-visiting reject-file))))

(global-set-key "\C-cr" 'switch-hg-reject)
(global-set-key "\C-xr" 'kill-hg-reject)
