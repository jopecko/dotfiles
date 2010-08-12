;;; ==================================================================
;;; File:    ini-ido
;;; Purpose: Setups for the ido package
;;; ==================================================================

;; ido makes completing buffers and finding files easier
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings

;; do not confirm a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)

(require 'ido)
(ido-mode 1)
(setq
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-enable-tramp-completion nil
 ido-show-dot-for-dired t         ; put . as the first item
 ido-save-directory-list-file (concat tmp-dir "/ido.last")
 ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
 ;; ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
 ido-everywhere t                 ; use for many file dialogs
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ;  don't use url at point (annoying)
 ido-enable-flex-matching t       ; be flexible
 ;;ido-max-prospects 4              ; don't spam my minibuffer
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable_recursive-minibuffers t))
      (visit-tags-table-buffer))
    (ido-completing-read "Project files: "
                         (tags-table-files)
                         nil t)))

(defun ffp () (interactive) (ido-find-file-in-tag-files))
(global-set-key "\C-c\C-f" 'ido-find-file-in-tag-files)
