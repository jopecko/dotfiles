;;; ==================================================================
;;; File:    ini-org
;;; Purpose: Setups for Org Mode
;;; ==================================================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode / remember-mode
;; we use org-mode as the backend for remember
;;(org-remember-insinuate)
;; (setq org-directory "~/.emacs.d/org/")
;; (setq
;;   org-agenda-files (directory-files (concat org-directory "agenda/")
;;                      t  "^[^#].*\\.org$") ; ignore backup files
;;   org-agenda-include-diary t
;;   org-agenda-show-all-dates t              ; shows days without items
;;   org-agenda-skip-deadline-if-done  t      ; don't show in agenda...
;;   org-agenda-skip-scheduled-if-done t      ; .. when done
;;   org-agenda-start-on-weekday nil          ; start agenda view with today

;;   org-agenda-skip-unavailable-files t      ; don't ask, just skip

;;   org-agenda-todo-ignore-deadlines t       ; don't include ...
;;   org-agenda-todo-ignore-scheduled t       ; ...timed/agenda items...
;;   org-agenda-todo-ignore-with-date t       ; ...in the todo list

;;   org-completion-use-ido t                  ; use ido when it makes sense

;;   org-enforce-to-checkbox-dependencies t   ; parents can't be closed...
;;   org-enforce-todo-dependencies t          ; ...before their children
;;   org-hide-leading-stars t                 ; hide leading stars

;;   org-log-done 'time                       ; log time when marking as DONE
;;   org-return-follows-link t                ; return follows the link
;;   org-tags-column -78                      ; tags end at pos 78

;;   org-export-with-section-numbers nil      ; no numbers in export headings
;;   org-export-with-toc nil                  ; no ToC in export
;;   org-export-with-author-info nil          ; no author info in export
;;   org-export-with-creator-info nil         ; no creator info
;;   org-export-htmlize-output-type 'css

;;   org-use-fast-todo-selection t            ; fast todo selection
;;   org-archive-location (concat org-directory "/archive.org::%s")

;;   org-tag-alist '( ("FAMILY"   .  ?f)
;;                    ("FINANCE"  .  ?m)
;;                    ("FRIENDS"  .  ?v)
;;                    ("HACKING"  .  ?h)
;;                    ("HOME"     .  ?t)
;;                    ("MUSIC"    .  ?m)
;;                    ("PHONE"    .  ?p)
;;                    ("SPORTS"   .  ?s)
;;                    ("URGENT"   .  ?u)
;;                    ("WORK"     .  ?w))

;;   org-todo-keywords '((type "TODO(t)" "STARTED(s)" "MAYBE(m)" "INFO(i)"
;;                         "WAITING(w)" "VIEW(v)" "|" "DONE(d)" "CANCELLED(c)")))

;; (org-remember-insinuate) ;; integrate remember with org
;; (setq
;;   org-default-notes-file (concat org-directory "agenda/remember.org")
;;   org-remember-templates
;;   '(
;;      ("Todo" ?t "* TODO %u %?\n" nil "Tasks")
;;      ("Link" ?l "* INFO %u %?\n %a\n" nil "Links")
;;      ("Note" ?n "* INFO %u %^{Title}\n %?\n" nil "Notes")))

;; (add-hook 'org-mode-hook
;;   (lambda()
;;     (auto-fill-mode t)
;;     (set-fill-column 78)
;;     (add-hook 'before-save-hook 'org-agenda-to-appt t t)
;;     (font-lock-add-keywords nil
;;       '(("\\<\\(FIXME\\)"
;;           1 font-lock-warning-face prepend)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; http://orgmode.org
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)

;; orgstruct++-mode is enabled in Gnus message buffers to aid in creating
;; structured email messages.
;; (setq message-mode-hook
;;       (quote (orgstruct++-mode
;;               (lambda nil (setq fill-column 72) (flyspell-mode 1))
;;               turn-on-auto-fill
;;               bbdb-define-all-aliases)))

;;  (setq org-todo-keywords
;;        '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
;;          (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
;;          (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
;;          (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S!)" "OPEN(O@)" "|" "CANCELLED(c@/!)")
              (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)"))))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("STARTED" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITING" :foreground "orange" :weight bold)
        ("SOMEDAY" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("QUOTE" :foreground "red" :weight bold)
        ("QUOTED" :foreground "magenta" :weight bold)
        ("APPROVED" :foreground "forest green" :weight bold)
        ("EXPIRED" :foreground "forest green" :weight bold)
        ("REJECTED" :foreground "forest green" :weight bold)
        ("OPEN" :foreground "blue" :weight bold)))

;; Fast todo selection allows changing from any task todo state to any other
;; state directly by selecting the appropriate key from the fast todo selection
;; key menu. This is a great feature!
;;
;; Changing a task state is done with
;; C-c C-t KEY
;; where KEY is the appropriate fast todo state selection key as defined in
;; org-todo-keywords.
(setq org-use-fast-todo-selection t)

;; This function uses org-mode support for plain list to facilitate dragging
;; URLs from a webbrowser (or other apps) to an org-mode buffer
;; http://www.emacswiki.org/emacs/OrgMode
(defadvice dnd-insert-text (around org-mouse-dnd-insert-text activate)
  (if (eq major-mode 'org-mode)
      (progn
	(cond
	 ;; if this is the end of the line then just insert text here
	 ((eolp)
	  (skip-chars-backward " \t")
	  (kill-region (point) (point-at-eol))
	  (unless (looking-back ":") (insert ":"))
	  (insert " "))

	 ;; if this is the beginning of the line then insert before
	 ((and (looking-at " \\|\t")
	       (save-excursion
		 (skip-chars-backward " \t") (bolp)))
	  (beginning-of-line)
	  (looking-at "[ \t]*")
	  (open-line 1)
	  (indent-to (- (match-end 0) (match-beginning 0)))
	  (insert "+ "))

	 ;; if this is a middle of the line, then insert after
	 (t
	  (end-of-line)
	  (newline t)
	  (indent-relative)
	  (insert "+ ")))
	(insert text)
	(beginning-of-line))
    ad-do-it))

;; include all org files from a directory into the agenda
(setq org-agenda-files (file-expand-wildcards "~/.gtd/*.org"))
