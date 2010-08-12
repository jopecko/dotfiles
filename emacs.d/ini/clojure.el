;;; ==================================================================
;;; File:    ini-clojure.el
;;; Purpose: Setup for Clojure mode
;;; ==================================================================

;;____________________________________________________________________
;;;;    Programming - Clojure

;; TODO dump message into *msg* buffer to remind to setq

(when (locate-library "clojure-mode")
  (add-hook 'clojure-mode-hook
            '(lambda ()
               (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp))))

;; http://bc.tech.coop/blog/081120.html
(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

;; set this on OS X only or put in local.el
(setq slime-browse-local-javadoc-root "/Users/Shared/SDKs/J2SE/5.0")

;; When you open a local JavaDoc html file on Mac, you may get the "Are you
;; sure you want to open it?" dialog box. You won't see it next time you open
;; the same html file, but it's really annoying to see it for any html files
;; being opened first time. You can stop it by running the
;; xattr command like this:
;;
;; xattr -d com.apple.quarantine `find . -name "*.html"`
;;
;; Unfortunately xattr has a limited buffer for the file names to process so
;; that you can't run the above command on the docs directory once for all or
;; it'll fail. Instead, run the command on the java, javax, and org
;; directories under the docs/api directory separately."
(defun slime-browse-local-javadoc (ci-name)
  "Browse local JavaDoc documentation on Java class/Interface at point."
  (interactive (list (slime-read-symbol-name "Class/Interface name: ")))
  (when (not ci-name)
    (error "No name given"))
  (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	(path (concat (expand-file-name slime-browse-local-javadoc-root) "/docs/api/")))
    (with-temp-buffer
      (insert-file-contents (concat path "allclasses-noframe.html"))
      (let ((l (delq nil
		     (mapcar #'(lambda (rgx)
				 (let* ((r (concat "\\.?\\(" rgx "[^./]+\\)[^.]*\\.?$"))
					(n (if (string-match r name)
					       (match-string 1 name)
					     name)))
				   (if (re-search-forward (concat "<A HREF=\"\\(.+\\)\" +.*>" n "<.*/A>") nil t)
				       (match-string 1)
				     nil)))
			     '("[^.]+\\." "")))))
	(if l
	    (browse-url (concat "file://" path (car l)))
	  (error (concat "Not found: " ci-name)))))))

(add-hook
 'slime-connected-hook
 #'(lambda ()
     (define-key slime-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)
     (define-key slime-repl-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)))

(add-hook
 'slime-connected-hook
 (lambda ()
   (interactive)
   (slime-redirect-inferior-output)
   (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
   (define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe)
   (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
   (define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc)))
