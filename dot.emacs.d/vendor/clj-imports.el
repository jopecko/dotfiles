;;; clj-imports.el --- Mangle Clojure imports of Java classes

;; Copyright (C) 2009  Mark Triggs

;; Author: Mark Triggs <mst@dishevelled.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Try this:
;;
;; (define-key clojure-mode-map (kbd "C-c i") 'clj-imports-insert)
;; (define-key clojure-mode-map (kbd "C-c C-n") 'clj-imports-eval-ns)
;;
;;; Then, from within your .clj file:
;;
;; C-c i        Prompt for a regexp matching a class and add it to the
;;              (ns (:import ...)), creating an (:import) entry if one
;;              doesn't exist already.  If you've got thingatpt.el loaded,
;;              input defaults to the symbol at point.
;;
;; C-u C-c i    As above, but insert a standalone (import ...) expression
;;              instead of fiddling with namespaces.
;;
;; C-c C-n      Re-evaluate the (ns ...) expression to pick up newly-added
;;              imports.
;;
;;
;; There's also clj-imports-insert-with-completion, which I think I prefer.
;; This prompts for a regular expression (which is just used to narrow the list
;; of classes to a workable subset--you can hit enter to match everything),
;; then lets you pick a (single) class to import using completion.  For
;; example, with an initial regexp of "lucene" I can very easily tab complete
;; to get my org.apache.lucene.search.IndexSearcher.
;;
;;; Code:


(defun clj-imports--find-ns-expression ()
  "Return the position of the first sexp inside the (ns ...).
E.g.:  (ns X)
           ^"
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^(ns " nil t)))


(defmacro clj-imports--with-point-at-ns (var &rest body)
  "Eval `body' with the point positioned inside the (ns) expression.
Throws an exception if there is no (ns) expression."
  `(let ((,var (clj-imports--find-ns-expression)))
    (when (not ,var)
      (error "Couldn't find an (ns ...) expression"))
    (goto-char ,var)
    ,@body))


(defun clj-imports--find-import-entry ()
  "Move the point to the (:import) expression within (ns).
Returns true if an (:import) was present, nil otherwise.
If there is no (:import), the point is left at the end of the (ns)."
  (goto-char (clj-imports--find-ns-expression))
  (while (and (not (looking-at "[[:space:]\n]*(:import"))
              (ignore-errors (scan-sexps (point) 1)))
    (goto-char (ignore-errors (scan-sexps (point) 1))))
  (looking-at "[[:space:]\n]*(:import"))


(defun clj-imports--fix-imports-at-point ()
  "Reformats and de-dupes the (import ...) or (:import ...) after point."
  (let ((imports (first
                  (read-from-string (buffer-substring
                                     (point)
                                     (ignore-errors
                                       (scan-sexps (point) 1)))))))
    (kill-sexp 1)
    (when (not (looking-back "^[[:space:]]*"))
      (newline-and-indent))
    (clj-imports--reformat imports)))



(defun clj-imports--group-by-packages (imports)
  "Convert a list like ((pkg1 name1 name2 ...) (pkg2 name1 name2 ...)) to a
hash table keyed on packages with lists of names as values."
  (let ((packages (make-hash-table :test 'equal)))
    (mapc (lambda (import)
            (let ((import (if (eq (first import) 'quote)
                              (second import)
                            import)))
              (mapc (lambda (name)
                      (puthash (first import)
                               (cons name
                                     (gethash (first import) packages '()))
                               packages))
                    (rest import))))
          imports)
    packages))


(defun clj-imports--reformat (imports)
  "Insert a cleaned up version of `imports'."
  (insert (format "(%s" (first imports)))
  (let ((packages (clj-imports--group-by-packages (rest imports)))
        (start (point)))
    (maphash (lambda (pkg names)
               (newline-and-indent)
               (insert (format (if (eq (first imports) 'import)
                                   "'%s" ; a standalone (import)
                                 "%s")   ; an import within an ns
                               (cons pkg (sort names #'string<)))))
             packages)
    (sort-lines nil start (point)))
  (insert ")"))


(defun clj-imports--prepare-for-ns-imports ()
  "Jump to the (ns) expression, inserting a new (:import) expression if there
isn't one already.  Leaves the point inside the (:import ...) expression."
  (clj-imports--with-point-at-ns ns
    (set-mark-command nil)
    (cond ((clj-imports--find-import-entry)
           (down-list)
           (forward-sexp)
           (newline-and-indent))
          (t
           (newline-and-indent)
           (insert "(:import)")
           (backward-char 1)
           (newline-and-indent)))))


(defun clj-imports--prepare-import-expression ()
  "Insert a new (import) expression leaving the point positioned inside."
  (when (not (looking-back "^[[:space:]]*"))
    (newline-and-indent))
  (insert "(import)")
  (backward-char 1)
  (newline-and-indent))


(defun clj-imports-tidy-ns-imports ()
  "Clean up the (:import) expression for the current namespace."
  (interactive)
  (save-excursion
    (clj-imports--with-point-at-ns ns
      (clj-imports--find-import-entry)
      (clj-imports--fix-imports-at-point))))


(defun clj-imports-eval-ns ()
  "Re-evaluate the current (ns) expression."
  (interactive)
  (save-excursion
    (clj-imports--with-point-at-ns ns
      (backward-up-list)
      (slime-eval-defun))))


(defun clj-imports--do-insert (imports)
  "Add `imports' to the buffer."
  (if current-prefix-arg
      (clj-imports--prepare-import-expression)
    (clj-imports--prepare-for-ns-imports))
  (mapc (lambda (class)
          (when (string-match "^\\(.*\\)\\.\\([^\\.]+\\)$" class)
            (let ((pkg (match-string 1 class))
                  (name (match-string 2 class)))
              (insert (format "(%s %s)\n" pkg name)))))
        imports)
  (backward-up-list)
  (clj-imports--fix-imports-at-point))


(defun clj-imports--get-default ()
  (if (featurep 'thingatpt)
      (thing-at-point 'symbol)
    nil))

(defun clj-imports-insert (re)
  "Insert an (:import) entry for any class matching `re'.
With a prefix argument, insert a standalone (import) expression instead."
  (interactive (list (read-from-minibuffer "Class regex?: "
                                           (clj-imports--get-default))))
  (save-excursion
    (let* ((s (slime-eval
               `(swank:eval-and-grab-output
                 ,(format "(distinct (filter #(re-find #\"%s\" %%)
                                     (map :name
                                          swank.util.class-browse/available-classes)))"
                          re))))
           (imports (car (read-from-string (second s)))))
      (clj-imports--do-insert imports))))


(defun clj-imports-insert-with-completion (re)
  "Insert an (:import) entry for a class, read from the minibuffer.
With a prefix argument, insert a standalone (import) expression instead."
  (interactive (list (read-from-minibuffer "Starting class regex?: "
                                           (clj-imports--get-default))))
  (save-excursion
    (let* ((s (slime-eval
               `(swank:eval-and-grab-output
                 ,(format "(distinct (filter #(re-find #\"%s\" %%)
                                     (map :name
                                          swank.util.class-browse/available-classes)))"
                          re))))
           (imports (car (read-from-string (second s)))))
      (clj-imports--do-insert
       (list (completing-read "Class? " imports nil t))))))


(defalias 'clj-import 'clj-imports-insert)


(provide 'clj-imports)
;;; clj-imports.el ends here
