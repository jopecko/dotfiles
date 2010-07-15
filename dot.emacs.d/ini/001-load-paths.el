;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-001-load-paths.el
;;; Purpose: Define the default load paths used by emacs.
;;; ==================================================================

;;; Helper functions -------------------------------------------------

(defun jw-filter (fn list)
  (cond ((null list) ())
        ((apply fn (list (car list)))
         (cons (car list) (jw-filter fn (cdr list))))
        (t (jw-filter fn (cdr list)))))

(defun jw-hidden-p (fn)
  (string-equal "." (substring fn 0 1)))

(defun jw-ignore-p (fn)
  (or (jw-hidden-p fn)
      (string-equal "inactive" fn)))

(defun load-directories-in (dir-name)
  (mapcar 'add-to-load-path
          (jw-filter 'file-directory-p
                  (mapcar '(lambda (fn) (concat dir-name "/" fn))
                          (jw-filter '(lambda (fn) (not (jw-ignore-p fn)))
                                  (directory-files dir-name))))))

(defun load-package-directories (path)
  (let ((dirname (concat emacs-dir "/" path)))
    (add-to-load-path dirname)
    (load-directories-in dirname)))

;;; Set the load paths -----------------------------------------------

(load-package-directories "vendor")
;;(load-package-directories "local-pkgs")
