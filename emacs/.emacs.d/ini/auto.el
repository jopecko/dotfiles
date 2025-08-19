;;; ==================================================================
;;; Author:  Jim Weirich
;;; File:    ini-auto
;;; Purpose: Define the Autoload functions and the auto-mode list
;;; ==================================================================

(setq default-major-mode 'indented-text-mode)

;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(autoload 'html-mode        "html-mode"        "HTML major mode." t)
(autoload 'tcl-mode         "tcl-mode"         "Major Mode for TCL" t)
(autoload 'python-mode      "python-mode"      "Automatic Mode for Python Code" t)
(autoload 'ruby-mode        "ruby-mode"        "Automatic Mode for Ruby Code" t)
(autoload 'filladapt-mode   "filladapt"        "Adaptive Filling Minor mode" t)

(make-auto "\\.awk$"  'awk-mode)
(make-auto "\\.html$" 'rhtml-mode) ; was html-mode
(make-auto "\\.htm$"  'rhtml-mode)
(make-auto "\\.erb$"  'rhtml-mode)
(make-auto "\\.tcl$"  'tcl-mode)
(make-auto "\\.cpp$"  'c++-mode)
(make-auto "\\.cxx$"  'c++-mode)
(make-auto "\\.h$"    'c++-mode)
(make-auto "\\.hxx$"  'c++-mode)
(make-auto "\\.C$"    'c++-mode)
(make-auto "\\.hxx$"  'c++-mode)
(make-auto "\\.H$"    'c++-mode)
(make-auto "\\.py$"   'python-mode)
(make-auto "\\.pl$"   'perl-mode)
(make-auto "\\.ph$"   'perl-mode)
(make-auto "\\.pm$"   'perl-mode)
(make-auto "\\.java$" 'java-mode)
(make-auto "\\.jav$"  'java-mode)
(make-auto "\\.rb$"   'ruby-mode)
(make-auto "\\.rjs$"   'ruby-mode)
(make-auto "\\.rbw$"   'ruby-mode)
(make-auto "\\.rake$"   'ruby-mode)
(make-auto "Rakefile$" 'ruby-mode)
(make-auto "\\.haml$" 'haml-mode)
(make-auto "\\.sass$" 'sass-mode)

;;; NOTE: .h files to be editted in C++ mode should include a
;;;            // -*-Mode: c++; -*-
;;;        as the first line


