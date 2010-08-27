;;; ==================================================================
;;; File:    ini-essential.el
;;; Purpose: Essential Emacs Functions and bindings
;;; ==================================================================

;;; Detect emacs version =============================================

(defun is-emacs-19 ()
  (string-equal (substring emacs-version 0 2) "19"))

(defun is-emacs-19-25 ()
  (string-equal (substring emacs-version 0 5) "19.25"))

(defun is-emacs-20 ()
  (string-equal (substring emacs-version 0 2) "20"))

(defun is-emacs-21 ()
  (string-equal (substring emacs-version 0 2) "21"))

(defun is-emacs-22 ()
  (string-equal (substring emacs-version 0 2) "22"))

(defun is-emacs-23 ()
  (string-equal (substring emacs-version 0 2) "23"))

(defun is-xemacs ()
  (string-match "XEmacs" emacs-version))

(defun is-aquamacs ()
  (boundp 'aquamacs-version))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; what kind of system are we using? start with these, as it will influence
;; other stuff; inspired by: http://www.steve.at/prg/emacs/emacs.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst win32-p (eq system-type 'windows-nt)
  "Are we running on a Windoze system?")
(defconst linux-p (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")
(defconst console-p (eq (symbol-value 'window-system) nil)
  "Are we running in a console (non-X) environment?")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require-soft (http://www.emacswiki.org/cgi-bin/wiki/LocateLibrary)
;; this is useful when this .emacs is used in an env where not all of the
;; other stuff is available
(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if 'require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Define autolist ==================================================

(defun make-auto (pattern mode)
  "Add a pattern to the auto-mode alist."
  (let ((ans (assoc pattern auto-mode-alist)))
    (if (and ans (equal mode (cdr ans)))
	(print "Do Nothing")
      (print "Added")
      (setq auto-mode-alist
	    (cons (cons pattern mode) auto-mode-alist)))))

;;;_____________________________________________________________________
;;; Customized Variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
(line-number-mode t)                     ; show line numbers
(column-number-mode t)                   ; show column numbers
(when (fboundp size-indication-mode)
  (size-indication-mode t))              ; show file size (emacs 22+)
(display-time-mode -1)                   ; don't show the time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(mouse-avoidance-mode 'jump)             ; mouse ptr when cursor is too close
(icomplete-mode t)                       ; completion in minibuffer
(setq icomplete-prospects-height 2)      ; don't spam my minibuffer

(partial-completion-mode t)              ; be smart with completion

(setq scroll-margin 1                    ; do smooth scrolling, ...
  scroll-conservatively 100000           ; ... the defaults ...
  scroll-up-aggressively 0.01            ; ... are very ...
  scroll-down-aggressively 0.01)         ; ... annoying

(when (fboundp 'set-fringe-mode)         ; emacs22+
  (set-fringe-mode 1))                   ; space left of col1 in pixels

(transient-mark-mode t)                  ; make the current 'selection' visible
(delete-selection-mode t)                ; delete the selection with a keypress
(setq x-select-enable-clipboard t        ; copy-paste should work ...
  interprogram-paste-function            ; ...with...
  'x-cut-buffer-or-selection-value)      ; ...other X clients

(setq search-highlight t                 ; highlight when searching..
  query-replace-highlight t)             ; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ; enable y/n answers to yes/no

(global-font-lock-mode t)                ; always do syntax highlighting

(setq completion-ignore-case t           ; ignore case when completing...
  read-file-name-completion-ignore-case t) ; ...filenames too

(put 'narrow-to-region 'disabled nil)    ; enable...
(put 'erase-buffer 'disabled nil)        ; ... useful things
(when (fboundp file-name-shadow-mode)    ; emacs22+
  (file-name-shadow-mode t))             ; be smart about filenames in mbuf

(setq inhibit-startup-message t          ; don't show ...
  inhibit-startup-echo-area-message t)   ; ... startup messages

(setq require-final-newline t)           ; end files with a newline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf8 / input-method
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saving things across sessions
;;
;; backups
(setq backup-dir (concat tmp-dir "/backups"))
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  backup-directory-alist '(("." . "~/.emacs.tmp/backups")) ;; FIXME
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc small stuff

; highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default

;; show-paren-mode: subtle blinking of matching paren (defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis) ; alternatives 'parenthesis' and 'mixed'
  (set-face-background 'show-paren-match-face (face-background 'default))
  (set-face-foreground 'show-paren-match-face "white")
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
  (set-face-foreground 'show-paren-mismatch-face "red")
  (set-face-attribute 'show-paren-mismatch-face nil
                      :weight 'extra-bold
                      :underline t
                      :overline nil
                      :slant 'normal))

(setq visible-bell t)             ; turn the damn beef off, but make it visible
(when (require-soft 'jit-lock)    ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21

;; don't commit trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default default-indicate-empty-lines t)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; enables 'a' so as to avoid accumulation of Dired buffers
(put 'dired-find-alternate-file 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; set frame title / icon title using filename or buffername
;; little trick (based on http://www.emacswiki/.org/cgi-bin/wiki/ShadyTrees)
;; to replace /home/foo with ~
(defun my-title-format ()
  (if buffer-file-name
      (replace-regexp-in-string
       "\\\\" "/"
       (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~"
                                 (convert-standard-filename buffer-file-name)))
    (buffer-name)))
(setq
 frame-title-format '(:eval (my-title-format))
 icon-title-format  '(:eval (concat "emacs:" (my-title-format))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window-system (i.e. _not_ console) specific settings
;;
(when (not console-p)

  ;; highlight the current line; set a custom face, so we can
  ;; recognize it from the normal marking (selection)
  ;; don't turn it on globally, only in specific modes (see my-c-mode-hook)
;;   (when-available 'global-hl-line-mode
;; 		  (progn
;; 		    (defface hl-line '((t (:background "Gray")))
;; 		      "Face to use for 'hl-line-face'." :group 'hl-line)
;; 		    (setq hl-line-face 'hl-line)
;; 		    (global-hl-line-mode t))) ; turn it on for all modes by default
  (when-available 'scroll-bar-mode
		  (progn
		    (scroll-bar-mode t)              ; show the scroll bar...
		    (set-scroll-bar-mode 'right)))   ; ...on the right side

  (when-available 'menu-bar-mode
                  (progn
                    (menu-bar-mode  -1)))            ; don't show the menu...

  (when-available 'tool-bar-mode
                  (tool-bar-mode -1))                ; ... nor the toolbar

  (when-available 'global-highlight-changes-mode
                  (progn
                    (global-highlight-changes-mode t)
                    (setq highlight-changes-visibility-initial-state nil)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time/date/calendar stuff
(setq
  diary-file  "~/.diary"
  holidays-in-diary-buffer          t
  mark-holidays-in-calendar         t
  all-christian-calendar-holidays   t        ;; show christian
  all-islamic-calendar-holidays     nil      ;; don't show islamic
  all-hebrew-calendar-holidays      nil      ;; don't show hebrew
  display-time-24hr-format          t        ;; use 24h format
  display-time-day-and-date         nil      ;; don't display time
  display-time-format               nil      ;;
  display-time-use-mail-icon        nil      ;; don't show mail icon
  calendar-latitude                 40.521634     ;; my...
  calendar-longitude                -74.862907     ;; ...position
  calendar-location-name "Flemington, NJ")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'flymake)
