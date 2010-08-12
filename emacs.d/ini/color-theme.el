;; ==================================================================
;; Color Theme
;; ==================================================================
;; Add the color-theme and theme directories to the load path
;(add-to-list 'load-path (concat
(load-package-directories "vendor/color-theme")
(load-package-directories "themes")
;(add-to-list 'load-path (concat emacs-dir "/themes"))

;; Initialize color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(setq color-theme-is-cumulative t)
(setq color-theme-load-all-themes nil)

;; The tangotango theme depends on the tango one
(require 'color-theme-tango)
(require 'color-theme-tangotango)
(color-theme-tangotango)
