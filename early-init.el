;;; early-init.el -*- lexical-binding: t; -*-

;; This file is loaded before package.el is initialized, and before
;; the first graphical frame is initialized, by Emacs 27 (but not by
;; any previous version of Emacs).
;;
;; If the early init-file is available, we actually execute our entire
;; init process within it, by just loading the regular init-file.
;; (That file takes care of making sure it is only loaded once.)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialisation occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

(defun radian--advice-fix-display-graphic-p (func &optional display)
    "Fix `display-graphic-p' so it works while loading the early init-file."
    (if display
        (funcall func display)
      ;; `display-graphic-p' lies by returning nil, but
      ;; `initial-window-system' tells the truth (it is nil only if we
      ;; are actually in a tty environment).
      initial-window-system))

(advice-add #'display-graphic-p :around
              #'radian--advice-fix-display-graphic-p)

(defun radian--advice-disable-x-resource-application ()
    "Disable `x-apply-session-resources'.
Now, `x-apply-session-resources' normally gets called before
reading the init-file. However if we do our initialization in the
early init-file, before that function gets called, then it may
override some important things like the cursor color. So we just
disable it, since there's no real reason to respect X
resources.")

(advice-add #'x-apply-session-resources :override
            #'radian--advice-disable-x-resource-application)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)
