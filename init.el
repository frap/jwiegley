;; -*- lexical-binding: t -*-

;; This file wraps the primary Atea configuration (which lives in
;; core.el) so that we don't have to wrap the entire file in various
;; `let' forms, etc. We put as much as possible in core.el.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later with
;; `doom-restore-garbage-collection-h'. Not resetting it will cause
;; stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritise non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  ;; Ensure Doom is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Load the heart of Doom Emacs
(load (concat user-emacs-directory "core/core")
      nil 'nomessage)

;; And let 'er rip!
;;(atea-initialise)

