;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2018 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "25.1")
  (error "Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-personal-dir)
  "This file contains a list of modules that will be loaded by Prelude.")
(defvar prelude-deprecated-modules-file
  (expand-file-name "prelude-modules.el" prelude-dir)
  (format "This file may contain a list of Prelude modules.

This is DEPRECATED, use %s instead." prelude-modules-file))

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

(require 'key-chord)

(key-chord-mode 1)
(evil-mode 1)
(prettify-symbols-mode 1)

(setq default-frame-alist '((font . "Fira Code-13")))

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(with-eval-after-load 'evil-maps
                      (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
                      (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
                      (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
                      (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
                      (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
                      (define-key evil-normal-state-map (kbd "ä") #'delete-other-windows)
                      (define-key evil-normal-state-map (kbd "ö") #'save-buffer)
                      (define-key evil-normal-state-map (kbd "°") #'cider-eval-buffer)
                      (define-key evil-normal-state-map (kbd "§") #'cider-eval-defun-at-point)
                      (define-key evil-normal-state-map (kbd "Ö") #'cider-find-var)
                      (define-key evil-normal-state-map (kbd "C-p") #'projectile-find-file)
                      (define-key evil-normal-state-map (kbd "Ä") #'projectile-ag)
                      (define-key evil-normal-state-map (kbd "¨") #'evil-search-forward)
                      (define-key evil-normal-state-map (kbd "TAB") #'switch-to-prev-buffer)
                      (define-key evil-normal-state-map (kbd "´") #'kill-buffer)
                      (define-key evil-normal-state-map (kbd "K") #'cider-doc)
                      (define-key evil-normal-state-map (kbd "<") #'sp-backward-barf-sexp)
                      (define-key evil-normal-state-map (kbd ">") #'sp-forward-barf-sexp)
                      (define-key evil-normal-state-map (kbd "(") #'sp-backward-slurp-sexp)
                      (define-key evil-normal-state-map (kbd ")") #'sp-forward-slurp-sexp)
                      (define-key evil-normal-state-map (kbd "g c") #'comment-line)
                      (define-key evil-normal-state-map (kbd "SPC ,") #'avy-goto-char)
                      (define-key evil-normal-state-map (kbd "SPC .") #'avy-goto-char-2)
                      (define-key evil-normal-state-map (kbd "SPC h") #'switch-to-prev-buffer)
                      (define-key evil-normal-state-map (kbd "SPC l") #'switch-to-next-buffer))

(defun setup-input-decode-map ()
  (define-key input-decode-map (kbd "SPC x") (kbd "C-x"))
  (define-key input-decode-map (kbd "C-h") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-l") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-n") (kbd "C-x C-f")))

(evil-set-initial-state 'term-mode 'emacs)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(setup-input-decode-map)

(add-hook 'tty-setup-hook #'setup-input-decode-map)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'prelude-macos))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'prelude-linux))

(message "Loading Prelude's modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
  (progn
    (load prelude-modules-file)
    (if (file-exists-p prelude-deprecated-modules-file)
      (message "Loading new modules configuration, ignoring DEPRECATED prelude-module.el")))
  (if (file-exists-p prelude-deprecated-modules-file)
    (progn
      (load prelude-deprecated-modules-file)
      (message (format "The use of %s is DEPRECATED! Use %s instead!"
                       prelude-deprecated-modules-file
                       prelude-modules-file)))
    (message "Missing modules file %s" prelude-modules-file)
    (message "You can get started by copying the bundled example file from sample/prelude-modules.el")))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (delete
                prelude-modules-file
                (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$"))))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(message "Halipazuippa, mestarini %s" current-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
                        (defun enriched-decode-display-prop (start end &optional param)
                          (list start end))))

(prelude-eval-after-init
  ;; greet the use with some useful tip
  (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here
