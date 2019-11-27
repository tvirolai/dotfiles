;;; settings.el --- Custom personal settings
;;

;; (require 'key-chord)

(require 'flycheck-clj-kondo)

(superword-mode 1)

(add-to-list 'load-path "~/.emacs.d/evil")
; (setq evil-disable-insert-state-bindings t)
(key-chord-mode 1)
(evil-mode 1)
(evil-commentary-mode 1)
(evil-visual-mark-mode 1)
(prettify-symbols-mode 1)

(package-initialize)
(require 'slime-autoloads)

(setq inferior-lisp-program "/usr/local/bin/sbcl" ; Steel Bank Common Lisp
      slime-contribs '(slime-fancy))

(require 'smex) ; Not needed if you use package.el
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-company-mode)

(global-undo-tree-mode)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq default-frame-alist '((font . "Fira Code-13")))

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(setq sentence-end-double-space nil)

(setq backup-directory-alist `(("." . "~/.saves")))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-u") #'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
  (define-key evil-normal-state-map (kbd "ä") #'delete-other-windows)
  (define-key evil-normal-state-map (kbd "C-ä") #'split-window-right)
  (define-key evil-normal-state-map (kbd "ö") #'save-buffer)
  (define-key evil-normal-state-map (kbd "C-p") #'projectile-find-file)
  (define-key evil-normal-state-map (kbd "Ä") #'projectile-ag)
  (define-key evil-normal-state-map (kbd "¨") #'evil-search-forward)
  (define-key evil-normal-state-map (kbd "TAB") #'switch-to-prev-buffer)
  (define-key evil-normal-state-map (kbd "<backtab>") #'switch-to-next-buffer)
  (define-key evil-normal-state-map (kbd "´") #'kill-buffer)
  (define-key evil-normal-state-map (kbd "SPC ,") #'avy-goto-char)
  (define-key evil-normal-state-map (kbd "SPC .") #'avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "SPC h") #'switch-to-prev-buffer)
  (define-key evil-normal-state-map (kbd "SPC l") #'switch-to-next-buffer)
  (define-key evil-normal-state-map (kbd "Q") #'kill-buffer-and-window)
  (define-key evil-normal-state-map (kbd "C-M-b") #'ibuffer))

(defun setup-input-decode-map ()
  (define-key input-decode-map (kbd "SPC x") (kbd "C-x"))
  (define-key input-decode-map (kbd "SPC c") (kbd "C-c"))
  (define-key input-decode-map (kbd "SPC f") (kbd "C-f"))
  (define-key input-decode-map (kbd "C-h") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-l") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-b") (kbd "C-x b"))
  (define-key input-decode-map (kbd "C-n") (kbd "C-x C-f"))
  (define-key input-decode-map (kbd "C-M-<left>") (kbd "C-x <left>"))
  (define-key input-decode-map (kbd "C-M-<right>") (kbd "C-x <right>")))

(evil-set-initial-state 'term-mode 'emacs)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

(setup-input-decode-map)

(add-hook 'tty-setup-hook #'setup-input-decode-map)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(recentf-mode 1)

(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Clojure settings

(defun clojure-mappings ()
  (define-key evil-normal-state-map (kbd "°") #'cider-eval-buffer)
  (define-key evil-normal-state-map (kbd "M-§") #'cider-eval-buffer)
  (define-key evil-normal-state-map (kbd "§") #'cider-eval-defun-at-point)
  (define-key evil-normal-state-map (kbd "Ö") #'cider-find-var)
  (define-key evil-normal-state-map (kbd "q") #'cider-popup-buffer-quit)
  (define-key evil-normal-state-map (kbd "K") #'cider-doc))

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'linum-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'flycheck-mode)
(add-hook 'clojure-mode-hook #'clojure-mappings)

;; Emacs Lisp settings

(defun elisp-mappings ()
  (define-key evil-normal-state-map (kbd "°") #'eval-buffer)
  (define-key evil-normal-state-map (kbd "M-§") #'eval-buffer)
  (define-key evil-normal-state-map (kbd "§") #'eval-defun))

(defun clisp-mappings ()
  (define-key evil-normal-state-map (kbd "°") #'slime-eval-buffer)
  (define-key evil-normal-state-map (kbd "M-§") #'slime-eval-buffer)
  (define-key evil-normal-state-map (kbd "§") #'slime-eval-defun))

(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'clisp-mappings)

(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'elisp-mappings)

(defun kill-magit-diff-buffer-in-current-repo (&rest _)
  "Delete the magit-diff buffer related to the current repo"
  (let ((magit-diff-buffer-in-current-repo
         (magit-mode-get-buffer 'magit-diff-mode)))
    (kill-buffer magit-diff-buffer-in-current-repo)))
;;
;; When 'C-c C-c' is pressed in the magit commit message buffer,
;; delete the magit-diff buffer related to the current repo.
;;
(add-hook 'git-commit-setup-hook
          (lambda ()
            (add-hook 'with-editor-post-finish-hook
                      #'kill-magit-diff-buffer-in-current-repo
                      nil t)))

(with-eval-after-load 'magit
  (defun mu-magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))

  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map))

(defun enable-evil ()
  (evil-mode 1))

(evil-set-initial-state 'cider-repl-mode 'emacs)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(evil-set-initial-state 'cider-test-report-mode 'emacs)
(evil-set-initial-state 'slime-repl-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(add-hook 'slime-repl-mode-hook #'paredit-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; Use system tmp directory for backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Purge old backup files

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))
