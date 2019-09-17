;;; settings.el --- Custom personal settings
;;

(require 'key-chord)

(key-chord-mode 1)
(evil-mode 1)
(evil-commentary-mode)
(prettify-symbols-mode 1)

(require 'smex) ; Not needed if you use package.el
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(setq default-frame-alist '((font . "Fira Code-13")))

(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(setq backup-directory-alist `(("." . "~/.saves")))

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
                      (define-key evil-normal-state-map (kbd "<backtab>") #'switch-to-next-buffer)
                      (define-key evil-normal-state-map (kbd "´") #'kill-buffer)
                      (define-key evil-normal-state-map (kbd "K") #'cider-doc)
                      ;; (define-key evil-normal-state-map (kbd "<") #'sp-backward-barf-sexp)
                      ;; (define-key evil-normal-state-map (kbd ">") #'sp-forward-barf-sexp)
                      ;; (define-key evil-normal-state-map (kbd "(") #'sp-backward-slurp-sexp)
                      ;; (define-key evil-normal-state-map (kbd ")") #'sp-forward-slurp-sexp)
                      ;; (define-key evil-normal-state-map (kbd "g c") #'comment-line)
                      (define-key evil-normal-state-map (kbd "SPC ,") #'avy-goto-char)
                      (define-key evil-normal-state-map (kbd "SPC .") #'avy-goto-char-2)
                      (define-key evil-normal-state-map (kbd "SPC h") #'switch-to-prev-buffer)
                      (define-key evil-normal-state-map (kbd "SPC l") #'switch-to-next-buffer))

(defun setup-input-decode-map ()
  (define-key input-decode-map (kbd "SPC x") (kbd "C-x"))
  (define-key input-decode-map (kbd "C-h") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-l") (kbd "C-x o"))
  (define-key input-decode-map (kbd "C-b") (kbd "C-x b"))
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

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'linum-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
