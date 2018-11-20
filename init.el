(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")

                         ("org" . "http://orgmode.org/elpa/")
		         ("gnu"  . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(use-package doom-themes
  :ensure t
  :config
  (require 'doom-themes)
  (load-theme 'doom-one t)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (doom-themes-org-config))

(use-package ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-file-extensions-order '(".clj" ".cljs" ".tf" ".org" ".el" ".py" ".txt"))
  (ido-mode t))

(use-package ido-vertical-mode
  :ensure t
  :pin melpa-stable
  :init
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))


(use-package paredit
  :ensure t
  :pin melpa-stable
  :diminish (paredit-mode . " Ⓟ")
  :bind (("C-d" . duplicate-sexp)
         ("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square)
         ("<C-M-up>" . transpose-sexp-backward)
         ("<C-M-down>" . transpose-sexp-forward)
         ("<M-S-left>" . backward-sexp)
         ("<M-S-right>" . forward-sexp))
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)

  (defun duplicate-sexp ()
    "Duplicates the sexp at point."
    (interactive)
    (save-excursion
      (forward-sexp)
      (backward-sexp)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (insert (concat (buffer-substring (car bounds) (cdr bounds)) "\n"))
        (indent-for-tab-command))))

  (defun transpose-sexp-forward ()
    (interactive)
    (forward-sexp)
    (transpose-sexps 1)
    (backward-sexp))

  (defun transpose-sexp-backward ()
    (interactive)
    (forward-sexp)
    (transpose-sexps -1)
    (backward-sexp))

  :config
  (define-key paredit-mode-map "\C-d" 'duplicate-sexp))


(use-package magit
  :ensure t
  :pin melpa-stable
  :config
  (global-set-key (kbd "C-c C-g") 'magit-status)

  (setq git-commit-fill-column 3000
        git-commit-finish-query-functions nil
        git-commit-summary-max-length 120
        magit-log-margin '(t "%Y-%m-%d " magit-log-margin-width t 18)))

(use-package clojure-mode
  :ensure t
  :pin melpa-stable
  :diminish (clojure-mode . "clj")
  :defines clojure-mode-map
  :bind (("C-x t" . clojure-jump-to-test)
         ("C-c C-w" . cider-eval-last-sexp-and-replace)
         ("C-c M-e" . cider-eval-print-last-sexp))
  :mode (("\\.edn$" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook
          (lambda ()
            (setq mode-name "λ")))

  (defun ss/string-join (sep s)
    (mapconcat 'identity s sep))

  (defun toggle-test-path (path)
    (ss/string-join
     "/"
     (mapcar
      (lambda (x)
        (cond ((string-equal x "test") "src")
              ((string-equal x "src") "test")

              ((string-equal x "src-cljs") "test-cljs")
              ((string-equal x "test-cljs") "src-cljs")

              ((string-match "\\(.+\\)_test\\.clj\\(.?\\)" x)
               (concat (match-string 1 x) ".clj" (match-string 2 x)))
              ((string-match "\\(.+\\)\\.clj\\(.?\\)" x)
               (concat (match-string 1 x) "_test.clj" (match-string 2 x)))

              (t x)))
      (split-string path "/"))))

  (defun clojure-jump-to-test ()
    "Jump to corresponding test buffer (or the corresponding src buffer if you're in a test.)"
    (interactive)
    (find-file (toggle-test-path buffer-file-name))))

(use-package cider
  :ensure t
  :defer t
  :diminish (cider-mode . " ⓒ")
  :pin melpa-stable
  :bind (("C-c M-o" . cider-repl-clear-buffer)
         ("C-x M-e" . cider-pprint-eval-last-sexp-to-repl))
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  :config

  (setq cider-prompt-for-symbol nil)
  (setq cider-font-lock-dynamically nil)
  (setq cider-repl-use-pretty-printing nil)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-show-error-buffer 'except-in-repl)
  (setq cider-repl-display-help-banner nil)
  (setq cider-inject-dependencies-at-jack-in nil)
  (setq nrepl-prompt-to-kill-server-buffer-on-quit nil)

  (bind-key "C-c M-o" 'cider-repl-clear-buffer cider-repl-mode-map))

(use-package paren
  :init
  (add-hook 'lisp-mode-hook 'show-paren-mode)
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
  (add-hook 'scheme-mode-hook 'show-paren-mode)
  (add-hook 'cider-repl-mode-hook 'show-paren-mode)
  (add-hook 'clojure-mode-hook 'show-paren-mode)
  (custom-set-faces
   '(show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold))))))

(use-package align-cljlet
  :ensure t
  ;;:pin marmalade
  :init
  (add-hook 'clojure-mode-hook
            '(lambda ()
               (define-key clojure-mode-map "\C-c\C-a" 'align-cljlet))))

(use-package s
  :ensure t)

;; misc
(use-package deadgrep
  :ensure t
  :bind ("<f9>" . deadgrep))


(use-package undo-tree
  :ensure t
  :pin gnu
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)

  (custom-set-faces
   '(undo-tree-visualizer-active-branch-face ((t (:background "#002b36" :foreground "gray95" :weight bold))))))

;;super-slow-scroll
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; one two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-conservatively 10000)


(global-set-key (kbd "C-=") (lambda () (interactive) (text-scale-increase 0.5)))
(global-set-key (kbd "C--") (lambda () (interactive) (text-scale-increase -0.5)))
(global-set-key (kbd "C-0") (lambda () (interactive) (text-scale-increase 0)))

(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key [f7] 'toggle-truncate-lines)

(require 'kebab)

;;GT-specific tools
(require 'gt)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (undo-tree deadgrep use-package s paredit magit ido-vertical-mode doom-themes cider align-cljlet))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold)))))
