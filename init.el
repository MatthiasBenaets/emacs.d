(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t
      use-dialog-box nil)

;;(setq gc-cons-threshold (* 100 1000 1000))
(setq gc-cons-threshold most-positive-fixnum)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
        (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(use-package esup)
(setq esup-depth 0)

(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)

(recentf-mode 1)
(save-place-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package helpful
  :custom
  (add-hook describe-function-function #'helpful-callable)
  (add-hook describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package expand-region
  :bind (("M-[" . er/contract-region)
         ("M-]" . er/expand-region)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-SPC") 'other-window)
(global-set-key (kbd "C-+") 'text-scale-adjust)
(global-set-key (kbd "C--") 'text-scale-adjust)
(global-set-key (kbd "C-0") 'text-scale-adjust)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system nil)
  :preface
  (defun mb/write-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))
  :config
  (define-key evil-motion-state-map (kbd "/") 'consult-line)
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'mb/write-kill-this-buffer))
(setq-default evil-shift-width tab-width)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer mb/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (mb/leader-keys
    "e"  '(:ignore t :which-key "eval")
    "eb" 'eval-buffer
    "er" 'eval-region
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(load-theme :which-key "load-theme")
    "q"  'delete-other-windows
    "h"  '(help-command :which-key "help")))

(blink-cursor-mode 1)
(global-hl-line-mode t)
(global-visual-line-mode)

(setq visible-bell t
      word-wrap t
      visual-line-fringe-indicators nil)

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-preserve-screen-position 'always
      scroll-conservatively 101
      scroll-margin 8
      scroll-step 1)

(column-number-mode)
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 1))))

(dolist (mode '(org-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode 1)
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package dashboard
  :if (< (length command-line-args) 2)
  :init
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
        dashboard-items '((recents . 10)
                          (bookmarks . 5))
        dashboard-banner-logo-title "Welcome back Matthias"
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t)
  :config
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config))

(set-face-attribute 'default nil
                    :font "Source Code Pro"
                    :weight 'normal
                    :height 115)

(set-face-attribute 'font-lock-comment-face nil
                    :font "Source Code Pro"
                    :slant 'italic
                    :weight 'normal
                    :height 110)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 5)
  (doom-modeline-buffer-name t)
  (doom-modeline-buffer-file-name-style 'truncate-nil)
  (doom-modeline-icon t))

(use-package all-the-icons)
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous);)
              ("RET" . vertico-directory-enter)
              ("<backspace>" . vertico-directory-delete-char))
  :init
  (setq vertico-count 8
        vertico-cycle t)
        (vertico-mode))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-f" . consult-buffer-other-window)
         ("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("M-s" . consult-history))
  :config
  (mb/leader-keys
    "s"  '(consult-line :which-key "search")
    "b"  '(:ignore f :which-key "buffer")
    "bb" '(consult-buffer :which-key "buffer")
    "b." '(consult-buffer-other-window :which-key "buffer-other-window")
    "."  'find-file
    "f"  '(:ignore f :which-key "files")
    "ff" 'find-file
    "f." 'find-file-other-window
    "fr" '(consult-recent-file :which-key "recent-file")))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package savehist
  :init
  (setq history-length 50)
        (savehist-mode))

(defun mb/org-mode ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook (org-mode . mb/org-mode)
  :commands org-babel-do-load-languages
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers nil
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation t
        org-hide-block-startup nil
        org-startup-folded 'showeverything
        ;;org-startup-with-inline-images t
        org-cycle-separator-lines 2)
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 1.20))))
  (org-level-2 ((t (:inherit outline-2 :height 1.15))))
  (org-level-3 ((t (:inherit outline-3 :height 1.10))))
  (org-level-4 ((t (:inherit outline-4 :height 1.08))))
  (org-level-5 ((t (:inherit outline-5 :height 1.05))))
  (org-document-title ((t (:height 2.5)))))

(defun mb/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
        (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . mb/org-mode-visual-fill))

(use-package org-superstar
  :defer 1
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-tempo
  :ensure nil
  :after (org)
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("nix" . "src nix")))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode)))

(use-package git-gutter
  :defer 2
  :config
  (global-git-gutter-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (mb/leader-keys
    "g"  '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gc" 'magit-branch-or-checkout
    "gb" 'magit-branch
    "gp" 'magit-pull-branch
    "gP" 'magit-push-current
    "gf" 'magit-fetch
    "gF" 'magit-fetch-all
    "gr" 'magit-rebase))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0))

(use-package lisp-mode
  :ensure nil
  :mode "\\.el\\'"
  :hook ((lisp-mode . company-mode)))

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode . lsp-deferred)))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode))
  :hook ((yaml-mode . company-mode)))

(setq gc-cons-threshold (* 2 1000 1000))
