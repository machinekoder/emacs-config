    ;;; Setup package repositories
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

  ;; use-package
  (add-to-list 'load-path "~/.emacs.d/lisp/use-package/")
  (eval-when-compile
    (require 'use-package))
  ;;(require 'diminish)                ;; if you use :diminish
  (require 'bind-key)                ;; if you use any :bind variant
  ;; for use-package functionality
  (use-package deferred :ensure t)

  ;;; General config
  ;; frame title
  (setq frame-title-format "%b")

  ;; disable startup screen
  ;; inhibit-startup-echo-area-message MUST be set to a hardcoded
  ;; string of your login name
  (setq inhibit-startup-echo-area-message "alexander")
  (setq inhibit-startup-message t)

  ;; Enable section delete
  (delete-selection-mode t)
  ;; Emacs cusomization
  (column-number-mode t)
  (scroll-bar-mode t)
  (show-paren-mode t)
(tool-bar-mode -1)
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

  ;; Disable backup files
  (setq make-backup-files nil)

  ;; Create shortcut for kill this buffer
  (bind-key "s-k" 'kill-this-buffer)
  ;; Create short to kill emacs
(bind-key "s-x" 'save-buffers-kill-emacs)

;; Back to identation or beginning (like Eclipse Emacs-Mode)
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
           (beginning-of-line)))
(bind-key "C-a" 'back-to-indentation-or-beginning)


  ;; Tell emacs where is your personal elisp lib dir
  (add-to-list 'load-path "~/.emacs.d/lisp/")

  ;; Spaces instead of tabs
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq tab-stop-list (number-sequence 4 200 4))
  (setq c-default-style "linux"
        c-basic-offset 4)

  ;; Show matching parenthesis
  (show-paren-mode 1)

  ;; Setup line numbers
  (use-package linum
    :config
    ;;(setq linum-format "%4d") ;;"\u23AE")
    (use-package linum-off
      :ensure t
      :config
      (global-linum-mode t)
      )
    (setq linum-disabled-modes-list '(dired-mode
                                      mu4e-headers-mode
                                      mu4e-view-mode
                                      mu4e-compose-mode
                                      mu4e-main-mode
                                      git-commit-mode
                                      magit-status-mode
                                      org-mode
                                      cfw:calendar-mode
                                      deft-mode))
    (defun linum-on ()
      (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
        (linum-mode 1)))
  )

  ;;; Theme
  (use-package base16-theme
    :ensure t
    :init
    ;; tone down the ugly fringes
    (defun my-tone-down-fringes ()
      (set-face-attribute 'fringe nil
                          :foreground (face-foreground 'default)
                          :background (face-background 'default)))

    ;; create hook
    (defvar toggle-theme-hook nil
      "Hook called after the theme is toggled")

    ;; allow theme toggling
    (setq dark-theme-active nil)
    (defun toggle-theme ()
      "remove 4 spaces from beginning of of line"
      (interactive)
      (if dark-theme-active
          (progn
            (load-theme 'base16-github t)
            (setq dark-theme-active nil))
        (progn
          (load-theme 'base16-atelier-forest t)
          (setq dark-theme-active t))
        )
      (my-tone-down-fringes)
      (run-hooks 'toggle-theme-hook)
      )
    (toggle-theme)
    :bind ("s-t" . toggle-theme))

  ;;; Fonts
(set-default-font "Hack-9.5")
;;(set-default-font "Droid Sans Mono Slashed-9.5")
(add-to-list 'default-frame-alist
             '(font . "Hack-9.5"))

  ;;; Flymake
  (use-package flymake
    :ensure t
    :config
    ;; run flymake in place to work with tramp
    (setq flymake-run-in-place nil)
    ;; Setup flymake for Python
    (when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (local-file (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name))))
          (list "~/bin/pycheckers"  (list local-file))))
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.py\\'" flymake-pylint-init)))

    (add-hook 'python-mode-hook
              (lambda ()
                (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
                (local-set-key [f2] 'flymake-goto-prev-error)
                (local-set-key [f3] 'flymake-goto-next-error)
                ))
    )
  ;; Show flymake cursor
  (use-package flymake-cursor :ensure t)

  ;;; Show Whitespace including tabs
  (use-package whitespace
    :config
    (setq whitespace-style (quote (face trailing tabs tab space-before-tab::space space-after-tab::space indentation::space)))
    (global-whitespace-mode t)
    )

  ;; Backtab unindent
  (bind-key "<backtab>" 'un-indent-by-removing-4-spaces)
  (defun un-indent-by-removing-4-spaces ()
    "remove 4 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at "^    ")
                  (replace-match "")))))

  ;; Remember file position
  (use-package saveplace
    :init
    (setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
    (setq-default save-place t)                   ;; activate it for all buffers
    )

  ;; use the same frame speedbar
  (use-package sr-speedbar
    :ensure t
    :init
    (setq speedbar-tag-hierarchy-method '(speedbar-simple-group-tag-hierarchy) )
    :bind
    ("<f11>" . sr-speedbar-toggle))

  ;; setup git-gutter and magit
  (use-package git-gutter+
    :ensure t :ensure git-gutter-fringe+
    :config
    ;; If you enable global minor mode
    (global-git-gutter+-mode t)
    :bind
    ;; Key setup
    ("C-x C-g" . git-gutter+:toggle)
    ("C-x v =" . git-gutter+:popup-hunk)
    ;; Jump to next/previous hunk
    ("C-x p" . git-gutter+:previous-hunk)
    ("C-x n" . git-gutter+:next-hunk)
    ;; Stage current hunk
    ("C-x v s" . git-gutter+:stage-hunk)
    ;; Revert current hunk
    ("C-x v r" . git-gutter+:revert-hunk)
    )

  (use-package magit
    :ensure t
    :config
    ;; disable annoying warning
    (setq magit-last-seen-setup-instructions "1.4.0")
    :bind
    ;; set key binding for magit
    ("C-x g" . magit-status))

  ;; NGC syntax highligthing
  (use-package gcode-mode)

  ;; HAL file syntax highlighting
  (use-package hal-mode
    :config
    ;;(add-hook 'hal-mode-hook (lambda () (electric-indent-local-mode -1)))
    )

  ;;; flyspell
  (use-package flyspell
    :ensure t
    :demand t
    :bind
    ("<f6>" . cycle-ispell-languages)
    :config
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"))

    (setq ispell-dictionary "american")
    (setq ispell-local-dictionary "american")
    (setq flyspell-default-dictionary "american")
    (setq flyspell-issue-message-flag nil) ;; better performance

    ;; german8
    (let ((langs '("american" "german")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))

    (defun cycle-ispell-languages ()
      (interactive)
      (let ((lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang)))

    ;; enable flyspell for several modes
    (add-hook 'git-commit-mode-hook 'flyspell-mode)
    (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'latex-mode-hook 'flyspell-mode)

    ;; for prog modes turn on flyspell-prog-mode (checks spell only in comments)
    (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face)) ;; disable string checking
    (add-hook 'python-mode-hook 'flyspell-prog-mode)
    (add-hook 'c-mode-common-hook 'flyspell-prog-mode))

  ;;; thesaurus
(use-package thesaurus
    :config
    (setq thesaurus-bhl-api-key "527a251f0fadd92810a66d5bd7616bad")  ;; from registration
    :bind
    ("C-c t" . thesaurus-choose-synonym-and-replace))

  ;; phyxcalc mode
  ;;(load "phyxcalc-mode")

  (use-package recentf
    :config
    (add-to-list 'recentf-exclude ".*/Maildir/.*"))

  ;; helm
  (use-package helm
    :ensure t
    :demand t
    :bind
    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (("C-c h" . helm-command-prefix)
    ("M-x" . helm-M-x)
    ("M-y" . helm-show-kill-ring)
    ("C-x b" . helm-mini)
    ("C-x C-f" . helm-find-files)
    ("s-b" . helm-bookmarks)
    ("C-c i" . helm-imenu)
    :map helm-map
    ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
    ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
    ("C-z" . helm-select-action)) ; list actions using C-z
    :config
    (require 'helm-config)
    (use-package helm-google
      :ensure t
      :config
      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))
      )

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t ;
          helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
          helm-buffers-fuzzy-matching           t
          helm-recentf-fuzzy-match              t
          )

    (helm-mode t)

    ;; helm semantic
    ;; C-c h i
    (setq helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match    t)
    )

  ;; helm flyspell
  (use-package helm-flyspell
    :ensure t
    :bind
    (:map flyspell-mode-map
          ("C-x c" . helm-flyspell-correct)))

  ;; helm deft
  (use-package helm-deft
    :load-path "~/.emacs.d/lisp/helm-deft"
    :bind
    ("s-d" . helm-deft)
    :config
    (setq helm-deft-dir-list (list "~/ownCloud/Privat/notes")))

  ;; helm-mu
  (use-package helm-mu
    :load-path "~/.emacs.d/lisp/helm-mu"
    :bind
    ("C-x C-m" . helm-mu))

  (use-package helm-gtags
    :ensure t
    :bind
    ("C-c g a" . helm-gtags-tags-in-this-function)
    ("M-s" . helm-gtags-select)
    ("M-." . helm-gtags-dwim)
    ("M-," . helm-gtags-pop-stack)
    ("C-c <" . helm-gtags-previous-history)
    ("C-c >" . helm-gtags-next-history)
    :config
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t
     )

    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    )

  (use-package helm-open-github
    :ensure t)

  ;;; Golden ratio mode
  (use-package golden-ratio
    :ensure t
    :config
    ;; do not mess with helm
    (defun pl/helm-alive-p ()
      (if (boundp 'helm-alive-p)
          (symbol-value 'helm-alive-p)))
    (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
      ;;(golden-ratio-mode t)
    )

  ;;; Python
  (use-package jedi
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)                 ; optional
    )

   (use-package cython-mode
     :ensure t)

  ;; Nameses
  (use-package nameses
    :ensure desktop+
    :bind
    ("<f9>" . nameses-load)
    ("C-<f9>" . nameses-prev)
    ("C-s-<f9>" . nameses-save)
    :config
    (require 'desktop)
    (require 'desktop+)
    (require 'ido)
    )

  ;;; gsl-minor-mode
  (use-package gsl-mode)

  ;; latex
  (use-package my_latex
    :ensure auctex
    :demand t)

   ;; convert visible dos line endings
   (defun dos2unix (buffer)
     "Automate M-% C-q C-m RET C-q C-j RET"
     (interactive "*b")
     (save-excursion
       (goto-char (point-min))
       (while (search-forward (string ?\C-m) nil t)
         (replace-match (string ?\C-j) nil t))))

   ;; Markdown with live preview
   (use-package markdown-mode
     :ensure t
     :config
     (use-package markdown-mode+ :ensure t)
     )
   (use-package livedown
     :load-path "~/.emacs.d/lisp/emacs-livedown"
     :config
     (setq  markdown-preview-style
            (concat "file://" (expand-file-name "~/emacs.d/github.css"))))

   ;; Dokumentation with Zeal
   (use-package zeal-at-point
     :ensure t
     :config
     (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python"))
     (add-to-list 'zeal-at-point-mode-alist '(angular-mode . "javascript,angularjs"))
     :bind
     ("<f2>" . zeal-at-point))

;; Autocomplete
(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (use-package ac-js2 :ensure t)
  (use-package ac-python :ensure t)
  (use-package ac-html :ensure t)
  (use-package ac-html-angular :ensure t)
  (use-package ac-html-bootstrap :ensure t)
  (use-package ac-etags :ensure t)
  (use-package fuzzy :ensure t)
  (use-package function-args :ensure t
    :config
    (fa-config-default))
  (use-package ac-c-headers :ensure t
    :config
    (add-hook 'c-mode-hook
              (lambda ()
                (add-to-list 'ac-sources 'ac-source-c-headers)
                (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
    )
  (ac-config-default)
  (setq ac-auto-show-menu 0.5)
  (setq ac-delay 0.5)
  (setq ac-menu-height 20)
  (setq ac-use-fuzzy t)
  )

  ;;; Abbrevations
  (use-package pabbrev
    :ensure t
    :config
    (global-pabbrev-mode 0)
    (setq pabbrev-read-only-error nil)

    (require 'popup)
    (defun pabbrevx-suggestions-goto-buffer (suggestions)
      (let* ((candidates (mapcar 'car suggestions))
             (bounds (pabbrev-bounds-of-thing-at-point))
             (selection (popup-menu* candidates
                                     :point (car bounds)
                                     :scroll-bar t)))
        (when selection
          ;; modified version of pabbrev-suggestions-insert
          (let ((point))
            (save-excursion
              (progn
                (delete-region (car bounds) (cdr bounds))
                (insert selection)
                (setq point (point))))
            (if point
                (goto-char point))
            ;; need to nil this so pabbrev-expand-maybe-full won't try
            ;; pabbrev expansion if user hits another TAB after ac aborts
            (setq pabbrev-last-expansion-suggestions nil)
            ))))
    (fset 'pabbrev-suggestions-goto-buffer 'pabbrevx-suggestions-goto-buffer)
    )

;; JavaScript mode
(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))
)

;; Angular
(use-package angular-mode
  :ensure t)

  ;; Autocomplete
  ;;(require 'auto-complete-config)
  ;;(add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs.d/elpa/auto-complete-1.4.20110207/dict"))
  ;;(setq ac-comphist-file (expand-file-name "~/.emacs.d/ac-comphist.dat"))
;;(ac-config-default)

;; Yasnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package angular-snippets :ensure t)
  (use-package auto-yasnippet :ensure t)
  (use-package helm-c-yasnippet :ensure t
    :bind
    ("C-c y" . helm-yas-complete)
    :config
    (setq helm-yas-space-match-any-greedy t))
  (yas-global-mode t)
  (yas-load-directory "~/.emacs.d/snippets/")
  ;; Term mode tab fix
  (add-hook 'term-mode-hook (lambda()
                              (setq yas-dont-activate t)))
  )

;; source code tagging
(use-package ggtags
  :ensure t
  :demand t
  :config
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
  )

;; Project Management
(use-package projectile
  :ensure t
  :demand t
  :config
  (use-package helm-projectile :ensure t)
  (use-package projectile-speedbar :ensure t)
  (projectile-global-mode)
  (helm-projectile-on)
  )

;; Nice colors
(use-package color-identifiers-mode
  :ensure t
  :config
  ;;(add-hook 'after-init-hook 'global-color-identifiers-mode)
  (add-hook 'after-init-hook 'global-color-identifiers-mode)
  (add-to-list 'color-identifiers:modes-alist
               '(angular-mode
                "[^.][[:space:]]*" "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face)) t)
  (add-hook 'toggle-theme-hook
            (lambda ()
              (color-identifiers:regenerate-colors)
              )
            )
  )

;; Highlight Symbol
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.2)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  )

;; Point undo - not sure
(use-package point-undo
  :ensure t)

;; OpenSCAD
(use-package scad-mode
  :ensure t)

;; Protobuf
(use-package protobuf-mode
  :ensure t)

;; QML
(use-package qml-mode
  :ensure t)

;; Gerkin feature mode
(use-package feature-mode
  :ensure t)

;; XML formatting
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

;; Tiny macros
(use-package tiny
  :ensure t)

;; vala mode
(use-package vala-mode
  :ensure t)
