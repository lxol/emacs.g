; lxol.el --- user specific init file       -*- lexical-binding: t -*-
(autoload 'if-let "subr-x")
(use-package lxol-config
  :init
  (provide 'lxol-config)
  :bind
  (
   ("s-j" . windmove-down)
   ("s-k" . windmove-up)
   ("s-h" . windmove-left)
   ("s-l" . windmove-right)
   ("s-C-j" . shrink-window)
   ("s-C-k" . enlarge-window)
   ("s-C-h" . shrink-window-horizontally)
   ("s-C-l" . enlarge-window-horizontally)
   ("M-<return>" . ansi-term)
   )
  :config
  (setq
   fringe-mode 1
   make-backup-files nil
   dired-dwim-target t
   create-lockfiles nil
   make-backup-files nil
   column-number-mode t
   scroll-error-top-bottom t
   electric-indent-mode 0
   ;; enable-recursive-minibuffers t
   backup-directory-alist `((".*" . ,temporary-file-directory)) ;don't clutter my fs and put backups into tmp
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   ;; browse-url-browser-function 'browse-url-firefox
   browse-url-browser-function 'browse-url-default-browser
   require-final-newline t        ;auto add newline at the end of file
   major-mode 'text-mode  ;use text mode per default
   history-length 250             ;default is 30
   locale-coding-system 'utf-8    ;utf-8 is default
   tab-always-indent 'complete    ;try to complete before identing
   recentf-max-saved-items 5000     ;same up to 5000 recent files
   eval-expression-print-length nil ;do not truncate printed expressions
   eval-expression-print-level nil  ;print nested expressions
   kill-ring-max 5000           ;truncate kill ring after 5000 entries
   mark-ring-max 5000           ;truncate mark ring after 5000 entries
   split-height-threshold 110   ;more readily split horziontally
   enable-recursive-minibuffers nil
   load-prefer-newer t           ;prefer newer .el instead of the .elc
   split-width-threshold 160 ;split horizontally only if less than 160 columns
   gc-cons-percentage 0.3    ;increase garbage collection limit
   switch-to-buffer-preserve-window-point t
   visible-bell t
   ring-bell-function 'ignore
   cursor-type t
   display-time-default-load-average nil
   custom-file "/tmp/custom-file.el" ;don't pollute the init file and don't `load' the customs
                                      ;but keep them for reference...
   scroll-conservatively 100
   )

  (display-time-mode t)
  (when window-system (global-hl-line-mode t))

  (setq org-confirm-elisp-link-function nil)
  ;; don't ask to kill buffers
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))
  ;; scratch is immortal
  (add-hook 'kill-buffer-query-functions
            (lambda () (not (member (buffer-name) '("*scratch*" "scratch.el")))))

  ;; default flags
  ;; buffer local variables
  (setq-default
   tab-width 4
   indicate-buffer-boundaries 'left     ;fringe markers
   indent-tabs-mode nil)                ;use spaces instead of tabs

  ;; disable full `yes' or `no' answers
  (defalias 'yes-or-no-p 'y-or-n-p)

  (defun copy-file-name-to-clipboard ()
    "Copy the current buffer file name to the clipboard."
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new filename)
        (message "Copied buffer file name '%s' to the clipboard." filename))))

  (defun qmk-align-planck-layer (start end)
    "Align region with planck qmk layer."
    (interactive "r")
    (save-excursion
      (goto-char start)
      (while (re-search-forward " *, *" end t)
        (replace-match " , " t t))
      (goto-char start)
      (while (re-search-forward "{ *" end t)
        (replace-match "{ " t t))
      (goto-char start)
      (while (re-search-forward " *}" end t)
        (replace-match " }" t t))
      (align-regexp start end ",\\(\\s-*\\)" 1 1 t)))

 (defun go-init ()
    "Open init file."
    (interactive)
    (find-file "~/.emacs.d/lxol.el"))

 (defun go-org ()
    "Open init file."
    (interactive)
    (find-file "~/org"))

 (defun go-dotfiles ()
    "Open init file."
    (interactive)
    (find-file "~/.dotfiles"))

 (defun go-i3 ()
    "Open init file."
    (interactive)
    (find-file "~/.dotfiles/i3"))

  (defun exit ()
    "Shorthand for DEATH TO ALL PUNNY BUFFERS!"
    (interactive)
    (if (daemonp)
        (message "You silly")
      (save-buffers-kill-emacs)))

  (defun xrandr-right-vertical ()
      "Packed-pixel display on the right, vertical"
    (interactive)
    (call-process "xrandr" nil nil nil
                  "--output" "eDP1"
                  "--mode" "2560x1440"
                  "--output" "DP1"
                  "--rotate" "left"
                  "--right-of" "eDP1"))

  (defun xrandr-right-horizontal ()
      "Packed-pixel display on the right, horizontal"
    (interactive)
    (call-process "xrandr" nil nil nil
                  "--output" "eDP1"
                  "--mode" "2560x1440"
                  "--output" "DP1"
                  "--right-of" "eDP1"))

  (defun xrandr-off ()
      "Packed-pixel display on the right, horizontal"
    (interactive)
    (call-process "xrandr" nil nil nil
                  "--output" "eDP1"
                  "--mode" "2560x1440"
                  "--output" "DP1"
                  "--off"))

  (defun keyboard-on ()
      "t460s internal keyboard on"
    (interactive)
    (call-process "xrandr" nil nil nil
                  "--output" "eDP1"
                  "--mode" "2560x1440"
                  "--output" "DP1"
                  "--off"))


  (defun keyboard-enable ()
      "enable t460s internal keyboard"
    (interactive)
    (let ((default-directory "/home/lxol/.dotfiles/"))
      (call-process "t460s-keyboard-enable.sh"
                   nil (current-buffer) nil)))

  (defun keyboard-disable ()
      "disable t460s internal keyboard"

    (interactive)
    (call-process "/home/lxol/.dotfiles/t460s-keyboard-disable.sh"
                  nil nil nil))

  (defun safe-kill-emacs ()
    "Only exit Emacs if this is a small session, otherwise prompt."
    (interactive )
    (if (daemonp)
        (delete-frame)
      (let ((count-buffers (length (buffer-list))))
        (if (< count-buffers 11)
            (save-buffers-kill-emacs)
          (message-box "use Use 'M-x exit'")))))
  (defvar my-term-shell "/usr/bin/bash")
  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'ansi-term)
)

(use-package default-text-scale)

(use-package undo-tree
  :diminish)

;; quick and dirty fix of c-electric-backspace for DEL in java-mode
;; bind-keys* makes sure that bindings will not be overriden by other modes
;; (bind-keys* :map java-mode-map ("DEL" . backward-delete-char-untabify))

(use-package bind-key
  :init
  :bind
  (
   ("C-x C-c" . safe-kill-emacs)
   ("C-=" . text-scale-increase)
   ("C-+" . text-scale-increase)
   ("C--" . text-scale-decrease)))

(use-package evil
  :init
  (evil-mode 1)
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'pam-card-mode 'emacs)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (use-package evil-goggles
     :config
    (evil-goggles-mode)
    )


  ;; (use-package evil-snipe
  ;;   :config
  ;;   (evil-snipe-mode +1)
  ;;   (setq evil-snipe-mode nil)
  ;;   ;;(evil-snipe-override-mode +1)
  ;;   )

  ;; (use-package lxol-evil-textobj-syntax)

  (use-package evil-surround
    :config
    (require 'evil-surround)
    ;;(global-evil-surround-mode nil)
    )
  )

(use-package hydra
  :bind
  ("C-c w t"  . lxol/themes-hydra/body)
  :config
  ;;(setq hydra-lv t)) ; use echo area
  (defhydra lxol/themes-hydra (:hint nil :color pink)
    "
Themes

------------------------------------------------------------------
_s_: Sol Dark   _A_: Mat Dark    _z_: Zenburn  _a_: TaoYang   _q_: QMono    _g_: GreenScreen  _c_: Monochrome  _g_: Gruvbox dark  _l_: Alect Light Alt
_S_: Sol Light  _M_: Mat Light   _e_: Eclipse  _i_: TaoYin    _d_: Darcula  _n_: none         _x_: Sexy Mono   _r_: Gruvbox light _D_: Idea Darkula
_m_: Monochrome themes
"
    ("s" (load-theme 'solarized-dark  t))
    ("S" (load-theme 'solarized-light t))
    ("A" (load-theme 'material        t))
    ("M" (load-theme 'material-light  t))
    ("z" (load-theme 'zenburn         t))
    ("e" (load-theme 'eclipse         t))
    ("a" (load-theme 'tao-yang         t))
    ("i" (load-theme 'tao-yin         t))
    ("g" (load-theme 'green-screen        t))
    ("q" (load-theme 'quasi-monochrome t))
    ("x" (load-theme 'sexy-monochrome t))
    ("d" (load-theme 'darcula t))
    ("D" (load-theme 'idea-darkula t))
    ("g" (load-theme 'gruvbox-dark-soft t))
    ("r" (load-theme 'gruvbox-light-soft t))
    ("c" (load-theme 'monochrome t))
    ("l" (load-theme 'alect-light-alt t))
    ("m" (lxol/themes-monochrome-hydra/body) :color blue)
    ("n" (lxol/disable-all-themes))
    ("RET" nil "done" :color blue))
  (setq hydra-lv nil)
  ) ; use echo area

(use-package hydra
  :config
  (defhydra lxol/themes-monochrome-hydra (:hint nil :color pink)
    "
Monochrome Themes
------------------------------------------------------------------
_b_: lxol Mono Black     _w_: lxol MonoWhite    _c_: lxol Mono Creme   _g_: lxol Mono Grey
_B_: Almost Mono Black   _W_: Almost MonoWhite  _G_: Almost Mono Creme _G_: Almost Mono Grey
------------------------------------------------------------------
"
    ("b" (load-theme 'lxol-mono-black  t))
    ("w" (load-theme 'lxol-mono-white  t))
    ("g" (load-theme 'lxol-mono-gray  t))
    ("c" (load-theme 'lxol-mono-creme  t))
    ("B" (load-theme 'almost-mono-black  t))
    ("W" (load-theme 'almost-mono-white  t))
    ("g" (load-theme 'almost-mono-gray  t))
    ("G" (load-theme 'almost-mono-creme  t))
    ("RET" nil "done" :color blue))
  (setq hydra-lv nil))

(use-package ivy
  :defer 0
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x C-b" . ivy-switch-buffer)
   ("C-x b"  . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("M-j" . ivy-next-line)
   ("M-k" . ivy-previous-line)
   ("M-s-j" . ivy-next-line-and-call)
   ("M-s-k" . ivy-previous-line-and-call))
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ")
  (use-package smex)
  (use-package wgrep)
  (use-package swiper
    :config
    (defun ivy-with-thing-at-point (cmd)
      (let ((ivy-initial-inputs-alist
             (list
              (cons cmd (thing-at-point 'symbol)))))
        (funcall cmd)))

    (defun counsel-ag-thing-at-point ()
      (interactive)
      (ivy-with-thing-at-point 'counsel-ag))

    (defun lxol-counsel-ag-current-dir ()
      (interactive)
      (counsel-ag nil default-directory))

    (defun counsel-git-thing-at-point ()
      (interactive)
      (ivy-with-thing-at-point 'counsel-git-grep))

    (defun swiper-thing-at-point ()
      (interactive)
      (ivy-with-thing-at-point 'swiper))
    :bind
    (
     ("C-s" . swiper)
     )))

(use-package counsel
  :defer 0
  :after  (ivy)
  :config
  (defun lxol-counsel-rg-current-dir ()
    (interactive)
    (counsel-rg nil default-directory))

  :bind
  (
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c r" . counsel-rg)
   ("C-c C-c C-r" . lxol-counsel-rg-current-dir)
   ("C-x l" . counsel-locate)
   ("C-x C-f" . counsel-find-file)
   ("C-c C-c k" . counsel-ag-thing-at-point)
   ("C-c C-c C-k" . counsel-ag-thing-at-point)
   ("C-c C-k" . lxol-counsel-ag-current-dir)
   ("C-c C-c j" . counsel-git-thing-at-point)
   ("C-c C-c C-j" . counsel-git-thing-at-point)
   ("C-c C-c C-s" . swiper-thing-at-point)))

(use-package counsel-etags
  :after  (counsel)
  :custom
  (tags-add-tables 'ask-user)
  (tags-revert-without-query t)
  (large-file-warning-threshold nil)
  (tags-case-fold-search nil)
  (case-fold-search nil)
  :config

  (defun lxol-projectile-visit-project-tags-table ()
    "Visit the current project's tags table."
    (interactive)
    (when (projectile-project-p)
      (let ((tags-file (projectile-expand-root ".TAGS")))
        (when (file-exists-p tags-file)
          (with-demoted-errors "Error loading tags-file: %s"
            (visit-tags-table tags-file t))))))

  (defun lxol-add-tag-table ()
    "Add a new tag table to existing list."
    ;; (interactive (list (read-file-name "Visit tags table (default TAGS): "
    ;;                                 default-directory
    ;;                                 (expand-file-name "TAGS"
    ;;                                                   default-directory)
    ;;                                 t)
    ;;                 current-prefix-arg))
    (interactive)
    (let ((tags-add-tables t)(tags-file-name nil))
      ;; (setq tags-add-tables t)
      (visit-tags-table (expand-file-name ".TAGS" default-directory) nil)
      ;; (select-tags-table)
      ;; (setq tags-add-tables tmp)
      ))

  (defhydra lxol/counsel-etags-hydra (:hint nil :color blue)
    "
                                                           ╭───────────────────┐
                                                           │  Etags            │
   ╭───────────────────────────────────────────────────────────────────────────┴
    [_._] Find tag at point
    [_f_] Two-step tag matching
    [_r_] Recent tags
    [_v_] Projectile visit .TAGS
    [_g_] Grep etags

"
    ("." counsel-etags-find-tag-at-point)
    ("f" counsel-etags-find-tag )
    ("r" counsel-etags-recent-tag)
    ("v" lxol-projectile-visit-project-tags-table)
    ("g" counsel-etags-grep)
    )

  :bind
  (("s-." . lxol/counsel-etags-hydra/body)
   ;;("M-." . counsel-etags-find-tag-at-point)
   ))

;;; Theme hooks
;;; http://www.greghendershott.com/2017/02/emacs-themes.html

(defvar lxol/theme-hooks nil
  "((theme-id . function) ...)")
(defun lxol/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun lxol/add-theme-hook (theme-id hook-func)
  (add-to-list 'lxol/theme-hooks (cons theme-id hook-func)))

(defun lxol/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `lxol/add-theme-hook'."
  (unless no-enable
    (lxol/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id lxol/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'lxol/load-theme-advice)
;; (use-package darcula-theme)
;; (use-package material-theme
;;   ;; :defer t
;;   :init
;;   (defun lxol/material-theme-hook ()
;;     (set-face-attribute 'which-key-key-face nil :foreground
;;                         (face-attribute 'error :foreground))
;;     (loop for n from 1 to 8
;;           do (set-face-attribute (intern-soft (format "org-level-%s" n))
;;                                  nil
;;                                  :height     'unspecified
;;                                  :background 'unspecified
;;                                  :box        'unspecified)))
;;   (lxol/add-theme-hook 'material       #'lxol/material-theme-hook)
;;   (lxol/add-theme-hook 'material-light #'lxol/material-theme-hook))

;; (use-package solarized-theme
;;   :init
;;   (defun lxol/solarized-theme-hook ()
;;     (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
;;     (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
;;     (set-face-attribute 'which-key-key-face nil :foreground
;;                         (face-attribute 'error :foreground)))
;;   (lxol/add-theme-hook 'solarized-dark  #'lxol/solarized-theme-hook)
;;   (lxol/add-theme-hook 'solarized-light #'lxol/solarized-theme-hook)
;;   :config
;;   (setq solarized-use-variable-pitch nil
;;         solarized-use-less-bold t
;;         solarized-use-more-italic nil
;;         solarized-distinct-doc-face t
;;         solarized-high-contrast-mode-line t
;;         ;; I find different font sizes irritating.
;;         solarized-height-minus-1 1.0
;;         solarized-height-plus-1 1.0
;;         solarized-height-plus-2 1.0
;;         solarized-height-plus-3 1.0
;;         solarized-height-plus-4 1.0))

;; (use-package zenburn-theme)

;; (use-package eclipse-theme)
;; (use-package green-screen-theme)
; (set-face-attribute 'default nil :font "Hack-11" )
;;(set-frame-font "Hack-11" nil t)
;; (set-frame-font "Monoid-9" nil t)

;; (set-frame-font "Input Mono Compressed-11")

;;(set-frame-font "FiraCode-10" nil t)



(use-package projectile
  :init
  (defun lxol-projectile-remove-known-projects (re-str)
    "Remove projects matching the regular expression."
    (interactive "s")
    (seq-do #'projectile-remove-known-project
            (seq-filter (lambda (x) (string-match re-str x))
                        projectile-known-projects)))

  (put 'projectile-project-name 'safe-local-variable #'stringp)

  :custom
  (projectile-tags-file-name ".TAGS" )
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))
  (add-to-list 'projectile-project-root-files ".projectile.topmost")
  (add-to-list 'projectile-project-root-files-bottom-up ".projectile.bottomup")
  (add-to-list 'projectile-project-root-files-top-down-recurring ".projectile.top-down-rec"))

(use-package eldoc
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  ("\\.markdown\\'" . gfm-mode)
  ("\\.md\\'" . gfm-mode)
  ("\\.lr\\'" . gfm-mode)
  :init
  ;; use tufte-css for preview
  (setq markdown-preview-style "https://edwardtufte.github.io/tufte-css/tufte.css")
  ;; use github markup for rendering
  ;; script: `https://github.com/steckerhalter/stecktc/blob/master/bin/gfm'
   (setq markdown-command "markdown"))


(use-package yasnippet
  :defer 0
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :bind
  (("C-M-<tab>" . yas-expand)
   ("C-c y" . yas-expand))
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  (setq yas-also-auto-indent-first-line t))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package ivy-yasnippet
  :after(yasnippet ivy)
  :bind
  (("C-<tab>" . ivy-yasnippet)))


;; org init
(defun lxol-load-init-file (init-file)
    "Load init file from USER_EMACS_DIRECTORY."
    (let ((file
           (expand-file-name init-file
                             user-emacs-directory)))
      (when (file-exists-p file)
        (load file))))

(use-package writeroom-mode
  :config
  (define-key writeroom-mode-map (kbd "C-M-,") #'writeroom-decrease-width)
  (define-key writeroom-mode-map (kbd "C-M-.") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-/") #'writeroom-adjust-width))



(use-package help-fns+)
;; (use-package auto-dim-other-buffers
;;   :config
;;   (add-hook 'after-init-hook
;;             (lambda ()
;;               (when (fboundp 'auto-dim-other-buffers-mode)
;;                 (auto-dim-other-buffers-mode t)))))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package expand-region
  :bind
  (("C-s-u" . er/expand-region)
   ("C-s-d" . er/contract-region)))
(use-package color)

(use-package company
  :diminish company-mode
  :commands company-mode
  :bind
  (
   :map company-search-map
   ("M-j" . company-select-next)
   ("M-k" . company-select-previous)
   :map company-active-map
   ("M-j" . company-select-next)
   ("M-k" . company-select-previous))
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-require-match nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)

  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
  ;; :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  ;;(define-key company-active-map [tab] nil)
  ;;(define-key company-active-map (kbd "TAB") nil)
  )

;; (use-package etags-select
;;   :commands etags-select-find-tag)

;; (use-package scala-mode
;;   :mode
;;   ("\\.sc\\'" . scala-mode)
;;   )
;; (use-package ensime
;;   :config
;;   (setq  ensime-search-interface 'ivy)

;;   (defun ensime-edit-definition-with-fallback ()
;;     "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
;;     (interactive)
;;     (unless (and (ensime-connection-or-nil)
;;                  (ensime-edit-definition))
;;       (projectile-find-tag)
;;       ))
;;   :bind
;;   (:map ensime-mode-map
;;         ;; ("s-." . 'ensime-edit-definition-with-fallback)
;;         ;; ("M-." . 'xref-find-definitions)
;;         ))



;; Enable defer and ensure by default for use-package
;; (setq use-package-always-defer t
;;       use-package-always-ensure t)

;; Enable scala-mode and sbt-mode

;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
;; (use-package flycheck
;;   :init (global-flycheck-mode))
;;   :bind
;;   (:map ensime-mode-map
;;         ;; ("s-." . 'ensime-edit-definition-with-fallback)
;;         ;; ("M-." . 'xref-find-definitions)
;;         ))



;; (use-package lsp-scala
;;   :after scala-mode
;;   ;; Optional - enable lsp-scala automatically in scala files
;;   :hook (scala-mode . lsp))


(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1))

(use-package edit-server
  :load-path "~/.emacs.d/lib/edit-server/servers/"
  :config
  (progn
    ;; Don't pop up a new frame
    ;;(setq edit-server-new-frame nil)
    (setq edit-server-default-major-mode 'org-mode)
    (setq edit-server-url-major-mode-alist
          '(("github\\.com" . markdown-mode)
            ("stackexchange\\.com" . markdown-mode)
            ("stackoverflow\\.com" . markdown-mode)
            ("reddit\\.com" . markdown-mode)))

    ;; Integration with Gmail
    (use-package edit-server-htmlize
      :defer t
      :config
      (progn
        (add-hook 'edit-server-start-hook #'edit-server-maybe-dehtmlize-buffer)
        (add-hook 'edit-server-done-hook  #'edit-server-maybe-htmlize-buffer)))
    (edit-server-start)))
(use-package linum-relative
  :config
  (linum-on))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; (use-package beacon
;;   :init
;;   (beacon-mode 1))

;;  dired-hacks packages
(use-package diredsubtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("C-<tab>" . dired-subtree-cycle)
        ("<S-iso-lefttab>" . dired-subtree-remove)))

(use-package dired-filter)

(use-package dired-narrow)

(use-package dired-list)


(lxol-load-init-file "init-haskell.el")
(lxol-load-init-file "init-org.el")
(lxol-load-init-file "scala.el")

(setq dired-guess-shell-alist-user '(("\\.pdf$" "zathura")))



;; (lxol-load-init-file "init-exwm.el")

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
