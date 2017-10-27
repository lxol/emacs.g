(use-package lxol-config
  :init
  (provide 'lxol-config)
  :config
  (setq
   make-backup-files nil
   dired-dwim-target t
   create-lockfiles nil
   make-backup-files nil
   column-number-mode t
   scroll-error-top-bottom t
   electric-indent-mode 0
   enable-recursive-minibuffers t
   backup-directory-alist `((".*" . ,temporary-file-directory)) ;don't clutter my fs and put backups into tmp
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   require-final-newline t        ;auto add newline at the end of file
   default-major-mode 'text-mode  ;use text mode per default
   history-length 250             ;default is 30
   locale-coding-system 'utf-8    ;utf-8 is default
   tab-always-indent 'complete    ;try to complete before identing
   recentf-max-saved-items 5000     ;same up to 5000 recent files
   eval-expression-print-length nil ;do not truncate printed expressions
   eval-expression-print-level nil  ;print nested expressions
   kill-ring-max 5000           ;truncate kill ring after 5000 entries
   mark-ring-max 5000           ;truncate mark ring after 5000 entries
   split-height-threshold 110   ;more readily split horziontally
   enable-recursive-minibuffers t
   load-prefer-newer t           ;prefer newer .el instead of the .elc
   split-width-threshold 160 ;split horizontally only if less than 160 columns
   gc-cons-percentage 0.3    ;increase garbage collection limit
   switch-to-buffer-preserve-window-point t
   visible-bell t
   ring-bell-function 'ignore
   cursor-type t
   custom-file "/tmp/custom-file.el" ;don't pollute the init file and don't `load' the customs
			;but keep them for reference...
   )
  
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
 
  (defun exit ()
    "Shorthand for DEATH TO ALL PUNNY BUFFERS!"
    (interactive)
    (if (daemonp)
        (messag "You silly")
      (save-buffers-kill-emacs)))
  
  (defun safe-kill-emacs ()
    "Only exit Emacs if this is a small session, otherwise prompt."
    (interactive )
    (if (daemonp)
        (delete-frame)
      (let ((count-buffers (length (buffer-list))))
        (if (< count-buffers 11)
            (save-buffers-kill-emacs)
          (message-box "use Use 'M-x exit'")))))
  ;; your preferred main font face here
  ;; (defvar my-font-attributes '(default nil :family "Anonymous Pro" :height 89))
  ;; source code pro: '(default nil :family "Source Code Pro" :height 95)
  ;; source code pro: '(default nil :family "Hack" :height 95)
  ;; (set-frame-font "Inconsolata-11")
  ;; (set-frame-font "Source Code Pro-11")
  ;; (set-frame-font "Anonymous Pro-12")
  ;; (set-frame-font "Hack-11")
  ;; (set-frame-font "Go Mono-10")
  ;; (set-frame-font "Input Mono Condensed-10")
  ;; (set-frame-font "Input Mono-10")
  ;; (set-frame-font "Input Mono Light-10")
  ;; (set-frame-font "Fira Code-11")
  (set-frame-font "Input Mono Narrow-10")
  ;; (set-frame-font "Input Mono Compressed-10")
  )

(use-package default-text-scale)

(use-package undo-tree
  :diminish)

(defhydra lxol/font-hydra (:hint nil :color pink)
  "
                                                           ╭───────────────────┐
                                                           │  Font             │
   ╭───────────────────────────────────────────────────────────────────────────┴
    [_i_] Inconsolata     
    [_s_] Source Code Pro 
    [_a_] Anonymous Pro
    [_h_] Hack
    [_g_] Go Mono
    [_m_] Input Mono
    [_c_] Input Mono Condensed
    [_p_] Input Mono Commpressed
    [_l_] Input Mono Light
    [_n_] Input Mono Narrow
    [_f_] Fira Code
"
  ("i" (set-frame-font "Inconsolata-11"))
  ("s" (set-frame-font "Source Code Pro-11"))
  ("a" (set-frame-font "Anonymous Pro-11"))
  ("h" (set-frame-font "Hack-11"))
  ("g" (set-frame-font "Go Mono-11"))
  ("c" (set-frame-font "Input Mono Condensed-11"))
  ("m" (set-frame-font "Input Mono-11"))
  ("l" (set-frame-font "Input Mono Light-11"))
  ("f" (set-frame-font "Fira Code-11"))
  ("n" (set-frame-font "Input Mono Narrow-11"))
  ("p" (set-frame-font "Input Mono Compressed-11"))
  ("RET" nil "done" :color blue))

(bind-keys
 :prefix "C-c w"
 :prefix-map lxol-window-keymap
 :menu-name "Windows key prefix"
 ("t"  . lxol/themes-hydra/body)
 ("f"  . lxol/font-hydra/body))

;; quick and dirty fix of c-electric-backspace for DEL in java-mode
;; bind-keys* makes sure that bindings will not be overriden by other modes
(bind-keys* :map java-mode-map ("DEL" . backward-delete-char-untabify))

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
  )

(use-package ivy
  :diminish ivy-mode
  :bind 
  (("C-c C-r" . ivy-resume)
   ("C-x C-b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("M-j" . ivy-next-line)
   ("M-k" . ivy-previous-line)
   ("C-M-j" . ivy-next-line-and-call)
   ("C-M-k" . ivy-previous-line-and-call))
  :config
  (ivy-mode 1)
  (use-package counsel
    :bind 
    (("C-c g" . counsel-git)
     ("C-c j" . counsel-git-grep)
     ("C-c k" . counsel-ag)
     ("C-x l" . counsel-locate)
     ("C-x C-f" . counsel-find-file)))
  (use-package hydra
    :config
    ;;(setq hydra-lv t)) ; use echo area
    (setq hydra-lv nil)) ; use echo area
  (use-package smex)
  (use-package swiper
    :bind 
    (("C-s" . swiper))))

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

(use-package material-theme
  ;; :defer t
  :init
  (defun lxol/material-theme-hook ()
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground))
    (loop for n from 1 to 8
          do (set-face-attribute (intern-soft (format "org-level-%s" n))
                                 nil
                                 :height     'unspecified
                                 :background 'unspecified
                                 :box        'unspecified)))
  (lxol/add-theme-hook 'material       #'lxol/material-theme-hook)
  (lxol/add-theme-hook 'material-light #'lxol/material-theme-hook))

(use-package solarized-theme
  :init
  (defun lxol/solarized-theme-hook ()
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground)))
  (lxol/add-theme-hook 'solarized-dark  #'lxol/solarized-theme-hook)
  (lxol/add-theme-hook 'solarized-light #'lxol/solarized-theme-hook)
  :config
  (setq solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))

(use-package zenburn-theme)

(use-package eclipse-theme)

(defhydra lxol/themes-hydra (:hint nil :color pink)
  "
Themes

^Solarized^   ^Material^   ^Other^    
----------------------------------------------------
_s_: Dark     _m_: Dark    _z_: Zenburn  _DEL_: none
_S_: Light    _M_: Light   _e_: Eclipse
"
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("z" (load-theme 'zenburn         t))
  ("e" (load-theme 'eclipse         t))
  ("DEL" (lxol/disable-all-themes))
  ("RET" nil "done" :color blue))

(bind-keys ("C-c w t"  . lxol/themes-hydra/body))

(use-package projectile
  :init
  (defun lxol-projectile-remove-known-projects (re-str)
    "Remove projects matching the regular expression."
    (interactive "s")
    (seq-do #'projectile-remove-known-project
            (seq-filter (lambda (x) (string-match re-str x))
                        projectile-known-projects)))

  (put 'projectile-project-name 'safe-local-variable #'stringp)

  :config
  (projectile-global-mode)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " <%s>" (projectile-project-name))))
  (add-to-list 'projectile-project-root-files ".projectile.topmost")
  (add-to-list 'projectile-project-root-files-bottom-up ".projectile.bottomup")
  (add-to-list 'projectile-project-root-files-top-down-recurring ".projectile.top-down-rec"))

;; (use-package swiper
;;   :quelpa
;;   :bind
;;   (
;;    ("C-s" . swiper)
;;    ("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("<f1> f" . counsel-describe-function)
;;    ("<f1> v" . counsel-describe-variable)
;;    ("<f1> l" . counsel-load-library)
;;    ("<f2> i" . counsel-info-lookup-symbol)
;;    ("<f2> u" . counsel-unicode-char)

;;    ("C-c g" . counsel-git)
;;    ("C-c j" . counsel-git-grep)
;;    ("C-c k" . counsel-ag)
;;    ("C-x l" . counsel-locate)

;;    ("C-c C-r" . ivy-resume)

;;    )
;;   :config
;;   (use-package smex
;;     :quelpa)
;;   (use-package ivy
;;     :quelpa
;;     :diminish ivy-mode
;;     :config
;;     ;; (define-key ivy-minibuffer-map (kbd "C-M-j") 'ivy-next-line-and-call)
;;     ;; (define-key ivy-minibuffer-map (kbd "C-M-k") 'ivy-previous-line-and-call)
;;     (setq ivy-count-format "(%d/%d) ")
;;     (setq ivy-use-virtual-buffers t)
;;     (setq ivy-views
;;           `(("english + notes {}"
;;              (vert
;;               (file "/home/lxol/org/notes/english.org")
;;               (buffer "english-notes")))
;;             ("ivy.el {}"
;;              (horz
;;               (file ,(find-library-name "ivy"))
;;               (buffer "*scratch*")))))
;;     (global-set-key (kbd "M-SPC") 'avy-goto-char-timer)
;;     (bind-keys
;;      ("C-x C-b" . ivy-switch-buffer)
;;      ("C-c v" . ivy-push-view)
;;      ("C-c V" . ivy-pop-view)
;;      :map ivy-minibuffer-map
;;      ("M-j" . ivy-next-line)
;;      ("M-k" . ivy-previous-line)
;;      ("C-M-j" . ivy-next-line-and-call)
;;      ("C-M-k" . ivy-previous-line-and-call)
;;      )
;;   ;; (use-package counsel
;;   ;;   :quelpa)
;;   ;; ;; (use-package ivy-hydra
;;   ;; ;;   quelpa)
  
;;   (use-package avy
;;     :bind
;;     (
;;      ("s-c" . avy-goto-char)
;;      ("s-d" . avy-goto-char-2)
;;      ("M-g g" . avy-goto-line)
;;      ("M-g M-g" . avy-goto-line)
;;      ("M-g d" . avy-goto-word-1)
;;      ("M-c" . avy-goto-word-or-subword-1)
;;      ("C-<return>" . avy-goto-word-1)
;;      ("M-g e" . avy-goto-word-0))

;;     :config
;;     (avy-setup-default)
;;     (setq avy-all-windows 'all-frames))
;;   (use-package wgrep)
;;   (use-package ace-window
;;     :bind (
;;            ("M-g w" . ace-window)
;;            ))
;;   (ivy-mode 1))
;; )
