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
   evil-want-keybinding nil
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
   croll-conservatively 100
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

 (defun go-emacs.d ()
    "Open init file."
    (interactive)
    (dired "~/.emacs.d"))

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
  ;; block for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; end of evil-collection block
  :config
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'pam-card-mode 'emacs)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  )

(use-package evil-leader
  :demand
  :after evil
  :config
  (global-evil-leader-mode)
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "s" 'save-buffer
      "b" 'switch-to-buffer
      "f" 'find-file
      "I" 'find-user-init-file
      "F" 'hydra-flycheck/body
      "B" 'hydra-btoggle/body
      "y" 'hydra-yasnippet/body
      "j" 'hydra-avy/body
      "p" 'hydra-projectile/body
      "(" 'hydra-smartparens/body
      "g" 'hydra-magit/body
      "m" 'hydra-smerge/body
      "w" 'hydra-windows/body
      "O" 'hydra-folding/body
      "n" 'hydra-next-error/body
      "o" 'hydra-org/body
      "e" 'eshell-new
      "a" 'org-agenda
      "i" 'org-capture
      "l" 'hydra-lsp/body
      "L" 'ledger-kredo-replace
      "S" 'sbt-hydra
      "t" 'treemacs
      "h" 'hydra-s/body
      "M" 'evil-mc-mode
      "c" 'hydra-org-clock/body
      "v" 'er/expand-region
      "<SPC>" 'other-window
      "qq" 'save-buffers-kill-terminal
      "qQ" 'save-buffers-kill-emacs)))

(use-package evil-collection
  :after evil
  ;; :custom
  ;; (evil-collection-setup-term t)
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init)
  )

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  )
(use-package evil-surround
  :after evil
  :config
  (require 'evil-surround)
  ;;(global-evil-surround-mode nil)
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
_s_: Sol Dark   _A_: Mat Dark   _z_: Zenburn  _a_: TaoYang   _q_: QMono    _g_: GreenScreen  _c_: Monochrome  _g_: Gruvbox dark  _l_: Alect Light Alt
_S_: Sol Light  _M_: Mat Light  _e_: Eclipse  _i_: TaoYin    _d_: Darcula  _n_: none         _x_: Sexy Mono   _r_: Gruvbox light _D_: Idea Darkula
_v_: VS Code    _O_: Modus Operandi _V_: Modus Vivendi
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
    ("v" (load-theme 'vscode-dark-plus t))
    ("O" (load-theme 'modus-operandi t))
    ("V" (load-theme 'modus-vivendi t))
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

(use-package major-mode-hydra
  :after hydra
  :bind
  ("M-SPC" . major-mode-hydra)
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust)
    "Displays an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-faicon (icon str &optional height v-adjust)
    "Displays an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    "Displays an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    "Displays an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
   ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

;;Hydra / BToggle
;;Group a lot of commands.
(pretty-hydra-define hydra-root
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "tree" "root" 1 -0.05))

  ("Column 1"
   (("F" hydra-flycheck/body "flycheck")
    ("y" hydra-yasnippet/body "yasnippet")
   ("j" hydra-avy/body "avy")))
   )
(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t))
   "Coding"
   (("e" electric-operator-mode "electric operator" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("H" hl-todo-mode "hl-todo" :toggle t)
    ("o" origami-mode "origami" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t))
   "Debug"
   (("de" toggle-debug-on-error "debug on error" :toggle debug-on-error)
    ("dq" toggle-debug-on-quit "debug on C-g" :toggle debug-on-quit))
   "UI"
   (("i" ivy-rich-mode "ivy-rich" :toggle t))))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("k" flycheck-previous-error "previous" :color pink)
    ("j" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

(defhydra hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

;; TODO this doesn't work
(pretty-hydra-define hydra-s
  (:hint t :color teal :quit-key "RET" :title "String manipulation")
  ("Pertaining to words"
   (("w" (lambda()(interactive)(s-split-words (buffer-substring-no-properties (region-beginning) (region-end)))) "split words")
    ("c" (lambda()(interactive)(s-lower-camel-case (buffer-substring-no-properties (region-beginning) (region-end)))) "lower camel")
    ("C" (lambda()(interactive)(s-upper-camel-case (buffer-substring-no-properties (region-beginning) (region-end)))) "upper camel")
    ("s" (lambda()(interactive)(s-snake-case (buffer-substring-no-properties (region-beginning) (region-end)))) "snake")
    ("d" (lambda()(interactive)(s-dashed-words (buffer-substring-no-properties (region-beginning) (region-end)))) "dashed")
    ("W" (lambda()(interactive)(s-capitalized-words (buffer-substring-no-properties (region-beginning) (region-end)))) "capital")
    ("t" (lambda()(interactive)(s-titleized-words (buffer-substring-no-properties (region-beginning) (region-end)))) "titleize")
    ("i" (lambda()(interactive)(s-word-initials (buffer-substring-no-properties (region-beginning) (region-end)))) "initials"))))

(defhydra hydra-avy (:exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_j_] timed char  [_c_] char          [_C_] char-2
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("j" avy-goto-char-timer)
  ("c" avy-goto-char)
  ("C" avy-goto-char-2)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase  _R_efine _<_: base-upper
_p_rev _u_pper _E_diff _=_: upper-lower
^ ^ _l_ower _C_ombine _>_: base-lower
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("n" smerge-next)
  ("l" smerge-keep-lower)
  ("p" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("q" nil :color blue))

(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("F" projectile-recentf "recent files")
    ("D" projectile-dired "dired")
    ("g" counsel-projectile-find-file-dwim "file dwim")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache")
    ("x" projectile-remove-known-project "remove known project")
    ("z" projectile-cache-current-file "cache current file")
    ("X" projectile-cleanup-known-projects "cleanup known projects"))
   "Search"
   (("r" projectile-replace "replace")
    ("o" projectile-multi-occur "occur")
    ("R" projectile-replace-regexp "regexp replace")
    ("sg" counsel-projectile-grep "grep")
    ("ss" counsel-projectile-ag "ag")
    ("sr" counsel-projectile-rg "rg")
    ("ss" counsel-rg "search")
    )))

(defhydra hydra-next-error (:hint nil)
    "
Compilation errors:
_j_: next error        _h_: first error    _q_uit
_k_: previous error    _l_: last error
"
    ("`" next-error     nil)
    ("j" next-error     nil :bind nil)
    ("k" previous-error nil :bind nil)
    ("h" first-error    nil :bind nil)
    ("l" (condition-case err
             (while t
               (next-error))
           (user-error nil))
     nil :bind nil)
    ("q" nil            nil :color blue))

(pretty-hydra-define hydra-lsp
  (:hint nil :color teal :quit-key "q" :exit t :title (with-faicon "rocket" "Lsp"))
 ("Find"
  (("D" lsp-find-declaration "declaration")
   ("d" lsp-find-definition "definition")
   ("R" lsp-find-references "references")
   ("i" lsp-find-implementation "implementation")
   ("gt" lsp-find-type-definition "type")
   ("f" lsp-ivy-workspace-symbol "symbol")
   ("F" lsp-ivy-global-workspace-symbol "global symbol")
   ("uf" lsp-ui-find-workspace-symbol "ui symbol")
   ("pd" lsp-ui-peek-find-definitions "peek def")
   ("pr" lsp-ui-peek-find-references "peek refs")
   ("pf" lsp-ui-peek-find-workspace-symbol "peek symb")
   ("pi" lsp-ui-peek-find-implementation "peek impl"))
  "Toggle"
  (("Td" lsp-ui-doc-mode "doc" :toggle t)
   ("TS" lsp-ui-sideline-mode "sideline" :toggle t)
   ("Ts" lsp-ui-sideline-toggle-symbols-info "side symb" :toggle t)
   ("Tl" lsp-lens-mode "lens" :toggle t)
   ("Ti" lsp-toggle-trace-io "trace-io" :toggle t)
   ("Th" lsp-toggle-symbol-highlight "symb highlight")
   ("Tf" lsp-toggle-on-type-formatting "format" :toggle t)
   ("TF" lsp-ui-flycheck-list "flycheck")
   ("TT" lsp-treemacs-sync-mode "treemacs sync" :toggle t)
   ("TD" lsp-diagnostics-modeline-mode "diag line" :toggle t)
   ("Tnf" lsp-signature-toggle-full-docs "sign docs full")
   ("Tna" lsp-signature-activate "sign activate help")
   ("Tns" lsp-toggle-signature-auto-activate "sign auto activate"))
  "Help"
  (("hd" lsp-ui-doc-glance "doc glance")
   ("hh" lsp-describe-thing-at-point "describe"))
  "Code"
  (("=f" lsp-format-buffer "format")
   ("=r" lsp-format-region "region")
   ("r" lsp-rename "rename")
   ("o" lsp-organize-imports "org imports")
   ("m" lsp-ui-imenu "imenu")
   ("x" lsp-execute-code-action "action"))
  "Other"
  (("l" lsp-avy-lens "avy lens")
   ("ge" lsp-treemacs-errors-list "errors")
   ("gh" lsp-treemacs-call-hierarchy "hierarchy")
   ("gf" lsp-ui-flycheck-list "flycheck")
   ("ga" xref-find-apropos "xref-apropos"))
  "Metals"
  (("Mb" lsp-metals-build-import "build import")
   ("Ms" lsp-metals-sources-scan "sources rescan")
   ("Mr" lsp-metals-build-connect "bloop reconnect"))
  "Session"
  (("s?" lsp-describe-session "describe")
   ("ss" lsp "start")
   ("sd" lsp-disconnect "disconnect")
   ("sr" lsp-workspace-restart "restart")
   ("sq" lsp-workspace-shutdown "shutdown")
   ("sl" lsp-workspace-show-log "log")
   ("sfa" lsp-workspace-folders-add "folders +")
   ("sfo" lsp-workspace-folders-open "folder")
   ("sfr" lsp-workspace-folders-remove "folders -")
   ("sfb" lsp-workspace-blacklist-remove "blacklist -"))))

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-alltheicon "git" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame-addition "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("f" magit-file-popup "file popup")
    ("t" git-timemachine "time machine")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("g" magit-status "status"))))

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("d" delete-window "delete window")
    ("o" delete-other-windows "delete others" :exit t)
    ("s" split-window-below "split below")
    ("h" split-window-horizontally "split horizontally")
    ("v" split-window-vertically "split vertically")
    ("w" other-window "other window" :exit t)
    ("r" rename-buffer "rename buffer" :exit t)
    ("k" kill-buffer-and-window "kill buffer and window" :exit t))
   "Frame"
   (("fk" delete-frame "delete frame")
    ("fo" delete-other-frames "delete others")
    ("fn" make-frame-command "make frame"))
   "Size"
   (("b" balance-windows "balance")
    ("H" shrink-window-horizontally "narrow")
    ("J" shrink-window "lower")
    ("K" enlarge-window "heighten")
    ("L" enlarge-window-horizontally "widen")
    ("S" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(defhydra hydra-folding (:color red)
   "
  _o_pen node    _n_ext fold       toggle _f_orward  _s_how current only
  _c_lose node   _p_revious fold   toggle _a_ll
  "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("f" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("s" origami-show-only-node))

(pretty-hydra-define hydra-org
  (:hint nil :color teal :quit-key "q" :title (with-fileicon "org" "Org" 1 -0.05))
  ("Action"
   (
    ("a" org-agenda "agenda")
    ("j" hydra-org-clock/body "clock")
    ("O" hydra-org-agenda/body "agenda hydra")
    ("C" cfw:open-org-calendar "calfw-org")
    ("s" my/org-ql-goals "goals")
    ("c" org-capture "capture")
    ("g" org-gcal-fetch "gcal fetch")
    ("G" org-gcal-sync "gcal sync")
    ("L" org-store-link "store-link")
    ("l" org-insert-link-global "insert-link")
    ("A" org-archive-done-in-file "archive done in file")
    ("d" org-decrypt-entry "decrypt")
    ("I" org-info-find-node "org info find")
    ("k" org-cut-subtree "cut-subtree")
    ("o" org-open-at-point-global "open-link")
    ("r" org-refile "refile")
    ("t" org-show-todo-tree "todo-tree"))))

(defhydra hydra-org-clock (:color blue :hint nil)
   "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    p_i_ck       _e_dit   _J_ goto     | (_z_)     _r_elative      ti_m_e
 ^ ^     _I_n         _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
 ^ ^     _o_ut        _j_ump   _r_eport     |  ^ ^      _p_ause toggle
 ^ ^     _c_ontinue   ^ ^      ^ ^          |  ^ ^      _s_top
 ^ ^     _P_omodoro   ^ ^      ^ ^          |  ^ ^
"

  ("i" org-mru-clock-in)
  ("I" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("P" org-pomodoro)

  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)

  ("j" org-mru-clock-select-recent-task)
  ("J" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands"))

  ("r" org-timer-start)
  ("n" org-timer-set-timer)
  ("p" org-timer-pause-or-continue)
  ("s" org-timer-stop)

  ("m" org-timer)
  ("t" org-timer-item)
  ("z" (org-info "Timers")))

(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

;; came from here - https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-elisp.el
(defhydra hydra-edebug (:color amaranth
                        :hint  nil)
  "
    EDEBUG MODE
^^_<SPC>_ step             ^^_f_ forward sexp         _b_reakpoint set                previous _r_esult      _w_here                    ^^_d_ebug backtrace
^^_n_ext                   ^^goto _h_ere              _u_nset breakpoint              _e_val expression      bounce _p_oint             _q_ top level (_Q_ nonstop)
_g_o (_G_ nonstop)         ^^_I_nstrument callee      next _B_reakpoint               _E_val list            _v_iew outside             ^^_a_bort recursive edit
_t_race (_T_ fast)         step _i_n/_o_ut            _x_ conditional breakpoint      eval _l_ast sexp       toggle save _W_indows      ^^_S_top
_c_ontinue (_C_ fast)      ^^^^                       _X_ global breakpoint
"
  ("<SPC>" edebug-step-mode)
  ("n"     edebug-next-mode)
  ("g"     edebug-go-mode)
  ("G"     edebug-Go-nonstop-mode)
  ("t"     edebug-trace-mode)
  ("T"     edebug-Trace-fast-mode)
  ("c"     edebug-continue-mode)
  ("C"     edebug-Continue-fast-mode)

  ("f"     edebug-forward-sexp)
  ("h"     edebug-goto-here)
  ("I"     edebug-instrument-callee)
  ("i"     edebug-step-in)
  ("o"     edebug-step-out)

  ;; breakpoints
  ("b"     edebug-set-breakpoint)
  ("u"     edebug-unset-breakpoint)
  ("B"     edebug-next-breakpoint)
  ("x"     edebug-set-conditional-breakpoint)
  ("X"     edebug-set-global-break-condition)

  ;; evaluation
  ("r"     edebug-previous-result)
  ("e"     edebug-eval-expression)
  ("l"     edebug-eval-last-sexp)
  ("E"     edebug-visit-eval-list)

  ;; views
  ("w"     edebug-where)
  ("p"     edebug-bounce-point)
  ("v"     edebug-view-outside) ; maybe obsolete??
  ("P"     edebug-view-outside) ; same as v
  ("W"     edebug-toggle-save-windows)

  ("d"     edebug-backtrace)

  ;; quitting and stopping
  ("q"     top-level :color blue)
  ("Q"     edebug-top-level-nonstop :color blue)
  ("a"     abort-recursive-edit :color blue)
  ("S"     edebug-stop :color blue))
(with-eval-after-load 'edebug
  (bind-key "?" #'hydra-edebug/body edebug-mode-map))


;;===================================================================================================
;;===================================================================================================
;;===================================================================================================
;;===============================            END HYDRA        =======================================
;;===================================================================================================
;;===================================================================================================
;;===================================================================================================

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
   ("M-C-j" . ivy-next-line-and-call)
   ("M-C-k" . ivy-previous-line-and-call))
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
  (projectile-completion-system 'ivy)
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

;; (use-package counsel-projectile
;;   :after projectile counsel
;;   :config (counsel-projectile-mode))

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
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0.5
   company-minimum-prefix-length 2)
  ;; :bind (:map company-active-map
  ;;             ("<return>" . company-complete-common)
  ;;             ("RET" . company-complete-common)
  ;;             ("C-SPC" . company-complete-selection))
  :config
  (global-company-mode 1)
  ;;   ;;(define-key company-active-map (kbd "RET") #'company-complete-selection)
  ;;   (define-key company-active-map (kbd "<return>") #'company-complete-selection)
  ;;   (define-key company-active-map (kbd "<tab>") #'company-complete-common)
  ;;   (define-key company-active-map (kbd "TAB") #'company-complete-common)
  ;;   ;; to complete common and then cycle
  ;;   ;;(define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
  ;;   ;;(define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))
  ;;  )
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))
  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))
  (with-eval-after-load 'company
    (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
    ;;(define-key company-active-map (kbd "M-n") nil)
    ;;(define-key company-active-map (kbd "M-p") nil)
    ;;(define-key company-active-map (kbd "C-n") #'company-select-next)
    ;;(define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-mode-map (kbd "C-<space>") #'company-complete)
    ;;(define-key company-active-map (kbd "RET") #'company-complete-selection)
    (define-key company-active-map (kbd "<return>") #'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") #'company-complete-common)
    (define-key company-active-map (kbd "TAB") #'company-complete-common)
    ;; to complete common and then cycle
    ;;(define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
    ;;(define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))
    )
)

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))
;; (use-package company
;;   :diminish company-mode
;;   :commands company-mode
;;   :bind
;;   (
;;    :map company-search-map
;;    ("M-j" . company-select-next)
;;    ("M-k" . company-select-previous)
;;    :map company-active-map
;;    ("M-j" . company-select-next)
;;    ("M-k" . company-select-previous))
;;   :init
;;   (setq
;;    company-dabbrev-ignore-case nil
;;    company-require-match nil
;;    company-dabbrev-code-ignore-case nil
;;    company-dabbrev-downcase nil
;;    company-idle-delay 0
;;    company-minimum-prefix-length 4)

;;   ;; (let ((bg (face-attribute 'default :background)))
;;   ;;   (custom-set-faces
;;   ;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;   ;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;   ;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;   ;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;   ;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))
;;   ;; :config
;;   ;; disables TAB in company-mode, freeing it for yasnippet
;;   ;;(define-key company-active-map [tab] nil)
;;   ;;(define-key company-active-map (kbd "TAB") nil)
;;   )
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

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


(pretty-hydra-define hydra-projectile
  (:hint nil :color teal :quit-key "q" :title (with-faicon "rocket" "Projectile" 1 -0.05))
  ("Buffers"
   (("b" counsel-projectile-switch-to-buffer "list")
    ("k" projectile-kill-buffers "kill all")
    ("S" projectile-save-project-buffers "save all"))
   "Find"
   (("d" counsel-projectile-find-dir "directory")
    ("F" projectile-recentf "recent files")
    ("D" projectile-dired "dired")
    ("g" counsel-projectile-find-file-dwim "file dwim")
    ("f" counsel-projectile-find-file "file")
    ("p" counsel-projectile-switch-project "project"))
   "Other"
   (("i" projectile-invalidate-cache "reset cache")
    ("x" projectile-remove-known-project "remove known project")
    ("z" projectile-cache-current-file "cache current file")
    ("X" projectile-cleanup-known-projects "cleanup known projects"))
   "Search"
   (("r" projectile-replace "replace")
    ("o" projectile-multi-occur "occur")
    ("R" projectile-replace-regexp "regexp replace")
    ("sg" counsel-projectile-grep "grep")
    ("ss" counsel-projectile-ag "ag")
    ("sr" counsel-projectile-rg "rg")
    ("ss" counsel-rg "search")
    )))

;; (lxol-load-init-file "init-exwm.el")

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
