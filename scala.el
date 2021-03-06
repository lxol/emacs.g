;;; scala.el --- scala configurations                   -*- lexical-binding: t -*-


(use-package scala-mode
  :mode "\\.s\\(cala\\|c\\|bt\\)$"
  :hook (scala-mode . pragmatapro-prettify-hook) 
  )

(use-package sbt-mode
  :init
  (setq sbt:ansi-support 'filter) ;; todo: investigate why ansi-color doesn't rendere espace sequences properly
  (setq sbt:sbt-prompt-regexp "^\\(\\[[^\]]*\\] \\)?[>$][ ]*"
        ;;sbt:program-options '("-Djline.terminal=auto -Dsbt.log.noformat=true"))
        sbt:program-options '("-Dsbt.log.noformat=true"))
  (setq sbt:prefer-nested-projects 't)

  (add-hook 'sbt-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    `((,(expand-file-name (directory-file-name default-directory)) . ?⌂)
                      (,(expand-file-name "~") . ?~)))
              (prettify-symbols-mode t))))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))
(setq sbt:program-options '("-Dsbt.supershell=false"))


(use-package flycheck
  :custom
  (flycheck-scalastylerc "~/.dotfiles/scalastyle_config.xml")
  :init (global-flycheck-mode)
  :config
  (defhydra hydra-flycheck
    (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
          :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
          :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  :bind
  (("C-c f" . hydra-flycheck/body)))

(setq lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :bind
  (:map lsp-mode-map
        ("M-l" . hydra-scala-lsp/body))
  :config
  (unbind-key "M-l")
  ;; (setq lsp-ui-doc-enable nil)

  ;; :bind
  ;; (:map lsp-mode-map
  ;;       ("M-." . lsp-find-definition))
  :custom
  (lsp-enable-symbol-highlighting t)
  (lsp-completion-enable t)
  (lsp-completion-enable t)
  (lsp-ui-doc-enable nil)
  )

(use-package lsp-metals
  :after lsp-mode
  :custom
  (lsp-metals-server-command "/usr/bin/metals-emacs")
  (lsp-metals-sbt-script "/usr/bin/sbt")
  )

(defhydra hydra-scala-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
  ^ ^                  [_M-r_] restart           [_d_] declaration  [_i_] implementation
 [_m_] imenu           [_S_]   shutdown          [_D_] definition   [_t_] type
 [_x_] execute action  [_M-s_] describe session  [_R_] references   [_s_] signature
  ^ ^                   ^   ^                    [_o_] docs         [_r_] rename
  ^ ^                   ^   ^                    [_f_] find
 [_T_] toggles"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace)
  ("f" lsp-ivy-workspace-symbol)
  ("T" (hydra-scala-lsp-toggles/body) :color blue)
  )

(defhydra hydra-scala-lsp-toggles (:exit t :hint nil)
  "
-------------------------------------------------------------------------------------
 [_b_] breadcrumb           [_c_] completion mode
 [_d_] doc mode
 [_h_] symbol highlighting
"
  ("b" lsp-headerline-breadcrumb-mode)
  ("d" lsp-ui-doc-mode)
  ("h" lsp-toggle-symbol-highlight)
  ("c" lsp-completion-mode)
  )

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode))

;;(use-package lsp-scala
;;   ;; :after scala-mode
;;   :hook (scala-mode . lsp)
;;   :init (setq lsp-scala-server-command "~/bin/metals-emacs"))

;; (use-package lsp-scala
;;   :after scala-mode
;;   :demand t
;;   ;; Optional - enable lsp-scala automatically in scala files
;;   :hook (scala-mode . lsp)
;;   :init (setq lsp-scala-server-command "~/bin/metals-emacs"))

;; (use-package company-lsp
;;  :after (company lsp-mode))

(use-package company-lsp
  :after lsp-mode company
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
  :commands company-lsp)

;; (use-package treemacs
;;     :defer t
;;     :init
;;     (with-eval-after-load 'winum
;;       (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;     :config
;;     (progn
;;       (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;             treemacs-deferred-git-apply-delay      0.5
;;             treemacs-directory-name-transformer    #'identity
;;             treemacs-display-in-side-window        t
;;             treemacs-eldoc-display                 t
;;             treemacs-file-event-delay              5000
;;             treemacs-file-extension-regex          treemacs-last-period-regex-value
;;             treemacs-file-follow-delay             0.2
;;             treemacs-file-name-transformer         #'identity
;;             treemacs-follow-after-init             t
;;             treemacs-git-command-pipe              ""
;;             treemacs-goto-tag-strategy             'refetch-index
;;             treemacs-indentation                   2
;;             treemacs-indentation-string            " "
;;             treemacs-is-never-other-window         nil
;;             treemacs-max-git-entries               5000
;;             treemacs-missing-project-action        'ask
;;             treemacs-no-png-images                 nil
;;             treemacs-no-delete-other-windows       t
;;             treemacs-project-follow-cleanup        nil
;;             treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;             treemacs-position                      'left
;;             treemacs-recenter-distance             0.1
;;             treemacs-recenter-after-file-follow    nil
;;             treemacs-recenter-after-tag-follow     nil
;;             treemacs-recenter-after-project-jump   'always
;;             treemacs-recenter-after-project-expand 'on-distance
;;             treemacs-show-cursor                   nil
;;             treemacs-show-hidden-files             t
;;             treemacs-silent-filewatch              nil
;;             treemacs-silent-refresh                nil
;;             treemacs-sorting                       'alphabetic-asc
;;             treemacs-space-between-root-nodes      t
;;             treemacs-tag-follow-cleanup            t
;;             treemacs-tag-follow-delay              1.5
;;             treemacs-user-mode-line-format         nil
;;             treemacs-width                         35)

;;       ;; The default width and height of the icons is 22 pixels. If you are
;;       ;; using a Hi-DPI display, uncomment this to double the icon size.
;;       ;;(treemacs-resize-icons 44)

;;       (treemacs-follow-mode t)
;;       (treemacs-filewatch-mode t)
;;       (treemacs-fringe-indicator-mode t)
;;       (pcase (cons (not (null (executable-find "git")))
;;                    (not (null treemacs-python-executable)))
;;         (`(t . t)
;;          (treemacs-git-mode 'deferred))
;;         (`(t . _)
;;          (treemacs-git-mode 'simple))))
;;     :bind
;;     (:map global-map
;;           ("M-0"       . treemacs-select-window)
;;           ("C-x t 1"   . treemacs-delete-other-windows)
;;           ("C-x t t"   . treemacs)
;;           ("C-x t B"   . treemacs-bookmark)
;;           ("C-x t C-t" . treemacs-find-file)
;;           ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after treemacs)

;; (use-package treemacs-projectile
;;   :after treemacs)

;; (use-package lsp-treemacs
;;  :after (lsp-mode treemacs)
;;  :config
;;   (lsp-metals-treeview-enable t)
;;   (setq lsp-metals-treeview-show-when-views-received t)
;;   )

 (defun go-scala-study ()
    "Open init file."
    (interactive)
    (find-file "~/Documents/projects/scala-study/"))


 (defun go-throwaway-scala ()
    "Open init file."
    (interactive)
    (find-file "~/Documents/projects/scala/throwaway-scala/"))

(use-package ammonite-term-repl
  :defer t
  :after scala-mode
  :demand t
  ;; Optional - enable lsp-scala automatically in scala files
  )
(use-package reformatter
  :defer t
  :after scala-mode
  :config
  (reformatter-define scala-ammonite-format
    :program "/usr/bin/scalafmt"
    :args '("--stdin" "--non-interactive" "--quiet" "--assume-filename" "foo.sc"))
  (define-key scala-mode-map (kbd "C-c C-x C-a") 'scala-ammonite-format-buffer)

  (reformatter-define scala-sbt-format
    :program "/usr/bin/scalafmt"
    :args '("--stdin" "--non-interactive" "--quiet" "--assume-filename" "foo.sbt"))
  (define-key scala-mode-map (kbd "C-c C-x C-s") 'scala-sbt-format-buffer)

  (reformatter-define scala-format
    :program "/usr/bin/scalafmt"
    :args '("--stdin" "--non-interactive" "--quiet"))
  (define-key scala-mode-map (kbd "C-c C-x C-l") 'scala-format-buffer)
  )

(use-package ob-ammonite
  ;; :defer t
  :config
  (setq ob-ammonite-prompt-str "scala>")
  )
;; (use-package ob-ammonite
;;   :ensure-system-package (amm . "sudo sh -c '(echo \"#!/usr/bin/env sh\" && curl -L https://github.com/lihaoyi/Ammonite/releases/download/2.0.4/2.13-2.0.4) > /usr/local/bin/amm && chmod +x /usr/local/bin/amm' && amm")
;;   ;; :defer 1
;;   :config
;;   (use-package ammonite-term-repl)
;;   (setq ob-ammonite-prompt-str "scala>")
;;   (setq ammonite-term-repl-auto-detect-predef-file nil)
;;   (setq ammonite-term-repl-program-args '("--no-remote-logging" "--no-default-predef" "--no-home-predef"))
;;   (defun my/substitute-sbt-deps-with-ammonite ()
;;     "Substitute sbt-style dependencies with ammonite ones."
;;     (interactive)
;;     (apply 'narrow-to-region (if (region-active-p) (my/cons-cell-to-list (region-bounds)) `(,(point-min) ,(point-max))))
;;     (goto-char (point-min))
;;     (let ((regex "\"\\(.+?\\)\"[ ]+%\\{1,2\\}[ ]+\"\\(.+?\\)\"[ ]+%\\{1,2\\}[ ]+\"\\(.+?\\)\"")
;;           (res))
;;       (while (re-search-forward regex nil t)
;;         (let* ((e (point))
;;                (b (search-backward "\"" nil nil 6))
;;                (s (buffer-substring-no-properties b e))
;;                (s-without-percent (apply 'concat (split-string s "%")))
;;                (s-without-quotes (remove-if (lambda (x) (eq x ?" ;"
;;                                                             ))
;;                                             s-without-percent))
;;                (s-as-list (split-string s-without-quotes)))
;;                     (delete-region b e)
;;           (goto-char b)
;;           (insert (format "import $ivy.`%s::%s:%s`" (first s-as-list) (second s-as-list) (third s-as-list)))
;;           )
;;         )
;;       res)
;;     (widen)))
;;(use-package format-all)


;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(scala-mode . ("/home/lxol/bin/metals-emacs")))
;;   ;; (optional) Automatically start metals for Scala files.
;;   :hook (scala-mode . eglot-ensure))

(use-package play-routes-mode
  :config
  (add-hook 'play-routes-mode-hook
               (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|fixme\\|todo\\):" 1 font-lock-warning-face t)))))
)
;;; scala.el ends here
