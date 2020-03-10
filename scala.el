;;; scala.el --- scala configurations                   -*- lexical-binding: t -*-


(use-package scala-mode
  :mode "\\.s\\(cala\\|c\\|bt\\)$")

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

(use-package lsp-mode
  :hook (scala-mode . lsp)
  :config
  (setq lsp-ui-doc-enable nil)
  :bind
  (:map lsp-mode-map
        ("M-." . lsp-find-definition))
  :init
  (setq lsp-prefer-flymake nil))

(use-package lsp-metals
  :custom
  (lsp-metals-server-command "/usr/local/bin/metals-emacs")
  (lsp-metals-sbt-script "/usr/bin/sbt")
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

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

(use-package company-lsp)

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

;;(use-package format-all)


;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(scala-mode . ("/home/lxol/bin/metals-emacs")))
;;   ;; (optional) Automatically start metals for Scala files.
;;   :hook (scala-mode . eglot-ensure))

;;; scala.el ends here
