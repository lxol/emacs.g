;; (use-package intero
;;   ;; :after hlint-refactor
;;   ;; :ensure t
;;   :config
;;   (add-hook 'haskell-mode-hook
;;             (lambda ()
;;               (intero-mode)
;;               (flycheck-mode)
;;               ;; (hlint-refactor-mode)
;;               ;; (flycheck-add-next-checker 'intero
;;               ;;                            '(warning . haskell-hlint))
;;               (define-key evil-normal-state-map (kbd "]") 'intero-goto-definition)
;;               (define-key evil-normal-state-map (kbd "[") 'xref-pop-marker-stack)
;;               ;; (setq-default haskell-stylish-on-save t)
;;               )
;;             )
;;   )
(use-package haskell-mode
  :config
  (defun haskell/evil-open-above ()
    "Opens a line above the current mode"
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun haskell/evil-open-below ()
    "Opens a line below the current mode"
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))
  (evil-define-key 'normal haskell-mode-map "o" 'haskell/evil-open-below
    "O" 'haskell/evil-open-above)

  :hook (haskell-mode . haskell-auto-insert-module-template)
  (haskell-mode . pragmatapro-prettify-hook) 
  (haskell-mode . haskell-indentation-mode)
  )

(use-package emamux)

(use-package emamux-ghci
  :after (haskell-mode)
  :config
  (setq emamux-ghci:tmux-address "haskell:ghci")
  (setq emamux-ghci:includes '("src" "tests"))
  (setq emamux-ghci:exts '("UnicodeSyntax" "GADTs"))
  (defun lxol-tmux-ghci ()
    "start tmux ghci session"
    (interactive)
    (cd (projectile-acquire-root))
    (start-process
     "unused"
     nil
     "kitty"
     "-e" "tmux" "new-session" "-n" "ghci" "-s" "haskell" "stack repl"
     ))
  :bind (:map haskell-mode-map
              ([f2] . emamux-ghci:proj-load-buffer)
              ([f3] . lxol-tmux-ghci))
  )
  

(use-package hindent
  :after haskell-mode
  )

(use-package ivy-hoogle
  :after haskell-mode
  :bind (:map haskell-mode-map
              ("C-c h" . ivy-hoogle))
  )
