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
  ;; (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
  ;; (use-package hindent)
  )
