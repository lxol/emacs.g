;; -*- lexical-binding: t -*-
(use-package exwm
  :config 
  (use-package xelm)
  (use-package exwm-config
    :config (exwm-config-default)))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "URxvt"))
              (setq-local exwm-input-prefix-keys '(?\C-x)))))
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)


(dolist (k '(                           ;
             ("s-s" "scrot")
             ("s-S-s" "scrot -s")
             ("s-<return>" "urxvt")
             ("<f8>" "mpc toggle")
             ("s-S-j"
              "amixer sset Master 5%-")
             ("s-S-k"
              "amixer set Master unmute; amixer sset Master 5%+")))
  (let ((f (lambda () (interactive)
             (save-window-excursion
               (start-process-shell-command (cadr k) nil (cadr k))))))
    (exwm-input-set-key (kbd (car k)) f)))
;; (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
;; (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
;; (exwm-input-set-key (kbd "s-h") #'windmove-left)
;; (exwm-input-set-key (kbd "s-j") #'windmove-down)
;; (exwm-input-set-key (kbd "s-k") #'windmove-up)
;; (exwm-input-set-key (kbd "s-l") #'windmove-right)
;; (exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
;; (exwm-input-set-key (kbd "s-b") #'list-buffers)
;; (exwm-input-set-key (kbd "s-f") #'find-file)

;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(0 "VGA1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto")))
;; (exwm-randr-enable)
