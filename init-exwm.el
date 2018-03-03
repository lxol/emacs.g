;; -*- lexical-binding: t -*-
(use-package exwm
  :config 
  (use-package xelb)
  (use-package exwm-config
    :config
    (exwm-config-default)
    (setq exwm-workspace-number 9
          exwm-workspace-show-all-buffers t
          exwm-layout-show-all-buffers t)))

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "URxvt"))
              (setq-local exwm-input-prefix-keys '(?\C-x)))))
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
(exwm-input-set-key (kbd "s-x") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)


;; (dolist (k '(                           ;
;;              ("s-s" "scrot")
;;              ("s-S-s" "scrot -s")
;;              ("s-<return>" "urxvt")
;;              ("<f8>" "mpc toggle")
;;              ("s-S-j"
;;               "amixer sset Master 5%-")
;;              ("s-S-k"
;;               "amixer set Master unmute; amixer sset Master 5%+")))

;; (let ((f (lambda () ((interactive "P"))
;;            (save-window-excursion
;;              (start-process-shell-command (cadr k) nil (cadr k))))))
;;   (exwm-input-set-key (kbd (car k)) f)))

;; (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
;; (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
;; (exwm-input-set-key (kbd "s-h") #'windmove-left)
;; (exwm-input-set-key (kbd "s-j") #'windmove-down)
;; (exwm-input-set-key (kbd "s-k") #'windmove-up)
;; (exwm-input-set-key (kbd "s-l") #'windmove-right)
;; (exwm-input-set-key (kbd "s-D") #'kill-this-buffer)
;; (exwm-input-set-key (kbd "s-b") #'list-buffers)
;; (exwm-input-set-key (kbd "s-f") #'find-file)

(require 'exwm-randr)

;; (setq exwm-randr-workspace-output-plist '(0 "eDP-1"  1 "DVI-I-1-1")) 
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output eDP1  --mode 2560x1440 --output DVI-I-1-1 --mode 1920x1080 --right-of eDP1" ))
;;           (exwm-randr-enable))

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output DP1 --left-of eDP1 --auto")))
(setq exwm-randr-workspace-output-plist '(0 "eDP1" 2 "DP1" ))  
(exwm-randr-enable)
