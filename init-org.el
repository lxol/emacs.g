(use-package org
  :mode
  ("\\.org_archive$" . org-mode)
  ("\\.txt$" . org-mode)
  ("\\.org$\\'" . org-mode)
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c i" . org-clock-in-last)
   ("C-c o" . org-clock-out)
   ("C-c C-9" . org-insert-subheading)
   ("C-c C-0" . org-insert-todo-subheading)
   )

  :init
  (use-package org-capture
    :init
    (setq org-capture-templates
          (quote (("t" "todo" entry (file "~/org/planner/refile.org")
                   "* TODO %?\n" :clock-in t :clock-resume t)
                  ("r" "respond" entry (file "~/org/planner/refile.org")
                   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                  ("n" "note" entry (file "~/org/planner/refile.org")
                   "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
                  ("j" "Journal" entry (file+datetree "~/org/planner/diary.org")
                   "* %?\n%U\n" :clock-in t :clock-resume t)
                  ("w" "org-protocol" entry (file "~/org/planner/refile.org")
                   "* TODO Review %c\n%U\n" :immediate-finish t)
                  ("m" "Meeting" entry (file "~/org/planner/refile.org")
                   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                  ("v" "Vocabulary drill" entry (file "~/org/drills/English/vocabulary.org")
                   "* %^{The word} :drill:\n %^{Extended word (may be empty)} \n** Answer \n%^{The definition}")
                  ("e" "Emacs drill" entry (file "~/org/drills/emacs.org")
                   "* %^{Question} :drill:\n %^{Extended question (may be empty)} \n** Answer \n%^{The Answer}")
                  ("p" "Phone call" entry (file "~/org/planner/refile.org")
                   "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                  ("h" "Habit" entry (f ile "~/org/planner/refile.org")
                   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

  ;; babel
  ;; (require 'ob-scala)
  ;; (require 'ob-sh)
  ;; (require 'ob-js)
  ;; (require 'ob-haskell)
  ;; (require 'ob-dot)
;;;;; org-bullets
  ;; (use-package org-bullets
  ;;   :config (add-hook 'org-mode-hook 'org-bullets-mode))

;;;;; org-clock
  ;; (use-package org-clock
  ;;   :init
  ;;   (setq org-clock-idle-time 15)
  ;;   (setq org-clock-in-resume t)
  ;;   (setq org-clock-persist t)
  ;;   (setq org-clock-persist-query-resume nil)
  ;;   (setq org-clock-clocked-in-display 'both)
  ;;   (setq org-clock-frame-title-format
  ;;         (append '((t org-mode-line-string)) '(" ") frame-title-format))
  ;;   (when (executable-find "xprintidle")
  ;;     (setq org-x11idle-exists-p t)
  ;;     (setq org-clock-x11idle-program-name "xprxointidle"))

  ;;   :config (org-clock-persistence-insinuate))
  )
;;;; org-contrib
;; org contrib files
;; (use-package org-contib
;;   :quelpa (org-contrib :url "git://orgmode.org/org-mode.git"
;;                        :fetcher git
;;                        :files ("contrib/lisp/*.el"))

;;   :init
;;   (setq org-directory "~/org/planner")
;;   ;;(setq org-modules (quote (org-habit)))

;;   (setq org-agenda-files (quote ("~/org"
;;                                  "~/org/planner"
;;                                  "~/org/notes")))


;;   (setq org-default-notes-file "~/org/planner/refile.org")
;;   (setq org-todo-keywords
;;         (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
;;                 (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

;;   (setq org-todo-state-tags-triggers
;;         (quote (("CANCELLED" ("CANCELLED" . t))
;;                 ("WAITING" ("WAITING" . t))
;;                 ("HOLD" ("WAITING" . t) ("HOLD" . t))
;;                 (done ("WAITING") ("HOLD"))
;;                 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
;;                 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
;;                 ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;;   (setq org-todo-keyword-faces
;;         (quote (("TODO" :foreground "red" :weight bold)
;;                 ("NEXT" :foreground "blue" :weight bold)
;;                 ("DONE" :foreground "forest green" :weight bold)
;;                 ("WAITING" :foreground "orange" :weight bold)
;;                 ("HOLD" :foreground "magenta" :weight bold)
;;                 ("CANCELLED" :foreground "forest green" :weight bold)
;;                 ("MEETING" :foreground "forest green" :weight bold)
;;                 ("PHONE" :foreground "forest green" :weight bold))))


;;   (setq org-log-done (quote time))
;;   (setq org-log-into-drawer t)
;;   (setq org-enforce-todo-dependencies t)

;;   ;; Keep tasks with dates on the global todo lists
;;   (setq org-agenda-todo-ignore-with-date nil)

;;   ;; Keep tasks with deadlines on the global todo lists
;;   (setq org-agenda-todo-ignore-deadlines nil)

;;   ;; Keep tasks with scheduled dates on the global todo lists
;;   (setq org-agenda-todo-ignore-scheduled nil)

;;   ;; Remove completed deadline tasks from the agenda view
;;   (setq org-agenda-skip-deadline-if-done t)

;;   ;; Remove completed scheduled tasks from the agenda view
;;   (setq org-agenda-skip-scheduled-if-done t)

;;   ;; Remove completed items from search results
;;   (setq org-agenda-skip-timestamp-if-done t)

;;                                         ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
;;   (setq org-refile-targets `((nil :maxlevel . 9)
;;                              (org-agenda-files :maxlevel . 9)))

;;                                         ; Use full outline paths for refile targets - we file directly with IDO
;;   (setq org-refile-use-outline-path `file)

;;                                         ; Targets complete directly with IDO
;;   (setq org-outline-path-complete-in-steps nil)

;;   ;; Allow refile to create parent tasks with confirmation
;;   (setq org-refile-allow-creating-parent-nodes (quote confirm))

;;   (setq org-indirect-buffer-display 'current-window)

;;   ;; controlling display of leading stars on headlines
;;   (setq org-hide-leading-stars nil)
;;   (setq org-startup-indent t)
;;   (setq org-cycle-separator-lines 0)


;;   (setq org-src-fontify-natively t)
  
;;   :config
;;   (require 'org-table)

;;   (use-package org-pomodoro
;;     :init

;;     (setq org-pomodoro-ask-upon-killing nil)
;;     :config

;;   )
