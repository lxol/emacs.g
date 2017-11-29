(use-package org
  :mode
  ("\\.org_archive$" . org-mode)
  ("\\.txt$" . org-mode)
  ("\\.org$\\'" . org-mode)
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c C-9" . org-insert-subheading)
   ("C-c C-0" . org-insert-todo-subheading)
   )

  :config

  (setq org-agenda-files (quote ("~/org"
                                 "~/org/planner/"
                                 "~/org/journal/"
                                 "~/org/notes/")))
  (use-package org-capture
    :config
    (setq org-capture-templates
          (quote (("t" "todo" entry (file "~/org/planner/refile.org")
                   "* TODO %?\n" :clock-in t :clock-resume t)))))
  (use-package org-journal
    :config
    (setq org-journal-dir "~/org/journal")
    (setq org-journal-file-format "%Y%m%d.org"))
  )
