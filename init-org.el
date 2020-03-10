
(setq org-directory "")

(use-package lxol-org
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
   ("C-c o" . hydra-org-clock/body)
   :map org-mode-map
   ("C-<tab>". nil )
   :map org-agenda-mode-map
   ("C-c o" . hydra-org-agenda/body)
   ("v" . hydra-org-agenda-view/body))
  :config
  ;; (add-to-list 'org-structure-template-alist '("S" . "src scala"))
  )

;; (add-to-list 'org-structure-template-alist '("S" . "src scala"))
(setq org-agenda-skip-scheduled-if-done t)

(setq org-default-notes-file (concat org-directory "~/org/notes/notes.org"))

(setq org-agenda-files (quote (
                               "~/org/planner/"
                               "~/org/planner/new-job"
                               "~/org/journal/"
                               "~/org/notes/notes.org"
                               "~/org/notes/english.org")))

(use-package org-capture
  :config

  (setq org-use-property-inheritance t)
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/org/planner/refile.org")
                 "* TODO %?\n" )
                ("n" "note" entry (file "~/org/notes/notes.org")
                 "* %? :NOTE:\n%U\n" )
                ("n" "note" entry (file "~/org/notes/work-notes.org")
                 "* %? :NOTE:\n%U\n" )
                ("j" "Journal" entry (file+datetree "~/org/planner/diary.org")
                 "* %?\n%U\n")
                ))))


(setq org-refile-targes org-agenda-files)

(use-package org-journal
  :config
  (setq org-journal-dir "~/org/journal")
  (setq org-journal-file-format "%Y%m%d.org"))

   ;; | =anki-editor-push-notes=             | Push notes to Anki. Additional arguments can be used to restrict the range of notes. |
   ;; | =anki-editor-retry-failure-notes=    | Same as above, except that it only pushes notes that have =ANKI_FAILURE_REASON=.     |
   ;; | =anki-editor-insert-note=            | Insert a note entry like =M-RET=, interactively.                                     |
   ;; | =anki-editor-cloze-region=           | Create a cloze deletion from region.                                                 |
   ;; | =anki-editor-export-subtree-to-html= | Export the subtree at point to HTML.                                                 |
   ;; | =anki-editor-convert-region-to-html= | Convert and replace region to HTML.                                                  |
(use-package anki-editor
  :bind (:map org-mode-map
              ("C-M-s" . anki-editor-push-notes)
              ("C-M-n" . anki-editor-insert-note)
              ("C-M-r" . anki-editor-cloze-region))
  )

(defhydra hydra-org-clock (:color blue :hint nil)
  "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    _i_n         _e_dit   _g_oto entry | (_z_)     _r_elative      ti_m_e
^ ^      _c_ontinue   _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
^ ^      _o_ut        ^ ^      _r_eport     |  ^ ^      _p_ause toggle
^ ^      ^ ^          ^ ^      ^ ^          |  ^ ^      _s_top
"
   ("i" org-clock-in)
   ("c" org-clock-in-last)
   ("o" org-clock-out)
   
   ("e" org-clock-modify-effort-estimate)
   ("q" org-clock-cancel)

   ("g" org-clock-goto)
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

(defhydra hydra-org-agenda-clock (:color blue :hint nil)
  ("i" org-agenda-clock-in)
  ("o" org-agenda-clock-out)
  ("q" org-agenda-clock-cancel)
  ("g" org-agenda-clock-goto)
  )


;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil))
  "
Org agenda (_q_uit)
^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
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


(defun org-agenda-cts ()
  (let ((args (get-text-property
               (min (1- (point-max)) (point))
               'org-last-args)))
    (nth 2 args)))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
_w_: ?w? week       _[_: inactive      _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
_m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
_y_: ?y? year       _q_: quit          _L__l__c_: ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view
       (if (eq 'day (org-agenda-cts))
           "[x]" "[ ]"))
  ("w" org-agenda-week-view
       (if (eq 'week (org-agenda-cts))
           "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view
       (if (eq 'fortnight (org-agenda-cts))
           "[x]" "[ ]"))
  ("m" org-agenda-month-view
       (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view
       (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode
       (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode
       (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode
       (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode
       (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid
       (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary
       (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("["
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" (message "Abort") :exit t))

(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 6 :fileskip0 t :compact t :narrow 60 :score 0))

(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))

(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

(setq org-clock-history-length 23)

(defun eos/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

(global-set-key (kbd "C-c I") #'eos/org-clock-in)
(global-set-key (kbd "C-c O") #'org-clock-out)

(setq org-clock-in-switch-to-state "NEXT")
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks
;; with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; use pretty things for the clocktable
(setq org-pretty-entities t)
(setq org-todo-keywords
  '((sequence "TODO(t)"
      "MAYBE(m)"
      "NEXT(n)"
      "WAITING(w)"
      "|"
      "DONE(d)"
      "DEFERRED(r)"
      "CANCELLED(c)")))
(setq org-log-into-drawer 'LOGBOOK)

(setq before "before")

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c n n" . org-projectile-capture-for-current-project)
         ("C-c c" . org-capture))
  :demand
  :config
  (progn
    (setq org-projectile-projects-file "/home/lxol/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates))
  )

;; (use-package org-projectile
;;   :init
;;   (setq sss 1)
;;   :no-require t
;;   :demand t)

(setq after "after")
;; (use-package org-trello)
(setq use-package-verbose t)

(use-package deadgrep)
