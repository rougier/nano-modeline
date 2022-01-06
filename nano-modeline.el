;;; nano-modeline.el --- N Λ N O modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; Nano modeline is a minor mode that modify the modeline as:
;; [ prefix | name (primary)                      secondary ]
;;
;; It can be displayed at the bottom (mode-line) or at the top (header-line)
;; depending on nano-modeline-position custom setting.
;;
;; There are two sets of faces (for active and inactive modelines) that
;; can be customized (M-x: customize-group + nano-modeline)
;;
;; - nano-modeline-active-name      / nano-modeline-inactive-name
;; - nano-modeline-active-primary   / nano-modeline-inactive-primary
;; - nano-modeline-active-secondary / nano-modeline-inactive-secondary
;; - nano-modeline-active-status-RO / nano-modeline-inactive-status-RO
;; - nano-modeline-active-status-RW / nano-modeline-inactive-status-RW
;; - nano-modeline-active-status-** / nano-modeline-inactive-status-**
;;
;; Usage example:
;;
;; M-x: nano-modeline-mode
;;
;;; NEWS:
;;
;; Version 0.5
;; - Dynamic version that is now configurable thanks to the wonderful
;;   contribution of Hans Donner (@hans-d)
;;
;; Version 0.4
;; - Reverted to RO/RW/** default prefix
;;
;; Version 0.3
;; - Usage of :align-to: properties for better alignment
;; - Added elpher mode
;; - Fix user mode
;;
;; Version 0.2
;; - Implements modeline as minor mode
;;
;; Version 0.1
;; - Submission to ELPA
;;

;;; Code:
(defgroup nano nil
  "N Λ N O"
  :group 'convenience)

(defgroup nano-modeline nil
  "N Λ N O Modeline"
  :group 'nano)

(defgroup nano-modeline-active nil
  "Active modeline faces.

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'nano-modeline)

(defgroup nano-modeline-inactive nil
  "Inactive modeline faces

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'nano-modeline)

(defcustom nano-modeline-position 'top
  "Default position (top or bottom)"
  :type '(choice (const :tag "Top"    top)
                 (const :tag "Bottom" bottom))
  :group 'nano-modeline)

(defcustom nano-modeline-space-top +0.100
  "Space adjustment for top of modeline
Possitive is upwards"
  :type 'float
  :group 'nano-modeline)

(defcustom nano-modeline-space-bottom -0.125
  "Space adjustment for bottom of modeline
Negative is downwards."
  :type 'float
  :group 'nano-modeline)

(defface nano-modeline-active
  '((t (:inherit mode-line)))
  "Modeline face for active modeline"
  :group 'nano-modeline-active)

(defface nano-modeline-active-name
  '((t (:inherit (mode-line bold))))
  "Modeline face for active name element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-primary
  '((t (:inherit mode-line)))
  "Modeline face for active primary element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-RO
  '((t (:inherit mode-line)))
  "Modeline face for active READ-ONLY element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-RW
  '((t (:inherit mode-line)))
  "Modeline face for active READ-WRITE element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-**
  '((t (:inherit mode-line)))
  "Modeline face for active MODIFIED element"
  :group 'nano-modeline-active)

(defface nano-modeline-inactive
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive window"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-name
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive name element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-primary
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive primary element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-secondary
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive primary element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-RO
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive READ-ONLY element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-RW
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive READ-WRITE element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-**
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive MODIFIED element"
  :group 'nano-modeline-inactive)

(defcustom nano-modeline-mode-formats
  '(;; with :mode-p first
    (buffer-menu-mode       :mode-p nano-modeline-buffer-menu-mode-p
                            :format nano-modeline-buffer-menu-mode
                            :on-activate nano-modeline-buffer-menu-activate
                            :on-inactivate nano-modeline-buffer-menu-inactivate)
    (calendar-mode          :mode-p nano-modeline-calendar-mode-p
                            :format nano-modeline-calendar-mode
                            :on-activate nano-modeline-calendar-activate
                            :on-inactivate nano-modeline-calendar-inactivate)
    (completion-list-mode   :mode-p nano-modeline-completion-list-mode-p
                            :format nano-modeline-completion-list-mode)
    (deft-mode              :mode-p nano-modeline-deft-mode-p
                            :format nano-modeline-deft-mode)
    (doc-view-mode          :mode-p nano-modeline-doc-view-mode-p
                            :format nano-modeline-doc-view-mode)
    (elfeed-search-mode     :mode-p nano-modeline-elfeed-search-mode-p
                            :format nano-modeline-elfeed-search-mode
                            :on-activate nano-modeline-elfeed-search-activate
                            :on-inactivate nano-modeline-elfeed-search-inactivate)
    (elfeed-show-mode       :mode-p nano-modeline-elfeed-show-mode-p
                            :format nano-modeline-elfeed-show-mode)
    (elpher-mode            :mode-p nano-modeline-elpher-mode-p
                            :format nano-modeline-elpher-mode
                            :on-activate nano-modeline-elpher-activate)
    (info-mode              :mode-p nano-modeline-info-mode-p
                            :format nano-modeline-info-mode
                            :on-activate nano-modeline-info-activate
                            :on-inactivate nano-modeline-info-inactivate)
    (messages-mode          :mode-p nano-modeline-messages-mode-p
                            :format nano-modeline-messages-mode)
    (mu4e-compose-mode      :mode-p nano-modeline-mu4e-compose-mode-p
                            :format nano-modeline-mu4e-compose-mode)
    (mu4e-dashboard-mode    :mode-p nano-modeline-mu4e-dashboard-mode-p
                            :format nano-modeline-mu4e-dashboard-mode)
    (mu4e-headers-mode      :mode-p nano-modeline-mu4e-headers-mode-p
                            :format nano-modeline-mu4e-headers-mode)
    (mu4e-loading-mode      :mode-p nano-modeline-mu4e-loading-mode-p
                            :format nano-modeline-mu4e-loading-mode)
    (mu4e-main-mode         :mode-p nano-modeline-mu4e-main-mode-p
                            :format nano-modeline-mu4e-main-mode)
    (mu4e-view-mode         :mode-p nano-modeline-mu4e-view-mode-p
                            :format nano-modeline-mu4e-view-mode)
    (nano-help-mode         :mode-p nano-modeline-nano-help-mode-p
                            :format nano-modeline-nano-help-mode)
    (org-agenda-mode        :mode-p nano-modeline-org-agenda-mode-p
                            :format nano-modeline-org-agenda-mode)
    (org-capture-mode       :mode-p nano-modeline-org-capture-mode-p
                            :format nano-modeline-org-capture-mode
                            :on-activate nano-modeline-org-capture-activate
                            :on-inactivate nano-modeline-org-capture-inactivate)
    (org-clock-mode         :mode-p nano-modeline-org-clock-mode-p
                            :format nano-modeline-org-clock-mode
                            :on-activate nano-modeline-org-clock-activate
                            :on-inactivate nano-modeline-org-clock-inactivate)
    (pdf-view-mode          :mode-p nano-modeline-pdf-view-mode-p
                            :format nano-modeline-pdf-view-mode)
    (prog-mode              :mode-p nano-modeline-prog-mode-p
                            :format nano-modeline-prog-mode)
    (term-mode              :mode-p nano-modeline-term-mode-p
                            :format nano-modeline-term-mode)
    (text-mode              :mode-p nano-modeline-text-mode-p
                            :format nano-modeline-text-mode)
    (vterm-mode             :mode-p nano-modeline-vterm-mode-p
                            :format nano-modeline-term-mode)

    ;; hooks only last
    (ein-notebook-mode      :on-activate nano-modeline-ein-notebook-activate
                            :on-inactivate nano-modeline-ein-notebook-inactivate)
    (esh-mode               :on-activate nano-modeline-esh-activate
                            :on-inactivate nano-modeline-esh-inactivate)
    (ispell-mode            :on-activate nano-modeline-ispell-activate
                            :on-inactivate nano-modeline-ispell-inactivate)
    (mu4e-mode              :on-activate nano-modeline-mu4e-activate
                            :on-inactivate nano-modeline-mu4e-inactivate))
  "Modes to be evalued for modeline.
KEY mode name, for reference only. Easier to do lookups and/or replacements.
:MODE-P the function to check if :FORMAT needs to be used, first one wins.
:ON-ACTIVATE and :ON-INACTIVATE do hook magic on enabling/disabling the mode.
"
  :type '(alist :key-type symbol
                :value-type (plist :key-type (choice (const :mode-p)
                                                     (const :format)
                                                     (const :on-activate)
                                                     (const :on-inactivate))
                                   :value-type function))
  :group 'nano-modeline)

(defcustom nano-modeline-mode-format-activate-hook nil
  "Add hooks on activation of the mode, for those modes that do their own mode-line magic"
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'nano-modeline)

(defcustom nano-modeline-mode-format-inactivate-hook nil
  "Remove hooks on de-activation of the mode, for those modes that do their own mode-line magic"
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'nano-modeline)

(defcustom nano-modeline-default-mode-format 'nano-modeline-default-mode
  "Default mode to evaluate if no match could be found in `nano-modelines-mode-formats'"
  :type 'function
  :group 'nano-modeline)

(defcustom nano-modeline-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'nano-modeline)
(make-obsolete-variable nano-modeline-user-mode "Add to `nano-modeline-mode-formats' instead")

(defcustom nano-modeline-user-mode-p nil
  "Function to indicate whether the user supplied mode should be used instead f the default one. This function will be dynamically called and can return t or nil depending on some user conditions. If the provied function always return t, this fully overrides the nano-modeline."
  :type '(choice (const nil) function)
  :group 'nano-modeline)
(make-obsolete-variable nano-modeline-user-mode-p "Add to `nano-modeline-mode-formats' instead")

(defun nano-modeline-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

(defun nano-modeline-vc-branch ()
  "Return current VC branch if any."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun nano-modeline-mode-name ()
  "Return current major mode name"
  (format-mode-line mode-name))

;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun nano-modeline-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; ---------------------------------------------------------------------
(defun nano-modeline-status ()
  "Return buffer status, one of 'read-only, 'modified or 'read-write."

  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  'modified)
          (read-only 'read-only)
          (t         'read-write))))

(defun nano-modeline-render (prefix name primary secondary &optional status)
  "Compose a string with provided information"
  (let* ((window (get-buffer-window (current-buffer)))
         (name-max-width (- (window-body-width)
                            1
                            (length prefix)
                            1
                            (length primary)
                            5
                            (length secondary)
                            1))
         (name (if (and (stringp name) (> (length name) name-max-width))
                   (format "%s…" (substring name 0 (- name-max-width 1)))
                 name))
         (status (or status (nano-modeline-status)))
         (active (eq window nano-modeline--selected-window))

         (prefix (or prefix (cond ((eq status 'read-only)  "RO")
                                  ((eq status 'read-write) "RW")
                                  ((eq status 'modified)   "**")
                                  (t                       "--"))))
         
         (prefix-face (cond ((eq status 'read-only) (if active
                                                        'nano-modeline-active-status-RO
                                                      'nano-modeline-inactive-status-RO))
                            ((eq status 'modified) (if active
                                                       'nano-modeline-active-status-**
                                                     'nano-modeline-inactive-status-**))
                            ((eq status 'read-write) (if active
                                                         'nano-modeline-active-status-RW
                                                       'nano-modeline-inactive-status-RW))
                            ((facep status) status)
                            ((listp status) (if active (car status)
                                              (cadr status)))
                            (t (if active 'nano-modeline-active
                                 'nano-modeline-inactive))))
         (left (concat (if (stringp prefix)
                           (propertize (format " %s " prefix)
                                       'face `(:inherit ,prefix-face)))
                       (propertize " " 'display `(raise ,nano-modeline-space-top))
                       (propertize name 'face (if active 'nano-modeline-active-name
                                                'nano-modeline-inactive-name))
                       (if (length name) " ")
                       (propertize primary 'face (if active 'nano-modeline-active-primary
                                                   'nano-modeline-inactive-primary))))
         (right (concat (propertize secondary 'face (if active 'nano-modeline-active-secondary
                                                      'nano-modeline-inactive-secondary))
                        (propertize " " 'display `(raise ,nano-modeline-space-bottom)))))
    (concat
     left 
     (propertize " " 'display `(space :align-to (- right ,(+ (length secondary) 1))))
     right)))

;; ---------------------------------------------------------------------
(defun nano-modeline-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (nano-modeline-render nil
                           buffer-name
                           ""
                           (ein:header-line)
                           (if (ein:notebook-modified-p)
                               'modified
                             'read-write))))

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the nano-modeline function to set
;; the header format in a notebook buffer. Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(defun nano-modeline-ein-notebook-activate ()
  (with-eval-after-load 'ein
    (if (eq nano-modeline-position 'top)
        (setq ein:header-line-format '((:eval (nano-modeline-ein-notebook-mode)))))))

(defun nano-modeline-ein-notebook-inactivate ()
  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line)))))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun nano-modeline-elfeed-search-mode ()
  (let* ((prefix "NEWS")
        (no-database (zerop (elfeed-db-last-update)))
        (update      (> (elfeed-queue-count-total) 0))

        (name  (cond (no-database "No database")
                     (update      "Update:") 
                     (t           "Search:")))
        (primary (cond  (no-database "")
                        (update
                         (let ((total (elfeed-queue-count-total))
                               (in-process (elfeed-queue-count-active)))
                           (format "%d jobs pending, %d active..."
                                   (- total in-process) in-process)))
                        (t  (let* ((db-time (seconds-to-time (elfeed-db-last-update)))
                                   (unread ))
                              (cond (elfeed-search-filter-active "")
                                    ((string-match-p "[^ ]" elfeed-search-filter)
                                     elfeed-search-filter)
                                    (""))))))
        (secondary (cond
                    ((zerop (elfeed-db-last-update)) "")
                    ((> (elfeed-queue-count-total) 0) "")
                    (t (elfeed-search--count-unread)))))
    (nano-modeline-render nil name primary secondary)))

;; Elfeed uses header-line, we need to tell it to use our own format
(defun nano-modeline-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

(defun nano-modeline-elfeed-search-activate ()
  (with-eval-after-load 'elfeed
    (if (eq nano-modeline-position 'top)
        (setq elfeed-search-header-function #'nano-modeline-elfeed-setup-header))))

(defun nano-modeline-elfeed-search-inactivate ()
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header)))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun nano-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (nano-modeline-render nil
                          title
                          ;;(nano-modeline-truncate title 40)
                           (concat "(" tags-str ")")
                           feed-title)))

;; ---------------------------------------------------------------------
(defun nano-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun nano-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(defun nano-modeline-calendar-setup-header ()
  (setq header-line-format "")
  (face-remap-add-relative
   'header-line `(:overline ,(face-foreground 'default)
                            :height 0.5
                            :background ,(face-background 'default))))

;; From https://emacs.stackexchange.com/questions/45650
;; (add-to-list 'display-buffer-alist
;;              `(,(rx string-start "*Calendar*" string-end)
;;               (display-buffer-below-selected)))

(defun nano-modeline-calendar-activate ()
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'nano-modeline-calendar-setup-header)))

(defun nano-modeline-calendar-inactivate ()
  (remove-hook 'calendar-initial-window-hook
               #'nano-modeline-calendar-setup-header))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-render nil
                        "Capture"
                        (concat "(" (org-capture-get :description) ")")
                        ""))

(defun nano-modeline-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

(defun nano-modeline-org-capture-activate ()
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'nano-modeline-org-capture-turn-off-header-line)))

(defun nano-modeline-org-capture-inactivate ()
  (remove-hook 'org-capture-mode-hook
               #'nano-modeline-org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
    (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
    line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                     crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
         (if (not (equal node "Top")) node
           (format "%s"
               (if (stringp Info-current-file)
               (file-name-sans-extension
                (file-name-nondirectory Info-current-file))
             Info-current-file)))))
    (setq line (concat line (if (null line) "" " > ")
                                (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun nano-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun nano-modeline-info-mode ()
  (nano-modeline-render nil
                        (nano-modeline-info-breadcrumbs)
                        ""
                         ""))

(defun nano-modeline-info-activate ()
  (if (eq nano-modeline-position 'top)
      (setq Info-use-header-line nil)))

(defun nano-modeline-info-inactivate ()
  (custom-reevaluate-setting 'Info-use-header-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-enlarge-ispell-choices-buffer (buffer)
  (when (string= (buffer-name buffer) "*Choices*")
    (with-current-buffer buffer
      ;; (enlarge-window +2)
      (setq-local header-line-format nil)
      (setq-local mode-line-format nil))))

(defun nano-modeline-ispell-activate ()
  (with-eval-after-load 'ispell
    (advice-add #'ispell-display-buffer :after
                #'nano-modeline-enlarge-ispell-choices-buffer)))

(defun nano-modeline-ispell-inactivate ()
  (advice-remove #'ispell-display-buffer
                 #'nano-modeline-enlarge-ispell-choices-buffer))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-render nil
                         "Agenda"
                         ""
;;                         (format "%s" org-agenda-span-name)
                         (format-time-string "%A %-e %B %Y")))

;; ---------------------------------------------------------------------

(defun nano-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun nano-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun nano-modeline-term-mode ()
  (nano-modeline-render ">_"
                         shell-file-name
                         (if (term-in-char-mode)
                             "(char mode)"
                           "(line mode)")
                         (nano-modeline-shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun nano-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))

(defun nano-modeline-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (version< "1.6.10" mu4e-mu-version)
      mu4e--server-props
    mu4e~server-props))

(defun nano-modeline-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'nano-modeline)))

(defun nano-modeline-mu4e-inactivate ()
  (advice-remove #'mu4e~header-line-format #'nano-modeline))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun nano-modeline-mu4e-dashboard-mode ()
  (nano-modeline-render nil
                         (format "%d messages"
                                 (plist-get (nano-modeline-mu4e-server-props) :doccount))
                         ""
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun nano-modeline-mu4e-loading-mode ()
  (nano-modeline-render nil
                         "Loading…"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun nano-modeline-mu4e-main-mode ()
  (nano-modeline-render nil
                        (nano-modeline-mu4e-context)
                        ""
                        (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-compose-mode-p ()
  (derived-mode-p 'mu4e-compose-mode))

(defun nano-modeline-mu4e-compose-mode ()
  (nano-modeline-render nil
                        (format-mode-line "%b")
                        ""
                        (format "[%s]"
                                (nano-modeline-mu4e-quote
                                 (mu4e-context-name (mu4e-context-current))))))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-quote (str)
  (if (version< "1.6.5" mu4e-mu-version)
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun nano-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun nano-modeline-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (nano-modeline-render nil
                          "Search:"
                          (or (nano-modeline-mu4e-quote
                               (nano-modeline-mu4e-last-query)) "")
                           (format "[%s]"
                                   (nano-modeline-mu4e-quote
                                    (mu4e-context-name (mu4e-context-current)))))))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun nano-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (nano-modeline-render nil
                          (or subject "")
                          ""
                          (or from "")
                          'read-only)))

(defun nano-modeline-mu4e-view-hook ()
  (setq header-line-format "%-")
  (face-remap-add-relative 'header-line
                           '(:background "#ffffff"
                                         :underline nil
                                         :box nil
                                         :height 1.0)))
;; (add-hook 'mu4e-view-mode-hook #'nano-modeline-mu4e-view-hook)

;; ---------------------------------------------------------------------
(defun nano-modeline-nano-help-mode-p ()
  (derived-mode-p 'nano-help-mode))

(defun nano-modeline-nano-help-mode ()
  (nano-modeline-render nil
                         "Emacs / N Λ N O"
                         "(help)"
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-messages-mode-p ()
  (derived-mode-p 'messages-buffer-mode))

(defun nano-modeline-messages-mode ()
  (nano-modeline-render nil "Messages" "" ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (stringp org-mode-line-string)))

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (branch      (nano-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-render nil
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

(defun nano-modeline-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun nano-modeline-org-clock-activate ()
  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'nano-modeline-org-clock-out)))

(defun nano-modeline-org-clock-inactivate ()
  (remove-hook 'org-clock-out-hook
               #'nano-modeline-org-clock-out))

;; ---------------------------------------------------------------------
(defun nano-modeline-esh-activate ()
  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil)))

(defun nano-modeline-esh-inactivate ()
  (custom-reevaluate-setting 'eshell-status-in-mode-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-doc-view-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun nano-modeline-doc-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
    (mode-name   (nano-modeline-mode-name))
    (branch      (nano-modeline-vc-branch))
    (page-number (concat
              (number-to-string (doc-view-current-page)) "/"
              (or (ignore-errors
                (number-to-string (doc-view-last-page-number)))
              "???"))))
    (nano-modeline-render nil
                           buffer-name
                           (if branch (concat "(" branch ")") "")
                           page-number)))

;; ---------------------------------------------------------------------
(defun nano-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(defun nano-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
    (mode-name   (nano-modeline-mode-name))
    (branch      (nano-modeline-vc-branch))
    (page-number (concat
              (number-to-string (pdf-view-current-page)) "/"
              (or (ignore-errors
                (number-to-string (pdf-cache-number-of-pages)))
              "???"))))
    (nano-modeline-render nil
                          buffer-name
                          (if branch (concat "(" branch ")") "")
                          page-number)))

;; ---------------------------------------------------------------------
(defun nano-modeline-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun nano-modeline-buffer-menu-mode ()
    (let ((buffer-name "Buffer list")
          (mode-name   (nano-modeline-mode-name))
          (position    (format-mode-line "%l:%c")))

      (nano-modeline-render nil
                             buffer-name "" position)))
;;(defun buffer-menu-mode-header-line ()
;;  (face-remap-add-relative
;;   'header-line `(:background ,(face-background 'nano-subtle))))
;;(add-hook 'Buffer-menu-mode-hook
;;          #'buffer-menu-mode-header-line)

(defun nano-modeline-buffer-menu-activate ()
  (if (eq nano-modeline-position 'top)
      (setq Buffer-menu-use-header-line nil)))

(defun nano-modeline-buffer-menu-inactivate ()
  (custom-reevaluate-setting 'Buffer-menu-use-header-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-elpher-mode-p ()
  (derived-mode-p 'elpher-mode))

(defun nano-modeline-elpher-mode ()
  (let* ((display-string (elpher-page-display-string elpher-current-page))
         (sanitized-display-string (replace-regexp-in-string "%" "%%" display-string))
         (address (elpher-page-address elpher-current-page))
         (tls-string (if (and (not (elpher-address-about-p address))
                              (member (elpher-address-protocol address)
                                      '("gophers" "gemini")))
                         "(TLS encryption)"
                       "")))
    (nano-modeline-render nil
                          sanitized-display-string
                          tls-string
                          "")))

(defun nano-modeline-elpher-activate ()
  (with-eval-after-load 'elpher
    (setq elpher-use-header nil)))

;; ---------------------------------------------------------------------
(defun nano-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (position    (format-mode-line "%l:%c")))

      (nano-modeline-render nil
                            buffer-name
                            ""
                            position)))

;; ---------------------------------------------------------------------
(with-eval-after-load 'deft
  (defun nano-modeline-deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun nano-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun nano-modeline-deft-mode ()
  (let ((prefix (nano-modeline-status))
        (primary "Notes")
        (filter  (concat (if deft-filter-regexp
                             (deft-whole-filter-regexp)) "_"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (nano-modeline-render nil "Search:" filter matches 'read-only)))

;; ---------------------------------------------------------------------
(defun nano-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun nano-modeline-prog-mode ()
  (nano-modeline-default-mode))

(defun nano-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun nano-modeline-text-mode ()
  (nano-modeline-default-mode))

(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (branch      (nano-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-render nil ;; (upcase  mode-name)
                            buffer-name
                            (if branch (concat "(" branch ")") "")
                            position)))

;; ---------------------------------------------------------------------
(defun nano-modeline-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))

;; ---------------------------------------------------------------------
(defvar nano-modeline--saved-mode-line-format nil)
(defvar nano-modeline--saved-header-line-format nil)
(defvar nano-modeline--selected-window nil)

(defun nano-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq nano-modeline--selected-window (selected-window)))

(defun nano-modeline ()
  "Build and set the modeline."
  (let* ((format
          '((:eval
             (funcall
              (or (catch 'found
                    (dolist (elt nano-modeline-mode-formats)
                      (let* ((config (cdr elt))
                             (mode-p (plist-get config :mode-p))
                             (format (plist-get config :format)))
                        (when mode-p
                          (when (funcall mode-p)
                            (throw 'found format))))))
                  nano-modeline-default-mode-format))))))
    (if (eq nano-modeline-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))

(defun nano-modeline-update-windows ()
  "Hide the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
          (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (setq mode-line-format
                  (cond ((one-window-p t) (list ""))
                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                        ((not (window-in-direction 'below)) (list ""))
                        (t nil))))))))

(defun nano-modeline-mode--activate ()
  "Activate nano modeline"

  ;; Save current mode-line and header-line
  (unless nano-modeline--saved-mode-line-format
    (setq nano-modeline--saved-mode-line-format mode-line-format)
    (setq nano-modeline--saved-header-line-format header-line-format))

  (dolist (elt nano-modeline-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-activate)))
      (when fn (funcall fn))))

  (run-hooks 'nano-modeline-mode-format-activate-hook)

  ;; Update selected window
  (nano-modeline--update-selected-window)
  ;; (setq nano-modeline--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (nano-modeline)

  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'nano-modeline--update-selected-window)

  ;; This hooks hide the modeline for windows having a window below them
  ;; Disabled for the time being,
  ;;  -> see https://github.com/rougier/nano-modeline/issues/24
  ;; (add-hook 'window-configuration-change-hook #'nano-modeline-update-windows)

  (force-mode-line-update t))

(defun nano-modeline-mode--inactivate ()
  "Inactivate nano mode line and restored default mode-line"

  (dolist (elt nano-modeline-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-inactivate)))
      (when fn (funcall fn))))

  (run-hooks 'nano-modeline-mode-format-inactivate-hook)

  (remove-hook 'post-command-hook
               #'nano-modeline--update-selected-window)
  (remove-hook 'window-configuration-change-hook
               #'nano-modeline-update-windows)

  (setq         mode-line-format nano-modeline--saved-mode-line-format)
  (setq-default mode-line-format nano-modeline--saved-mode-line-format)
  (setq         header-line-format nano-modeline--saved-header-line-format)
  (setq-default header-line-format nano-modeline--saved-header-line-format))

;;;###autoload
(define-minor-mode nano-modeline-mode
  "Toggle nano-modeline minor mode"
  :group 'nano-modeline
  :global t
  :init-value nil

  (if nano-modeline-mode
      (nano-modeline-mode--activate)
    (nano-modeline-mode--inactivate))

  ;; Run any registered hooks
  (run-hooks 'nano-modeline-mode-hook))


(provide 'nano-modeline)
;;; nano-modeline.el ends here
