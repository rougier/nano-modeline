;;; nano-modeline.el --- N Λ N O modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 0.2
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
;; [ status | name (primary)                               secondary ]
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
;; Vesrion 0.2
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
  "N Λ N O modeline"
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

(defcustom nano-modeline-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'nano-modeline)

(defun nano-modeline-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  nano-modeline-user-mode)

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


(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
     (active        (eq window nano-modeline--selected-window))
         (space-up       +0.20)
         (space-down     -0.25)
         (prefix (cond ((string= status "RO")
                        (propertize (if (window-dedicated-p)"•RO " " RO ")
                                    'face (if active
                                              'nano-modeline-active-status-RO
                                            'nano-modeline-inactive-status-RO)))
                       ((string= status "**")
                        (propertize (if (window-dedicated-p)"•** " " ** ")
                                    'face (if active
                                              'nano-modeline-active-status-**
                                            'nano-modeline-inactive-status-**)))
                       ((string= status "RW")
                        (propertize (if (window-dedicated-p) "•RW " " RW ")
                                    'face (if active 'nano-modeline-active-status-RW
                                            'nano-modeline-inactive-status-RW)))
                       (t (propertize status
                                      'face (if active 'nano-modeline-active-status-**
                                              'nano-modeline-inactive-status-**)))))
         (left (concat
                (propertize " "  'face (if active 'nano-modeline-active
                                         'nano-modeline-inactive)
                            'display `(raise ,space-up))
                (propertize name 'face (if active 'nano-modeline-active-name
                                         'nano-modeline-inactive-name))
                (propertize " "  'face (if active 'nano-modeline-active
                                         'nano-modeline-inactive)
                            'display `(raise ,space-down))
                (propertize primary 'face (if active 'nano-modeline-active-primary
                                            'nano-modeline-inactive-primary))))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
                             (length prefix) (length left) (length right)
                             (/ (window-right-divider-width) char-width)))
     (available-width (max 1 available-width)))
    (concat prefix
            left
            (propertize (make-string available-width ?\ )
                        'face (if active 'nano-modeline-active
                                'nano-modeline-inactive))
            (propertize right 'face (if active 'nano-modeline-active-secondary
                                      'nano-modeline-inactive-secondary)))))


;; ---------------------------------------------------------------------
(defun nano-modeline-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (nano-modeline-compose (if (ein:notebook-modified-p) "**" "RW")
                           buffer-name
                           ""
                           (ein:header-line))))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun nano-modeline-elfeed-search-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

(defun nano-modeline-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

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
    (nano-modeline-compose (nano-modeline-status)
                           (nano-modeline-truncate title 40)
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

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(defun nano-modeline-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

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
  (nano-modeline-compose (nano-modeline-status)
                         "Info"
                         (concat "("
                                 (nano-modeline-info-breadcrumbs)
                                 ")")
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Agenda"
                         ""
                         (format-time-string "%A %-e %B %Y")))

;; ---------------------------------------------------------------------
(defun nano-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun nano-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun nano-modeline-term-mode ()
  (nano-modeline-compose " >_ "
                         "Terminal"
                         (concat "(" shell-file-name ")")
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
  (if (string> mu4e-mu-version "1.6.5")
      mu4e--server-props
    mu4e~server-props))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun nano-modeline-mu4e-dashboard-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format "%d messages" (plist-get (nano-modeline-mu4e-server-props) :doccount))))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun nano-modeline-mu4e-loading-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun nano-modeline-mu4e-main-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-quote (str)
  (if (string> mu4e-mu-version "1.6.5")
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun nano-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun nano-modeline-mu4e-headers-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         (nano-modeline-mu4e-quote (nano-modeline-mu4e-last-query))
                         ""
                         (format "[%s]"
                                 (nano-modeline-mu4e-quote
                                  (mu4e-context-name (mu4e-context-current))))))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun nano-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (nano-modeline-compose (nano-modeline-status)
                           (nano-modeline-truncate subject 40)
                           ""
                           from)))

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
  (nano-modeline-compose (nano-modeline-status)
                         "GNU Emacs / N Λ N O"
                         "(help)"
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun nano-modeline-message-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Message" "(draft)" ""))

;; ---------------------------------------------------------------------
(defvar org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook #'nano-modeline-org-clock-out))

(defun nano-modeline-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun nano-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (branch      (nano-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun nano-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun nano-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
    (mode-name   (nano-modeline-mode-name))
    (branch      (nano-modeline-vc-branch))
    (page-number (concat
              (number-to-string (doc-view-current-page)) "/"
              (or (ignore-errors
                (number-to-string (doc-view-last-page-number)))
              "???"))))
    (nano-modeline-compose
     (nano-modeline-status)
     buffer-name
     (concat "(" mode-name
         (if branch (concat ", "
                (propertize branch 'face 'italic)))
         ")" )
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
    (nano-modeline-compose
     "RW"
     buffer-name
     (concat "(" mode-name
         (if branch (concat ", "
                (propertize branch 'face 'italic)))
         ")" )
     page-number)))

;; ---------------------------------------------------------------------
(defun nano-modeline-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun nano-modeline-buffer-menu-mode ()
    (let ((buffer-name "Buffer list")
          (mode-name   (nano-modeline-mode-name))
          (position    (format-mode-line "%l:%c")))

      (nano-modeline-compose (nano-modeline-status)
                             buffer-name "" position)))
;;(defun buffer-menu-mode-header-line ()
;;  (face-remap-add-relative
;;   'header-line `(:background ,(face-background 'nano-subtle))))
;;(add-hook 'Buffer-menu-mode-hook
;;          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun nano-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (position    (format-mode-line "%l:%c")))

      (nano-modeline-compose (nano-modeline-status)
                             buffer-name "" position)))

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
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (nano-modeline-compose prefix primary filter matches)))

;; ---------------------------------------------------------------------
(defun nano-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun nano-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-modeline-mode-name))
          (branch      (nano-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

;; ---------------------------------------------------------------------
(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))
  

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


(defun nano-modeline-mode--activate ()
  "Activate nano modeline"

  ;; Save current mode-line and header-line
  (unless nano-modeline--saved-mode-line-format
    (setq nano-modeline--saved-mode-line-format mode-line-format)
    (setq nano-modeline--saved-header-line-format header-line-format))

  ;; since the EIN library itself is constantly re-rendering the notebook, and thus
  ;; re-setting the header-line-format, we cannot use the nano-modeline function to set
  ;; the header format in a notebook buffer. Fortunately, EIN exposes the
  ;; ein:header-line-format variable for just this purpose.
  (with-eval-after-load 'ein
    (if (eq nano-modeline-position 'top)
        (setq ein:header-line-format '((:eval (nano-modeline-ein-notebook-mode))))))

  ;; Elfeed uses header-line, we need to tell it to use our own format
  (with-eval-after-load 'elfeed
    (if (eq nano-modeline-position 'top)
        (setq elfeed-search-header-function #'nano-modeline-elfeed-setup-header)))
  
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'nano-modeline-calendar-setup-header))

  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'nano-modeline-org-clock-out))
  
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'nano-modeline-org-capture-turn-off-header-line))

  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil))

  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'nano-modeline))

  (if (eq nano-modeline-position 'top)
      (setq Info-use-header-line nil))

  (if (eq nano-modeline-position 'top)
      (setq Buffer-menu-use-header-line nil))

  ;; Update selected window
  (nano-modeline--update-selected-window)
  ;; (setq nano-modeline--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)
      
  (let* ((format
          '((:eval
             (cond
              ((nano-modeline-user-mode-p)            (funcall ,nano-modeline-user-mode)) 
              ((nano-modeline-prog-mode-p)            (nano-modeline-default-mode))
              ((nano-modeline-message-mode-p)         (nano-modeline-message-mode))
              ((nano-modeline-elfeed-search-mode-p)   (nano-modeline-elfeed-search-mode))
              ((nano-modeline-elfeed-show-mode-p)     (nano-modeline-elfeed-show-mode))
              ((nano-modeline-deft-mode-p)            (nano-modeline-deft-mode))
              ((nano-modeline-info-mode-p)            (nano-modeline-info-mode))
              ((nano-modeline-calendar-mode-p)        (nano-modeline-calendar-mode))
              ((nano-modeline-org-capture-mode-p)     (nano-modeline-org-capture-mode))
              ((nano-modeline-org-agenda-mode-p)      (nano-modeline-org-agenda-mode))
              ((nano-modeline-org-clock-mode-p)       (nano-modeline-org-clock-mode))
              ((nano-modeline-term-mode-p)            (nano-modeline-term-mode))
              ((nano-modeline-vterm-mode-p)           (nano-modeline-term-mode))
              ((nano-modeline-mu4e-dashboard-mode-p)  (nano-modeline-mu4e-dashboard-mode))
              ((nano-modeline-mu4e-main-mode-p)       (nano-modeline-mu4e-main-mode))
              ((nano-modeline-mu4e-loading-mode-p)    (nano-modeline-mu4e-loading-mode))
              ((nano-modeline-mu4e-headers-mode-p)    (nano-modeline-mu4e-headers-mode))
              ((nano-modeline-mu4e-view-mode-p)       (nano-modeline-mu4e-view-mode))
              ((nano-modeline-text-mode-p)            (nano-modeline-default-mode))
              ((nano-modeline-pdf-view-mode-p)        (nano-modeline-pdf-view-mode))
              ((nano-modeline-docview-mode-p)         (nano-modeline-docview-mode))
              ;;  ((nano-modeline-buffer-menu-mode-p)     (nano-modeline-buffer-menu-mode))
              ((nano-modeline-completion-list-mode-p) (nano-modeline-completion-list-mode))
              ((nano-modeline-nano-help-mode-p)       (nano-modeline-nano-help-mode))
              (t                                      (nano-modeline-default-mode)))))))
    
    (if (eq nano-modeline-position 'top)
        (progn 
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))

    ;; This hooks is necessary to register selected window because when
    ;;  a modeline is evaluated, the corresponding window is always selected.
    (add-hook 'post-command-hook #'nano-modeline--update-selected-window)

    (force-mode-line-update t)
  ))


(defun nano-modeline-mode--inactivate ()
  "Inactivate nano mode line and restored default mode-line"
  
  (custom-reevaluate-setting 'Info-use-header-line)
  (custom-reevaluate-setting 'Buffer-menu-use-header-line)
  (custom-reevaluate-setting 'eshell-status-in-mode-line)
    
  (setq ein:header-line-format '(:eval (ein:header-line)))
  (setq elfeed-search-header-function #'elfeed-search--header)
  (remove-hook 'calendar-initial-window-hook
               #'nano-modeline-calendar-setup-header)
  (remove-hook 'org-capture-mode-hook
               #'nano-modeline-org-capture-turn-off-header-line)
  (remove-hook 'org-clock-out-hook
               #'nano-modeline-org-clock-out)
  (remove-hook 'post-command-hook
               #'nano-modeline--update-selected-window)
  (advice-remove 'mu4e~header-line-format #'nano-modeline)

  (setq         mode-line-format nano-modeline--saved-mode-line-format)
  (setq-default mode-line-format nano-modeline--saved-mode-line-format)
  (setq         header-line-format nano-modeline--saved-header-line-format)
  (setq-default header-line-format nano-modeline--saved-header-line-format))


;;;###autoload
(define-minor-mode nano-modeline-mode
  "Toggle nano-modeline minor mode"
  :group 'nano
  :global t
  :init-value nil

  (if nano-modeline-mode
      (nano-modeline-mode--activate)
    (nano-modeline-mode--inactivate))

  ;; Run any registered hooks
  (run-hooks 'nano-modeline-mode-hook))


(provide 'nano-modeline)
;;; nano-modeline.el ends here
