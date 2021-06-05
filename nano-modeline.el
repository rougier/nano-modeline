;;; nano-modeline.el --- N Λ N O Modeline -*- lexical-binding: t -*-
;; -------------------------------------------------------------------
;; GNU Emacs / N Λ N O Modeline
;; Copyright (C) 2020-2021 - N Λ N O developers 
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;
;; Modeline is rendered as:
;; [ status | name (primary)                               secondary ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)

(defgroup nano nil
  "N Λ N O")

(defgroup nano-modeline nil
  "Modeline settings"
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

(defvar nano-modeline--selected-window nil
  "Currently selected window")

(defcustom nano-modeline-position 'top
  "Default position (top or bottom)"
  :type '(choice (const :tag "Top"    top)
                 (const :tag "Bottom" bottom))
  :group 'nano-modeline)

(defface nano-modeline-active
  '((t (:inherit nano-subtle)))
  "Modeline face for active modeline"
  :group 'nano-modeline-active)

(defface nano-modeline-active-name
  '((t (:inherit (nano-strong nano-modeline-active))))
  "Modeline face for active name element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-primary
  '((t (:inherit (nano-default nano-modeline-active))))
  "Modeline face for active primary element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-secondary
  '((t (:inherit (nano-faded nano-modeline-active))))
  "Modeline face for active secondary element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-RO
  '((t (:inherit nano-popout-i)))
  "Modeline face for active READ-ONLY element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-RW
  '((t (:inherit nano-faded-i)))
  "Modeline face for active READ-WRITE element"
  :group 'nano-modeline-active)

(defface nano-modeline-active-status-**
  '((t (:inherit nano-critical)))
  "Modeline face for active MODIFIED element"
  :group 'nano-modeline-active)

(defface nano-modeline-inactive
  '((t (:inherit nano-subtle)))
  "Modeline face for inactive window"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-name
  '((t (:inherit (nano-faded  nano-modeline-inactive))))
  "Modeline face for inactive name element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-primary
  '((t (:inherit (nano-faded nano-modeline-inactive))))
  "Modeline face for inactive primary element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-secondary
  '((t (:inherit (nano-faded nano-modeline-inactive))))
  "Modeline face for inactive primary element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-RO
  '((t (:inherit (nano-popout nano-modeline-inactive))))
  "Modeline face for inactive READ-ONLY element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-RW
  '((t (:inherit (nano-faded nano-modeline-inactive))))
  "Modeline face for inactive READ-WRITE element"
  :group 'nano-modeline-inactive)

(defface nano-modeline-inactive-status-**
  '((t (:inherit (nano-critical-i nano-modeline-inactive))))
  "Modeline face for inactive MODIFIED element"
  :group 'nano-modeline-inactive)

(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun nano-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun shorten-directory (dir max-length)
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
			(propertize (if (window-dedicated-p)" -- " " RO ")
				    'face (if active 'nano-modeline-active-status-RO
					             'nano-modeline-inactive-status-RO)))
                       ((string= status "**")
			(propertize (if (window-dedicated-p)" -- " " ** ")
				    'face (if active 'nano-modeline-active-status-**
					             'nano-modeline-inactive-status-**)))
                       ((string= status "RW")
			(propertize (if (window-dedicated-p)" -- " " RW ")
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
(defun nano-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun nano-modeline-mu4e-dashboard-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format "%d messages" (plist-get mu4e~server-props :doccount))))

;; ---------------------------------------------------------------------
;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the nano-modeline function to set
;; the header format in a notebook buffer.  Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.
(with-eval-after-load 'ein
  (defun nano-modeline-ein-notebook-mode ()
    (let ((buffer-name (format-mode-line "%b")))
      (nano-modeline-compose (if (ein:notebook-modified-p) "**" "RW")
                             buffer-name
                             ""
                             (ein:header-line))))
  (setq ein:header-line-format '((:eval (nano-modeline-ein-notebook-mode)))))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun nano-modeline-elfeed-search-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

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
                           (s-truncate 40 title "…")
                           (concat "(" tags-str ")")
                           feed-title)))

;; ---------------------------------------------------------------------
(defun nano-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun nano-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Capture"
                         "(org)"
                         ""))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
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
                         (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun nano-modeline-mu4e-main-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun nano-modeline-mu4e-headers-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         ""
                         ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (nano-modeline)))

;; ---------------------------------------------------------------------
(setq mu4e-modeline-max-width 72)

(defun nano-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun nano-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (nano-modeline-compose (nano-modeline-status)
                           (s-truncate 40 subject "…")
                           ""
                           from)))

(defun nano-modeline-mu4e-view-hook ()
  (setq header-line-format "%-")
  (face-remap-add-relative 'header-line
                           '(:background "#ffffff"
                                         :underline nil
                                         :box nil
                                         :height 1.0)))
(add-hook 'mu4e-view-mode-hook #'nano-modeline-mu4e-view-hook)


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
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            '(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun nano-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
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
	(mode-name   (nano-mode-name))
	(branch      (vc-branch))
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
	(mode-name   (nano-mode-name))
	(branch      (vc-branch))
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
(defun buffer-menu-mode-header-line ()
  (face-remap-add-relative
   'header-line `(:background ,(face-background 'nano-subtle))))
(add-hook 'Buffer-menu-mode-hook
          #'buffer-menu-mode-header-line)

;; ---------------------------------------------------------------------
(defun nano-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun nano-modeline-completion-list-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (position    (format-mode-line "%l:%c")))

      (nano-modeline-compose (nano-modeline-status)
                             buffer-name "" position)))
;; ---------------------------------------------------------------------
(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun nano-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun nano-modeline-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Notes")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (nano-modeline-compose " DEFT "
                           primary filter matches)))
    

;; ---------------------------------------------------------------------
(defun nano-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun nano-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (nano-mode-name))
          (branch      (vc-branch))
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
(defun nano-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))


;; ---------------------------------------------------------------------
(defun nano-modeline-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))

(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)

  ;; Update selected window
  (setq nano-modeline--selected-window (selected-window))
  
  ;; "Box" effect is obtained through display property
  (nano-modeline-face-clear 'mode-line)
  (nano-modeline-face-clear 'mode-line-inactive)
  (nano-modeline-face-clear 'header-line)
  (setq eshell-status-in-modeline nil)
  
        ;; TTY mode top
  (cond ((and (not (display-graphic-p)) (eq nano-modeline-position 'top))
	 (setq mode-line-format nil)
	 (setq-default mode-line-format nil)
	 (set-face-attribute 'mode-line nil :inherit 'nano-modeline-active)
	 (set-face-attribute 'mode-line-inactive nil :inherit 'nano-modeline-inactive))

	;; TTY Mode bottom
	((and (not (display-graphic-p)) (eq nano-modeline-position 'top))
	 (setq header-line-format nil)
	 (setq-default header-line-format nil))
	
	;; Graphic mode, modeline at top
	((eq nano-modeline-position 'top)
	 (setq mode-line-format (list ""))
	 (setq-default mode-line-format (list ""))
	 (set-face-attribute 'mode-line nil :inherit 'nano-modeline-active
			                    :height 0.1)
	 (set-face-attribute 'mode-line-inactive nil :inherit 'nano-modeline-inactive
			                              :height 0.1))
	 
	;; Graphic mode, modeline at bottom
	(t
	 (setq header-line-format nil)
	 (setq-default header-line-format nil)
	 (set-face-attribute 'mode-line nil          :height (face-attribute 'default :height))
	 (set-face-attribute 'mode-line-inactive nil :height (face-attribute 'default :height))))
	 

  (let ((format
     '((:eval
	(cond
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
	 ((nano-modeline-mu4e-headers-mode-p)    (nano-modeline-mu4e-headers-mode))
	 ;; ((nano-modeline-mu4e-view-mode-p)       (nano-modeline-mu4e-view-mode))
	 ((nano-modeline-text-mode-p)            (nano-modeline-default-mode))
	 ((nano-modeline-pdf-view-mode-p)        (nano-modeline-pdf-view-mode))
	 ((nano-modeline-docview-mode-p)         (nano-modeline-docview-mode))
	 ((nano-modeline-completion-list-mode-p) (nano-modeline-completion-list-mode))
	 ((nano-modeline-nano-help-mode-p)       (nano-modeline-nano-help-mode))
	 (t                                      (nano-modeline-default-mode)))))))
    (if (eq nano-modeline-position 'top)
	(setq-default header-line-format format)
      (setq-default mode-line-format format))))



;; This hooks is necessary to register selected window because when
;;  a modeline is evaluated, the corresponding window is always selected.
(add-hook 'post-command-hook
	  (lambda () (setq nano-modeline--selected-window (selected-window))))

(provide 'nano-modeline)
