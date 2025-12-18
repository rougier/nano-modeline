;;; nano-modeline.el --- NANO modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 2.0
;; Package-Requires: ((emacs "27.1" mode-line-maker "0.1"))
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
;; Nano modeline is a an alternative to the GNU/Emacs modeline.  It can
;; be displayed at the bottom (mode-line) or at the top (header-line).
;; Everything is configurable via the nano-modeline customization group.
;;
;; Usage example:
;;
;; Use default modeline for the current buffer
;; (nano-modeline 'header)
;;
;; Make the default modeline to be the default for all buffers:
;; (nano-modeline 'header t)
;;
;; Install the modeline for all prog buffers:
;; (add-hook 'prog-mode-hook #'nano-modeline)
;;
;;
;;; NEWS:
;;
;; Version  2.0
;; - Full rewrite for heavy simplification
;; - New dependency on mode-line-maker (same author)
;; - No more dedicated mode nor buttons
;; - Explicit faces for active / inactive modes
;; - Pixel precise alignment of the mode-line/header-line
;;
;; Version  1.1.0
;; - Minor bugfix with org-capture
;; - Better mu4e message mode line
;; - Fixed eat mode line
;; - Better margin/fringe alignment
;; - API change: button now take advantage of new svg-lib API
;; - Fixed flat-button style
;;
;; Version 1.0.1
;; - Minor bugfix
;;
;; Version 1.0.0
;; - Complete rewrite to make it simpler & faster
;; - API break: No longer a minor mode
;; - Activatable buttons can be added and grouped
;; - Modeline can be now be activated through modes hook
;;
;; Version 0.7.2
;; - Fix a bug in info mode (breadcrumbs)
;; - Fix mu header mode for version 1.8
;; - Put back padding (for default style)
;;
;; Version 0.7.1
;; - Fix a bug with mu4e-dashboard
;; - Fix a bug in pdf view mode
;; - Better org-capture mode
;;
;; Version 0.7
;; - Prefix is now an option (none, status or icon)
;; - Prefix can be replaced by icons
;; - Better space computation
;; - New imenu-list mode
;; - Indirect buffers are now handled properly
;; - Bugfix in org-clock-mode
;;
;; Version 0.6
;; - Spaces have face that enforce active/inactive
;; - Better marker for dedicated windows
;; - Internal reordering of modes, most frequent first
;;    (educated guess, might vary greatly with users)
;;
;; Version 0.5.1
;; - Bug fix (make-obsolete-variable)
;; - Added marker for dedicated window
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
(require 'mode-line-maker)

(defgroup nano nil
  "NANO"
  :group 'convenience)

(defgroup nano-modeline nil
  "NANO Modeline"
  :group 'nano)

(defgroup nano-modeline-faces nil
  "NANO modeline faces"
  :group 'nano-modeline)

(defcustom nano-modeline-position 'header
  "Default position for the nano modeline"

  :type '(choice (const :tag "Top"    header)
                 (const :tag "Bottom" footer))
  :group 'nano-modeline)

(defcustom nano-modeline-buffer-status '((read-only  . "RO")
                                         (read-write . "RW")
                                         (modified   . "**")
                                         (narrowed   . "--")
                                         (other      . "##"))
  "Buffer status strings."
 :type '(alist :key-type (choice
                          (const :tag "Read-only"  read-only)
                          (const :tag "Read-write" read-write)
                          (const :tag "Modified"   modified)
                          (const :tag "Narrowed"   narrowed)
                          (const :tag "Other"      other))
               :value-type (string))
  :group 'nano-modeline)

(defcustom nano-modeline-window-status '((root       . "")
                                         (dedicated  . "D"))
  "Window status strings."
 :type '(alist :key-type (choice
                          (const :tag "Root window"       root)
                          (const :tag "Dedicated window"  dedicated))
               :value-type (string))
  :group 'nano-modeline)

(defcustom nano-modeline-padding '(0.20 . 0.25)
  "Default vertical space adjustment (in fraction of character height) for
the buffer status element. This is inserted before and after the status indicator and is only visible in GUI Emacs. This is a purely visual hack."
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'nano-modeline)

(defcustom nano-modeline-active-indicator ?▏
  "Character indicator for the active window."
  :type 'character
  :group 'nano-modeline)

(defface nano-modeline-active-indicator
  `((t ( :foreground ,(face-foreground 'default)
         :background ,(face-background 'mode-line-active nil 'default))))
  "Face for the active indicator."
  :group 'nano-modeline-faces)

(defface nano-modeline-active
  `((t ( :foreground ,(face-foreground 'mode-line-active nil 'default)
         :background ,(face-background 'mode-line-active nil 'default))))
  "Face for active modeline"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive
  `((t ( :foreground ,(face-foreground 'font-lock-comment-face nil 'default)
         :background ,(face-background 'mode-line-active nil 'default))))
  "Face for inactive modeline"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-primary
  `((t ( :foreground ,(face-foreground 'default)
         :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for active primary"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-primary
  `((t ( :foreground ,(face-foreground 'font-lock-comment-face nil 'default)
         :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for inactive primary"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-secondary
  `((t ( :foreground ,(face-foreground 'default))))
  "Face for active secondary information"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-secondary
  `((t ( :foreground ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for inactive secondary information"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-extra
  `((t ( :foreground ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for active extra information"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-extra
  `((t ( :foreground ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for active extra information"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-status-read-only
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'default)
         :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for active read only status"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-status-read-only
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for inactive read only status"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-status-read-write
  `((t ( :foreground ,(face-background 'font-lock-comment-face nil 'default)
         :background ,(face-foreground 'font-lock-comment-face nil 'default)
         :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for read-write status"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-status-read-write
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for inactive read write status"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-status-modified
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'warning nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for modified status"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-status-modified
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for inactive modified status"
  :group 'nano-modeline-faces)

(defface nano-modeline-active-status-other
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'link nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for other status"
  :group 'nano-modeline-faces)

(defface nano-modeline-inactive-status-other
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for inactive other status"
  :group 'nano-modeline-faces)

(defun nano-modeline-is-other (&optional buffer)
  "Return whether BUFFER status is other based on different conditions.

This includes:
- No file associated with buffer
"
  (or (not (buffer-file-name))))

(defun nano-modeline-buffer-status (&optional status)
  "Return a prefix string indicating the current buffer unless a
STATUS string is given."
  
  (let* ((is-other (nano-modeline-is-other))
         (is-narrowed (buffer-narrowed-p))
         (is-read-only buffer-read-only)
         (is-modified (buffer-modified-p)))    
    ;; Order is important to keep the modified status even if read-only
    (or (when (stringp status) status)
        (cond (is-narrowed  (cdr (assoc 'narrowed   nano-modeline-buffer-status)))
              (is-other     (cdr (assoc 'other      nano-modeline-buffer-status)))
              (is-modified  (cdr (assoc 'modified   nano-modeline-buffer-status)))
              (is-read-only (cdr (assoc 'read-only  nano-modeline-buffer-status)))
              (t            (cdr (assoc 'read-write nano-modeline-buffer-status)))))))

(defun nano-modeline-primary ()
  "Return the name of the current buffer."

  (format-mode-line "%b"))

(defun nano-modeline-secondary ()
  "Return the mode and vc information of the current buffer."

  (let* ((file (buffer-file-name))
         (branch (when (and file vc-mode) (substring-no-properties vc-mode 5)))
         (state (when (and file vc-mode) (vc-state file)))
         (mode (downcase (cond ((consp mode-name) (car mode-name))
                               ((stringp mode-name) mode-name)
                               (t "unknown"))))
         (secondary (if (and file branch state)
                      (format "(%s mode, %s [%s])" mode branch state)
                    (format "(%s mode)" mode))))
    secondary))

(defun nano-modeline-extra ()
  "Return cursor line:column information + window status."

  (concat (format-mode-line "%3c:%3l ")
          (nano-modeline-window-status)))

(defun nano-modeline-window-status ()
  "Return window status information."

  (let* ((window (get-buffer-window))
         (is-dedicated (window-dedicated-p window))
         (is-root (frame-root-window-p window))
         (info (concat
                (if is-root
                    (cdr (assoc 'root nano-modeline-window-status))
                  "")
                (if is-dedicated
                    (cdr (assoc 'dedicated nano-modeline-window-status))
                  ""))))
    (if (> (length info) 0)
        (format " [%s]" info)
      "")))

(defun nano-modeline-empty ()
  "Return empty string"

  "")

(defun nano-modeline-progress (progress &optional width)
  "Return a PROGRESS bar of WIDTH characters"
  (let* ((width (or width 12))
         (progress (min (max progress 0.0) 1.0))
         (completed (floor (* progress width)))
         (uncompleted (- width completed)))
    (concat (make-string completed ?█)
            (make-string uncompleted ?░)
            (format " %d%%" (floor (* progress 100))))))

(defun nano-modeline-string (string)
  "Return string"

  string)

(defun nano-modeline--status (text)
  "Propertize TEXT for the modeline (status face)."
  
  (let* ((window-active (mode-line-window-selected-p))
         (padding nano-modeline-padding)
         (is-other (nano-modeline-is-other))
         (is-read-only buffer-read-only)
         (is-modified (buffer-modified-p))
         ;; Order is important: other prevails over other status
         ;; Then read-only such that a read-only but modified buffer is visible
         (face (if window-active
                   (cond (is-other     'nano-modeline-active-status-other)
                         (is-read-only 'nano-modeline-active-status-read-only)
                         (is-modified  'nano-modeline-active-status-modified)
                         (t            'nano-modeline-active-status-read-write))
                 (cond (is-other       'nano-modeline-inactive-status-other)
                       (is-read-only   'nano-modeline-inactive-status-read-only)
                       (is-modified    'nano-modeline-inactive-status-modified)
                       (t              'nano-modeline-inactive-status-read-write))))
         (face-indicator
          `( :foreground ,(face-foreground 'nano-modeline-active-indicator nil 'default)
             :background ,(face-background face nil 'default)))
         (left (if window-active
                   (propertize (format "%c" nano-modeline-active-indicator)
                               'face face-indicator
                               'display `(raise ,(car padding)))
                 (propertize " "
                             'face face
                             'display `(raise ,(car padding)))))
         (right  (propertize " "
                             'face face
                             'display `(raise ,(- (cdr padding))))))
    (concat left (propertize text 'face face) right)))


(defun nano-modeline--primary (text)
"Propertize TEXT for the modeline (primary face)."
  (let* ((window-active (mode-line-window-selected-p)))
    (if window-active
        (propertize text 'face 'nano-modeline-active-primary)
      (propertize text 'face 'nano-modeline-inactive-primary))))

(defun nano-modeline--secondary (text)
"Propertize TEXT for the modeline (secondary face)."
  (let* ((window-active (mode-line-window-selected-p)))
    (if window-active
        (propertize text 'face 'nano-modeline-active-secondary)
      (propertize text 'face 'nano-modeline-inactive-secondary))))

(defun nano-modeline--extra (text)
"Propertize TEXT for the modeline (extra face)."
  (let* ((window-active (mode-line-window-selected-p)))
    (if window-active
        (propertize text 'face 'nano-modeline-active-extra)
      (propertize text 'face 'nano-modeline-inactive-extra))))


;; --- MU4E -------------------------------------------------------------------
(defun nano-modeline-mu4e-last-query ()
  "MU4E: Last search query"
  (mu4e-last-query))

(defun nano-modeline-mu4e-last-query-count ()
  "MU4E: Last search query count (parsing messages buffer)."

  (with-current-buffer "*Messages*"
    (save-excursion
      (goto-char (point-max))
      (if (re-search-backward
           "Found \\([0-9]+\\) matching message[s]?; \\([0-9]+\\) hidden[s]?" nil t)
          (format "%s messages " (match-string 1))
        ""))))

(defun nano-modeline-mu4e-view-tags ()
  "MU4E: Message at point tags"
  (let* ((msg (mu4e-message-at-point))
         (tags (mu4e-message-field msg :tags)))
      (mapconcat #'identity tags ",")))

(defun nano-modeline-mu4e-view-from ()
  "MU4E: Message at point sender"
  (let* ((msg (mu4e-message-at-point))
         (me (mapcar #'downcase (mu4e-personal-addresses)))
         (from (mu4e-message-field msg :from))
         (from-name (plist-get (car from) :name))
         (from-email (plist-get (car from) :email)))
    (cond ((member from-email me) "Me")
           ((stringp from-name)   (capitalize (downcase from-name)))
           (t                     from-email))))

(defun nano-modeline-mu4e-view-subject ()
  "MU4E: Message at point subject"
  (let* ((msg (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject)))
    (format "%s" subject)))

(defun nano-modeline-mu4e-compose-subject ()
  "MU4E: Compose subject (live)"
  (if (derived-mode-p '(mu4e-compose-mode))
      (save-excursion
        (message-position-on-field "Subject")
        (message-beginning-of-line)
        (if (eq (point) (line-beginning-position))
            "(no subject)"
          (buffer-substring (point) (line-end-position))))
    ""))

(defun nano-modeline-mu4e-view-date ()
  "MU4E: Message at point date"
  (let* ((msg (mu4e-message-at-point))
         (date (mu4e-message-field msg :date)))
    (format-time-string "%d %b %Y at %H:%M " date)))

(defun nano-modeline-mu4e-context ()
  "MU4E: Current context name."
  (if-let* ((context (mu4e-context-current))
            (name (mu4e-context-name context)))
      (upcase name)
    "NONE"))

(defun nano-modeline-mu4e-update-mbsync ()
  "Return the most relevant mbsync value from the '*mu4e-update*' buffer.

Priority:
1. If a `Channels: N` summary exists, return `C: N/N`.
2. Else return the most recent `C: a/b` line.
3. Else return --/--."

  (if-let ((buffer (get-buffer " *mu4e-update*")))
      (let ((total)
            (count))
        (with-current-buffer buffer
          (save-excursion
          (goto-char (point-max))
          (when (re-search-backward "Channels: *\\([0-9]+\\)" nil t)
            (setq total (match-string 1)))
          (goto-char (point-max))
          (when (re-search-backward "\\bC: *\\([0-9]+/[0-9]+\\)" nil t)
            (setq count (match-string 1)))))
      (cond (total (format "%s/%s" total total))
            (count (format "%s" count))
            (t "--/--")))
    "--/--"))

(defun nano-modeline-mu4e--update-hook (where)
  (when (get-buffer " *mu4e-update*")
    (with-current-buffer (get-buffer " *mu4e-update*")
      (nano-modeline where nil
                     (lambda () (nano-modeline-buffer-status "MU4E"))
                     (lambda () (nano-modeline-string "Update"))
                     (lambda () (nano-modeline-string (format "(%s)" mu4e-get-mail-command)))
                     #'nano-modeline-mu4e-update-mbsync))
    (remove-hook 'buffer-list-update-hook
                 #'nano-modeline-mu4e--update-hook)))

(defun nano-modeline-mu4e-update (&optional where)
  "MU4E: update mode
  
  This installs a hook on buffer list update in order to detect the
creation of `*mu4e-update*' buffer. This works only with mbsync update
process."
  
  (add-hook 'buffer-list-update-hook
            (lambda () (nano-modeline-mu4e--update-hook where))))

;;;### autoload
(defun nano-modeline-mu4e-view (&optional where)
  "MU4E: view mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "FROM"))
                 #'nano-modeline-mu4e-view-from
                 #'nano-modeline-mu4e-view-subject
                 #'nano-modeline-mu4e-view-date))

;;;### autoload
(defun nano-modeline-mu4e-compose (&optional where)
  "MU4E: compose mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status
                             (nano-modeline-mu4e-context)))
                 #'nano-modeline-mu4e-compose-subject
                 #'nano-modeline-empty))

;;;### autoload
(defun nano-modeline-mu4e-headers (&optional where)
  "MU4E: headers mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "SEARCH"))
                 #'nano-modeline-mu4e-last-query
                 #'nano-modeline-empty
                 #'nano-modeline-mu4e-last-query-count))
;; --- MU4E -------------------------------------------------------------------

;; --- ELFEED -----------------------------------------------------------------
(defun nano-modeline-elfeed-entry-title ()
  "ELFEED: entry feed"

  (if-let* ((buffer (get-buffer "*elfeed-entry*"))
              (entry (with-current-buffer buffer elfeed-show-entry)))
    (or (elfeed-meta  entry :title)
        (elfeed-entry-title entry))
    ""))

(defun nano-modeline-elfeed-entry-feed ()
  "ELFEED: entry feed"

  (if-let* ((buffer (get-buffer "*elfeed-entry*"))
            (entry (with-current-buffer buffer elfeed-show-entry))
            (feed (elfeed-entry-feed entry)))
      (format "%s " (or (elfeed-meta feed :title)
                        (elfeed-feed-title feed)))
    ""))

(defun nano-modeline-elfeed-search-filter ()
  "ELFEED: search filter"

  elfeed-search-filter)
  
(defun nano-modeline-elfeed-search-count ()
  "ELFEED: entries count filter"

  (if (and (not (zerop (elfeed-db-last-update)))
           (> (elfeed-queue-count-total) 0))
      (let ((total (elfeed-queue-count-total))
            (in-process (elfeed-queue-count-active)))
        (format "(%d jobs pending, %d active)"  (- total in-process) in-process))
    (cond (elfeed-search-filter-active "")
          ((string-match-p "[^ ]" elfeed-search-filter) elfeed-search-filter)
          (t ""))
    (with-current-buffer "*elfeed-search*"
      (cond ((zerop (elfeed-db-last-update)) "")
            ((> (elfeed-queue-count-total) 0) "")
            (t  (if (and elfeed-search-filter-active elfeed-search-filter-overflowing)
                    "(?/?)"
                  (cl-loop with feeds = (make-hash-table :test 'equal)
                           for entry in elfeed-search-entries
                           for feed = (elfeed-entry-feed entry)
                           for url = (elfeed-feed-url feed)
                           count entry into entry-count
                           count (elfeed-tagged-p 'unread entry) into unread-count
                           do (puthash url t feeds)
                           finally
                           (cl-return
                            (format "(%s/%s)" (+ 1 unread-count) entry-count)))))))))

(defun nano-modeline-element-elfeed-search-stats ()
  "ELFEED: search statistics"
  (or (cond ((zerop (elfeed-db-last-update)) " ")
            ((> (elfeed-queue-count-total) 0) " ")
            (t (elfeed-search--count-unread))) " "))

(defun nano-modeline-elfeed-last-update ()
  "ELFEED: Last update time."
  (format-time-string "%Y-%m-%d %H:%M " (elfeed-db-last-update)))

;;;### autoload
(defun nano-modeline-elfeed-search (&optional where)
  "ELFEED: search mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "SEARCH"))
                 #'nano-modeline-elfeed-search-filter
                 #'nano-modeline-elfeed-search-count
                 #'nano-modeline-elfeed-last-update))

;;;### autoload
(defun nano-modeline-elfeed-show (&optional where)
  "ELFEED: search mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "ENTRY"))
                 #'nano-modeline-elfeed-entry-title
                 #'nano-modeline-empty
                 #'nano-modeline-elfeed-entry-feed))
;; --- ELFEED -----------------------------------------------------------------

;; --- ELPHER -----------------------------------------------------------------
(defun nano-modeline-elpher-title ()
  "ELPHER: Page title"
  (if-let* ((buffer (get-buffer elpher-buffer-name)))
      (with-current-buffer buffer
        (elpher-page-display-string elpher-current-page))
    ""))

(defun nano-modeline-elpher-url ()
  "ELPHER: Page URL"
  (if-let* ((buffer (get-buffer elpher-buffer-name)))
    (with-current-buffer buffer
      (let* ((address (elpher-page-address elpher-current-page)))
        (format "(%s)" (elpher-address-to-url address))))
    ""))

(defun nano-modeline-elpher-tls ()
  "ELPHER: TLS encryption status"
  (if-let* ((buffer (get-buffer elpher-buffer-name)))
    (with-current-buffer buffer
      (let* ((address (elpher-page-address elpher-current-page)))
        (if (and (not (elpher-address-about-p address))
                 (member (elpher-address-protocol address)
                         '("gophers" "gemini")))
            "(TLS encryption)"
          "")))))

;;;### autoload
(defun nano-modeline-elpher (&optional where)
  "ELPHER: elpher mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "GEM"))
                 #'nano-modeline-elpher-title
                 #'nano-modeline-elpher-tls
                 #'nano-modeline-empty))
;; --- ELPHER -----------------------------------------------------------------

;; --- AGENDA -----------------------------------------------------------------
(defun nano-modeline-org-agenda-name ()
  "ORG-AGENDA: name"

  (with-current-buffer org-agenda-buffer
    org-agenda-name))

(defun nano-modeline-org-agenda-span ()
  "ORG-AGENDA: span"

  (with-current-buffer org-agenda-buffer
    (save-excursion
      (goto-char (point-min))
      (format "%s "
              (substring-no-properties
               (buffer-substring (point-min) (1- (line-end-position))))))))

;;;### autoload
(defun nano-modeline-org-agenda (&optional where)
  "ORG-AGENDA: org-agenda mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "AGENDA"))
                 #'nano-modeline-org-agenda-name
                 #'nano-modeline-empty
                 #'nano-modeline-org-agenda-span))
;; --- AGENDA -----------------------------------------------------------------

;; --- CALENDAR ---------------------------------------------------------------
(defun nano-modeline-calendar-date (&optional format date)
  "CALENDAR: date"

  (with-current-buffer calendar-buffer
    (if-let* ((date (or date (calendar-cursor-to-date)))
              (date (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
              (format (or format "%A %d %B %Y")))
        (format-time-string format date)
      "")))

(defun nano-modeline-calendar-holidays (&optional date)
  "CALENDAR: holiday"

  (with-current-buffer calendar-buffer
    (if-let* ((date (or date (calendar-cursor-to-date))))
        (let* ((holidays (car (calendar-check-holidays date)))
               (today (format-time-string "%d %B %Y"))
               (date (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
               (date (format-time-string "%d %B %Y" date)))
          (cond (holidays (format "(%s)" holidays))
                ((string= date today) "(Today)")
                (t "")))
      "")))

(defun nano-modeline-calendar (&optional where)
  "CALENDAR: calendar mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "CALENDAR"))
                 #'nano-modeline-calendar-date
                 #'nano-modeline-calendar-holidays
                 #'nano-modeline-empty))
;; --- CALENDAR ---------------------------------------------------------------

;; --- NANO-CALENDAR ---------------------------------------------------------------
(defun nano-modeline-nano-calendar-date (&optional format)
  "NANO-CALENDAR: date"

  (with-current-buffer nano-calendar-buffer
    (if-let* ((date (nano-calendar-cursor-date)))
        (nano-modeline-calendar-date format date)
      "")))
              
(defun nano-modeline-nano-calendar-holidays ()
  "NANO-CALENDAR: holiday"

  (with-current-buffer nano-calendar-buffer
    (if-let* ((date (nano-calendar-cursor-date)))
        (nano-modeline-calendar-holidays date)
      "")))

(defun nano-modeline-nano-calendar-workload ()
  "NANO-CALENDAR: workload"

  (with-current-buffer nano-calendar-buffer
    (if-let* ((workload (nano-calendar-cursor-workload)))
        (cond ((eq workload 0) "No event")
              ((eq workload 0) "1 event")
              (t              (format "%s events " workload)))
      "")))

(defun nano-modeline-nano-calendar (&optional where)
  "NANO-CALENDAR: calendar mode"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "CALENDAR"))
                 #'nano-modeline-nano-calendar-date
                 #'nano-modeline-nano-calendar-holidays
                 #'nano-modeline-nano-calendar-workload))
;; --- NANO-CALENDAR ---------------------------------------------------------------

;; --- TERMINAL ---------------------------------------------------------------
(defun nano-modeline-terminal-directory (&optional max-length)
  "TERMINAL: Current working directory"

  (let* ((max-length (or max-length 32))
         (dir default-directory)
         (path (reverse (split-string (abbreviate-file-name dir) "/")))
         (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 0)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    (format "%s " output)))

(defun nano-modeline-terminal-mode ()
  "TERMINAL: mode"
  (if-let ((mode (cond ((derived-mode-p '(term-mode))
                        (cond ((term-in-char-mode) "char")
                              ((term-in-line-mode) "line")
                              (t                   "????")))
                       ((derived-mode-p '(eat-mode))
                        (cond (eat--semi-char-mode "semi-char")
                              (eat--char-mode "char")
                              (eat--line-mode "line")
                              (t               "????")))
                       (t "????"))))
      (format "(%s mode)" mode)
    ""))

(defun nano-modeline-terminal-shell ()
  "TERMINAL: shell name"
  (format "%s" shell-file-name))

(defun nano-modeline-terminal (&optional where)
  "TERM: term mode (including eat)"

  (interactive)
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status ">_"))
                 #'nano-modeline-terminal-shell
                 #'nano-modeline-terminal-mode
                 #'nano-modeline-terminal-directory))
;; --- TERMINAL ---------------------------------------------------------------

;; --- DIRED ------------------------------------------------------------------
(defun nano-modeline-dired-deleted-count ()
  "DIRED: marked files for deletion."
  (save-excursion
    (goto-char (point-min))
    (count-matches "^[[:blank:]]*D")))

(defun nano-modeline-dired-marked-count ()
  "DIRED: marked files."
  (save-excursion
    (goto-char (point-min))
    (count-matches "^[[:blank:]]*\\*")))

(defun nano-modeline-dired-secondary ()
  "DIRED: secondary information"
  (let ((marked  (nano-modeline-dired-marked-count))
        (deleted (nano-modeline-dired-deleted-count)))
    (cond ((and (> marked 0) (> deleted 0))
           (format "(%d files marked, %d files marked for deletion)" marked deleted))
           ((> marked 0)
            (format "(%d files marked)" marked))
           ((> deleted 0)
            (format "(%d files marked for deletion)" deleted))
           (t (nano-modeline-secondary)))))

(defun nano-modeline-dired-filename ()
  "DIRED: filename"
  (if-let ((filename (dired-get-filename nil t)))
      (file-name-nondirectory filename)
    ""))

(defun nano-modeline-dired (&optional where)
  "DIRED: dired mode"

  (interactive)
  (add-hook 'post-command-hook #'force-mode-line-update nil t)  
  (nano-modeline where nil
                 (lambda () (nano-modeline-buffer-status "DIRED"))
                 #'nano-modeline-primary
                 #'nano-modeline-dired-secondary
                 #'nano-modeline-dired-filename))
;; --- DIRED ------------------------------------------------------------------
  
(defun nano-modeline (&optional where default status primary secondary extra)
  "Install a modeline WHERE specified ('header or 'footer) and make it the
DEFAULT if specified.

The resulting line is made of two parts, left and right.
- Left is the concatenation of: STATUS space PRIMARY space SECONDARY
- Right is the concatenation of: EXTRA space.

STATUS    defaults to 'nano-modeline-buffer-status'
PRIMARY   defaults to 'nano-modeline-primary'
SECONDARY defaults to 'nano-modeline-secondary'
EXTRA     defaults to 'nano-modeline-extra'."

  (interactive)
  (let* ((where (or where nano-modeline-position))
         (status (or status #'nano-modeline-buffer-status))
         (primary (or primary #'nano-modeline-primary))
         (secondary (or secondary #'nano-modeline-secondary))
         (extra (or extra #'nano-modeline-extra))
         (left  `((:eval (nano-modeline--status (,status))) " "
                  (:eval (nano-modeline--primary (,primary))) " "
                  (:eval (nano-modeline--secondary (,secondary)))))
         (right `((:eval (nano-modeline--extra (,extra))) "")))
    (when (eq where 'footer)
      (setq mode-line-format (mode-line-maker left right))
      (if default
          (setq-default mode-line-format (mode-line-maker left right))))
    (when (eq where 'header)
        (setq header-line-format (mode-line-maker left right))
        (if default
            (setq-default header-line-format (mode-line-maker left right))))))

(provide 'nano-modeline)

;;; nano-modeline.el ends here

(add-hook 'mu4e-headers-mode-hook  #'nano-modeline-mu4e-headers)
(add-hook 'mu4e-view-mode-hook     #'nano-modeline-mu4e-view)
(add-hook 'mu4e-compose-mode-hook  #'nano-modeline-mu4e-compose)
(add-hook 'mu4e-update-pre-hook    #'nano-modeline-mu4e-update)
(add-hook 'elfeed-search-mode-hook #'nano-modeline-elfeed-search)
(add-hook 'elfeed-show-mode-hook   #'nano-modeline-elfeed-show)
(add-hook 'term-mode-hook          #'nano-modeline-terminal)
(add-hook 'eat-mode-hook           #'nano-modeline-terminal)
(add-hook 'calendar-mode-hook      #'nano-modeline-calendar)
(add-hook 'org-agenda-mode-hook    #'nano-modeline-org-agenda)
(add-hook 'dired-mode-hook         #'nano-modeline-dired)
