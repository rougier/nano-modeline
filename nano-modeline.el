;;; nano-modeline.el --- N Λ N O modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 0.7.2
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
;; Nano modeline is a an alterntive to the GNU/Emacs modeline. It can
;; be displayed at the bottom (mode-line) or at the top (header-line)
;; depending on the nano-modeline-position custom setting. There are
;; several modelines that can be installed on a per-mode basis or as
;; the default one.
;;
;; Usage example:
;;
;; Install prog mode modeline:
;; (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
;;
;; Make text mode modeline the default:
;; (nano-modeline-text-mode t)
;;
;;
;;; NEWS:
;;
;; Version 1.0.0
;; - Complete rewrite to make it simpler & faster
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

(defgroup nano nil
  "N Λ N O"
  :group 'convenience)

(defgroup nano-modeline nil
  "N Λ N O Modeline"
  :group 'nano)

(defcustom nano-modeline-padding '(0.20 . 0.25)
  "Default vertical space adjustment (in fraction of character height)"
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'nano-modeline)

(defcustom nano-modeline-position #'nano-modeline-header
  "Default position for the nano modeline"
  
  :type '(choice (const :tag "Top"    nano-modeline-header)
                 (const :tag "Bottom" nano-modeline-footer))
  :group 'nano-modeline)

(defface nano-modeline-active
  `((t (:foreground ,(face-foreground 'default)
        :background ,(face-background 'header-line nil t)
        :box (:line-width 1 :color ,(face-background 'default)))))
    "Face for when line is active")

(defface nano-modeline-inactive
  `((t (:inherit (,(when (facep 'nano-faded) 'nano-faded)
                  nano-modeline-active))))
  "Face for when line is inactive")

(defface nano-modeline-status
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'shadow nil t)
        :inherit bold)))
  "Face for line status")

;; Nano line faces
(defcustom nano-modeline-faces
  `((header-active      . (nano-modeline-active))
    (header-inactive    . (nano-modeline-inactive))
    (footer-active      . (nano-modeline-active))
    (footer-inactive    . (nano-modeline-inactive))
    (status-RW-active   . (nano-modeline-status))
    (status-RO-active   . (nano-modeline-status))
    (status-**-active   . (nano-modeline-status
                           ,(when (facep 'nano-popout-i) 'nano-popout-i)))
    (bold-active        . (bold))
    (faded-active       . (,(when (facep 'nano-faded) 'nano-faded))))
  "Nano line faces.

Each face defined here is used by the modeline depending on the current state (active / inactive). It is ok to define a face for a single state. In such case, the alternative state will use defaults."
  :type '(alist :key-type (symbol :tag "Face")
                :value-type (repeat :tag "inherits" face)))

(defface nano-modeline--empty-face
  `((t (:foreground ,(face-foreground 'default))))
  "Empty face for resetting mode-line / header-line."
  :group nil)

(defvar nano-modeline--selected-window nil
  "Selected window before mode-line was activated.")

(defun nano-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq nano-modeline--selected-window (selected-window)))

(defun nano-modeline--base-face (face-prefix)
  "Return the face for FACE-PREFIX according to current active state."

  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window nano-modeline--selected-window))
         (state (intern (concat (symbol-name face-prefix)
                                (if active "-active" "-inactive"))))
         (face (cdr (assoc state nano-modeline-faces))))
    face))

(defun nano-modeline-face (&optional face-prefix)
  "Return the face for FACE-PREFIX according to current active state and
make it inherit the base face."

  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window nano-modeline--selected-window))
         (state (intern (concat (symbol-name face-prefix)
                                (if active "-active" "-inactive"))))
         (face (cdr (assoc state nano-modeline-faces)))
         (face (if (boundp 'nano-modeline-base-face)
                           (push nano-modeline-base-face face)
                   face))
         (face (reverse face)))
    `(:inherit ,face)))

(defun nano-modeline--make (left right face-prefix)
  "Build a dynamic mode/header line made of LEFT and RIGHT part,
using the given FACE-PREFIX as the default."
  
  `(:eval
    (let* ((nano-modeline-base-face (nano-modeline--base-face ',face-prefix))
           (left (mapconcat
                  (lambda (element)
                    (if (stringp element)
                        (propertize element 'face nano-modeline-base-face)
                      (apply (car element) (cdr element))))
                  ',left))
           (right (mapconcat
                   (lambda (element)
                    (if (stringp element)
                        (propertize element 'face nano-modeline-base-face)
                     (apply (car element) (cdr element))))
                   ',right))
           (width (window-width))
           (left-max-size (- width (length right) 2))
           (left (if (> (length left) left-max-size)
                     (concat (truncate-string-to-width left left-max-size)
                             (propertize "…" 'face `(:inherit  ,nano-modeline-base-face)))
                   left)))
      (concat (propertize " "
                'display `(space :align-to (+ left-margin
                                              (0.0 . left-fringe)
                                              (1.0 . left-margin))))
              left
              (propertize " "
                'face `(:inherit ,nano-modeline-base-face)
                'display `(space :align-to (- right
                                              (-1.0 . right-fringe)
                                              ,(length right))))
              right))))

(defun nano-modeline-header (left &optional right default)
  "Install a header line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (if default
      (setq-default header-line-format (nano-modeline--make left right 'header))
    (setq-local header-line-format (nano-modeline--make left right 'header)))
  (face-remap-set-base 'header-line 'nano-modeline--empty-face)
  (add-hook 'post-command-hook #'nano-modeline--update-selected-window))

(defun nano-modeline-footer (left &optional right default)
  "Install a footer line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (if default
      (setq-default mode-line-format (nano-modeline--make left right 'header))
    (setq-local mode-line-format (nano-modeline--make left right 'header)))
  (face-remap-set-base 'mode-line 'nano-modeline--empty-face)
  (face-remap-set-base 'mode-line-inactive 'nano-modeline-empty-face)
  (add-hook 'post-command-hook #'nano-modeline--update-selected-window))

(defun nano-modeline-buffer-name (&optional name)
  "Buffer name"
  
  (propertize
   (cond (name                name)
         ((buffer-narrowed-p) (format"%s [narrow]" (buffer-name)))
         (t                   (buffer-name)))
   'face (nano-modeline-face 'bold)))

(defun nano-modeline-buffer-status (&optional status padding)
  "Generic prefix to indicate buffer STATUS with vertical PADDING (top . bottom)"

  (let* ((padding (or padding nano-modeline-padding))
         (top (propertize " " 'display `(raise ,(car padding))))
         (bot (propertize " " 'display `(raise ,(- (cdr padding))))))
    (cond (buffer-read-only
           (propertize (concat top (or status "RO") bot)
                       'face (nano-modeline-face 'status-RO)))
          ((buffer-modified-p)
           (propertize (concat top (or status "**") bot)
                       'face (nano-modeline-face 'status-**)))
          (t
           (propertize (concat top (or status "RW") bot)
                       'face (nano-modeline-face 'status-RW))))))

(defun nano-modeline-file-size ()
  "File size in human readable format"

  (if-let* ((file-name (buffer-file-name))
            (file-attributes (file-attributes file-name))
            (file-size (file-attribute-size file-attributes))
            (file-size (file-size-human-readable file-size)))
      (propertize (format "(%s)" file-size)
                  'face (nano-modeline-face))
    ""))

(defun nano-modeline-cursor-position (&optional format)
  "Cursor position using given FORMAT."

  (let ((format (or format "%l:%c ")))
    (propertize (format-mode-line format)
                'face (nano-modeline-face 'faded))))

(defun nano-modeline-buffer-line-count ()
  "Buffer total number of lines"
  
  (save-excursion
    (goto-char (point-max))
    (propertize     
     (format-mode-line "(%l lines)")
     'face (nano-modeline-face))))

(defun nano-modeline-window-dedicated (&optional symbol)
  "Pin symbol when window is dedicated"
  
  (propertize (if (window-dedicated-p) (or symbol " ") "")
              'face (nano-modeline-face)))

(defun nano-modeline-git-info (&optional symbol)
  "Git information as (branch, file status)"
  
  (if vc-mode
      (when-let* ((file (buffer-file-name))
                  (branch (substring-no-properties vc-mode 5))
                  (state (vc-state file)))
        (propertize (format "(%s%s, %s)" (or symbol " ") branch state)
                    'face (nano-modeline-face)))
    (propertize "" 'face (nano-modeline-face))))

(defun nano-modeline-mu4e-search-filter ()
  "Mu4e current search"
  
  (propertize (mu4e-last-query) 'face (nano-modeline-face 'bold)))

(defun nano-modeline-mu4e-context ()
  "Mu4e current context"
  
  (let* ((context (mu4e-context-current))
         (name (if context (mu4e-context-name context) "none")))
    (propertize (format "[%s] " name)
                'face (nano-modeline-face))))

(defun nano-modeline-mu4e-message-subject ()
  "Mu4e message subject"
  
  (let* ((msg (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject)))
    (propertize (format "%s" subject)  'face (nano-modeline-face 'bold))))

(defun nano-modeline-mu4e-message-date ()
  "Mu4e message date"
  
  (let* ((msg (mu4e-message-at-point))
         (date (mu4e-message-field msg :date)))
    (propertize (format-time-string " %d/%m " date) 'face (nano-modeline-face))))
 
(defun nano-modeline-pdf-page ()
  "PDF view mode page number / page total"
  
  (let ((page-current (image-mode-window-get 'page))
        (page-total (pdf-cache-number-of-pages)))
    (propertize (format "%d/%d " page-current page-total)
                'face (nano-modeline-face))))

(defun nano-modeline-elfeed-entry-status ()
  "Elfeed entry status"
  
  (let* ((feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title)))
    (nano-modeline-buffer-status feed-title)))

(defun nano-modeline-elfeed-entry-title ()
  "Elfeed entry title"
  
  (let* ((title (elfeed-entry-title elfeed-show-entry)))
    (propertize title 'face (nano-modeline-face 'bold))))

(defun nano-modeline-elfeed-search-filter ()
  "Elfeed search filter"
  
  (propertize
   (if (and (not (zerop (elfeed-db-last-update)))
            (> (elfeed-queue-count-total) 0))
       (let ((total (elfeed-queue-count-total))
             (in-process (elfeed-queue-count-active)))
         (format "%d jobs pending, %d active"  (- total in-process) in-process))
     (cond (elfeed-search-filter-active "")
           ((string-match-p "[^ ]" elfeed-search-filter) elfeed-search-filter)
           (t "")))
   'face (nano-modeline-face 'bold)))

(defun nano-modeline-elfeed-search-count ()
  "Elfeed search statistics"
  
  (propertize (cond ((zerop (elfeed-db-last-update)) "")
                    ((> (elfeed-queue-count-total) 0) "")
                    (t (concat (elfeed-search--count-unread) " ")))
   'face (nano-modeline-face)))

(defun nano-modeline-date (&optional date format)
  "Date using given FORMAT and DATE"

  (propertize (format-time-string (or format "%A %-e %B %Y") date) 
              'face (nano-modeline-face)))

(defun nano-modeline-org-agenda-date (&optional format)
  "Date at point in org agenda  using given FORMAT"

  (when-let* ((day (or (org-get-at-bol 'ts-date)
                       (org-get-at-bol 'day)))
              (date (calendar-gregorian-from-absolute day))
              (day (nth 1 date))
              (month (nth 0 date))
              (year (nth 2 date))
              (date (encode-time 0 0 0 day month year)))
    (propertize (format-time-string (or format "%A %-e %B %Y") date) 
                'face (nano-modeline-face))))

(defun nano-modeline-term-shell-name ()
  "Term shell name"

  (propertize shell-file-name
              'face (nano-modeline-face 'bold)))

(defun nano-modeline-term-shell-mode ()
  "Term shell mode"
  
  (propertize (if (term-in-char-mode)
                  "(char mode)"
                "(line mode)")
               'face (nano-modeline-face)))

(defun nano-modeline-eat-shell-mode ()
  "Eat shell mode"
  
  (propertize (if eat--char-mode
                  "(char mode)"
                "(line mode)")
               'face (nano-modeline-face)))

(defun nano-modeline-default-directory (&optional max-length)
  "Term current directory"
  
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
    (propertize output 'face (nano-modeline-face))))

(defun nano-modeline-xwidget-uri ()
  "xwidget URI"
  
  (propertize (xwidget-webkit-uri (xwidget-at (point-min)))
              'face (nano-modeline-face 'bold)))

(defun nano-modeline-org-buffer-name (&optional name)
  "Org buffer name"
  
  (propertize
   (cond (name
          name)
         ((buffer-narrowed-p) 
          (format"%s [%s]" (or (buffer-base-buffer) (buffer-name))
                 (org-link-display-format 
                  (substring-no-properties
                   (or (org-get-heading 'no-tags) "-")))))
          (t
           (buffer-name)))
   'face (nano-modeline-face 'bold)))

(defun nano-modeline-org-capture-description ()
  "Org capture descrioption"
  
  (propertize (format "(%s)"
                      (substring-no-properties (org-capture-get :description)))
              'face (nano-modeline-face)))

(defun nano-modeline-prog-mode (&optional default)
  "Nano line for prog mode. Can be made DEFAULT mode."
  
  (funcall nano-modeline-position
            '((nano-modeline-buffer-status) " "
              (nano-modeline-buffer-name) " "
              (nano-modeline-git-info))
            '((nano-modeline-cursor-position)
              (nano-modeline-window-dedicated))
            default))

(defun nano-modeline-text-mode (&optional default)
  "Nano line for text mode. Can be made DEFAULT mode."

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status) " "
             (nano-modeline-buffer-name) " "
             (nano-modeline-git-info))
           '((nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))
           default))

(defun nano-modeline-org-mode ()
  "Nano line for org mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status) " "
             (nano-modeline-buffer-name) " "
             (nano-modeline-git-info))
           '((nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))))

(defun nano-modeline-pdf-mode ()
  "Nano line for text mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "PDF") " "
             (nano-modeline-buffer-name) " "
             (nano-modeline-file-size))
           '((nano-modeline-pdf-page)
             (nano-modeline-window-dedicated))))

(defun nano-modeline-mu4e-headers-mode ()
  "Nano line for mu4e headers mode"
  
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "MAIL") " "
             (nano-modeline-mu4e-search-filter))
           '((nano-modeline-mu4e-context)
             (nano-modeline-window-dedicated))))

(defun nano-modeline-mu4e-message-mode ()
  "Nano line for mu4e message mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "MAIL") " "
             (nano-modeline-mu4e-message-subject))
           '((nano-modeline-mu4e-message-date)
             (nano-modeline-window-dedicated))))

(defun nano-modeline-elfeed-entry-mode ()
  "Nano line for elfeed entry mode"
  
  (funcall nano-modeline-position
           '((nano-modeline-elfeed-entry-status) " "
             (nano-modeline-elfeed-entry-title))))

(defun nano-modeline-elfeed-search-mode ()
  "Nano line for elfeed search mode"
  
  (add-hook 'elfeed-search-update-hook #'force-mode-line-update)
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "NEWS") " "
             (nano-modeline-elfeed-search-filter))
           '((nano-modeline-elfeed-search-count)
             (nano-modeline-window-dedicated))))

(defun nano-modeline-term-mode ()
  "Nano line for term mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status ">_") " "
             (nano-modeline-term-shell-name) " "
             (nano-modeline-term-shell-mode))
           '((nano-modeline-default-directory) " "
             (nano-modeline-window-dedicated))))

(defun nano-modeline-eat-mode ()
  "Nano line for term mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status ">_") " "
             (nano-modeline-term-shell-name) " "
             (nano-modeline-eat-shell-mode))
           '((nano-modeline-default-directory) " "
             (nano-modeline-window-dedicated))))

(defun nano-modeline-xwidget-mode ()
  "Nano line for xwidget mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "URL") " "
             (nano-modeline-xwidget-uri))
           '((nano-modeline-window-dedicated))))
    
(defun nano-modeline-message-mode ()
  "Nano line for messages mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "LOG") " "
             (nano-modeline-buffer-name) " "
             (nano-modeline-buffer-line-count))
           '((nano-modeline-window-dedicated))))

(defun nano-modeline-org-capture-mode ()
  "Nano line for org capture mode"
  
  (funcall nano-modeline-position
            '((nano-modeline-buffer-status "ORG") " "
              (nano-modeline-buffer-name "Capture") " "
              (nano-modeline-org-capture-description))
            '("Finish: C-c C-c, refile: C-c C-w, cancel: C-c C-k "
              (nano-modeline-window-dedicated))))

(defun nano-modeline-org-agenda-mode ()
  "Nano line for org capture mode"

  (add-hook 'post-command-hook #'force-mode-line-update)
  (funcall nano-modeline-position
            '((nano-modeline-buffer-status "ORG") " "
              (nano-modeline-buffer-name "Agenda"))
            '((nano-modeline-org-agenda-date) " "
              (nano-modeline-window-dedicated))))

(provide 'nano-modeline)
;;; nano-modeline.el ends here

