;;; nano-modeline.el --- N Λ N O modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 1.0.1
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
;; Install all available modes:
;; (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;; (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;; (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;; (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;; (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;; (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;; (add-hook 'mu4e-compose-mode-hook    #'nano-modeline-mu4e-compose-mode)
;; (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;; (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
;; (add-hook 'elpher-mode-hook          #'nano-modeline-elpher-mode)
;; (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;; (add-hook 'eat-mode-hook             #'nano-modeline-eat-mode)
;; (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;; (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;; (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;; (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)
;;
;;
;;; NEWS:
;;
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

(defface nano-modeline-button-active-face
  `((t :foreground ,(face-foreground 'default)
       :background ,(face-background 'default)
       :family "Roboto Mono"
       :box (:line-width 2
             :color ,(face-foreground 'default)
             :style none)))
  "Active button face")

(defface nano-modeline-button-inactive-face
  `((t :foreground ,(face-foreground (if (facep 'nano-faded) 'nano-faded 'default))
       :background ,(face-background 'header-line nil t)
       :family "Roboto Mono"
       :box (:line-width 2
             :color ,(face-foreground 'default)
             :style none)))
  "Inactive button face.")

(defface nano-modeline-button-highlight-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'default)
       :family "Roboto Mono"
       :weight bold))
  "Highlight button face.")

(defun nano-modeline--stroke-width (face)
  "Extract the line width of the box for the given FACE."
  
  (let* ((box (face-attribute face ':box nil 'default))
         (width (plist-get box ':line-width)))
      (cond ((integerp width) width)
            ((consp width) (car width))
            (t 0))))

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
    (name-active        . (bold))
    (primary-active     . ())
    (secondary-active   . (,(when (facep 'nano-faded) 'nano-faded))))
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
           (fringe (if fringes-outside-margins 0.0 -1.0))
           (left-max-size (- width (length right) 2))
           (left (if (> (length left) left-max-size)
                     (concat (truncate-string-to-width left left-max-size)
                             (propertize "…" 'face `(:inherit  ,nano-modeline-base-face)))
                   left)))
      (concat (propertize " "
                'display `(space :align-to (+ left
                                              (,fringe . left-fringe)
                                              ( 0.0 . left-margin))))
              left
              (propertize " "
                'face `(:inherit ,nano-modeline-base-face)
                'display `(space :align-to (- right
                                              (,fringe . right-fringe)
                                              ( 0.0 . right-margin)
                                              ,(length right))))
              right))))



(defun nano-modeline--stroke-color (face)
  "Extract the line color of the box for the given FACE."
  
  (let* ((box (face-attribute face ':box))
         (color (plist-get box ':color)))
    (cond ((stringp color) color)
          (t (face-foreground face nil 'default)))))

(defun nano-modeline--make-text-button (label face)
  "Make a text button from LABEL and FACE"

  (let* ((foreground (face-foreground face nil 'default))
         (background (face-background face nil 'default))
         (label (concat " " label " "))
         ;; We compensate the footer padding with an irregular outer
         ;; box around label (vertical border with a default
         ;; background color). If this is not made the background color
         ;; is the height of the modeline which is not very aesthetic.
         (padding (floor (/ (* (frame-char-height)
                               (+ (car nano-modeline-padding)
                                  (cdr nano-modeline-padding))) 2)))
         (padding (+ padding 0))
         (window (get-buffer-window (current-buffer)))
         (active (eq window nano-modeline--selected-window))
         (face (if active
                   'nano-modeline-active
                 'nano-modeline-inactive)))
    (propertize label
                'face `(:inherit ,face
                        :foreground ,foreground
                        :background ,background))))

(defvar nano-modeline--cached-svg-buttons nil
  "List of cached SVG buttons indexed with (label . face)")
(setq nano-modeline--cached-svg-buttons nil)

(defun nano-modeline--make-svg-button (label face)
  "Make a svg button from LABEL and FACE"
    
  (require 'svg-lib)
  (let* ((key (cons label face))
         (cached (assoc key nano-modeline--cached-svg-buttons))
         (foreground (face-foreground face nil 'default))
         (background (face-background face nil 'default))
         (weight (face-attribute face ':weight nil 'default))
         (stroke (nano-modeline--stroke-width face))
         (family (face-attribute face ':family nil 'default))
         (button (cond (cached (cdr cached))
                       ((string-match "\\([a-zA-Z0-9-]+\\):\\([a-zA-Z0-9]+\\)" label)
                        (propertize "   "
                                   'display (svg-lib-icon (match-string 1 label) nil
                                                          :foreground foreground
                                                          :background background
                                                          :font-weight weight
                                                          :font-family family
                                                          :stroke stroke
                                                          :height 1.
                                                          :padding 1
                                                          :margin 0
                                                          :collection (match-string 2 label))))
                       (t
                        (propertize (concat label " ")
                                    'display (svg-lib-tag label nil
                                                          :foreground foreground
                                                          :background background
                                                          :font-weight weight
                                                          :font-family family
                                                          :stroke stroke
                                                          :padding 1
                                                          :margin 0))))))
    (unless cached
      (add-to-list 'nano-modeline--cached-svg-buttons (cons key button)))
    button))


(defun nano-modeline--make-button (button &optional use-svg)
  "Make a button from a BUTTON decription. When USE-SVG is t and
svg-lib is installed, result is a SVG button else, it is a text
button."

  (let* ((label (plist-get button :label))
         (label (if (functionp label)
                    (funcall label)
                  label))
         (state (plist-get button :state))
         (help (plist-get button :help))
         (hook (plist-get button :hook))
         (window (get-buffer-window (current-buffer)))
         (active (eq window nano-modeline--selected-window))         
         (face (cond ((not active)          'nano-modeline-button-inactive-face)
                     ((eq state 'highlight) 'nano-modeline-button-highlight-face)
                     ((eq state 'inactive)  'nano-modeline-button-inactive-face)
                     (t                     'nano-modeline-button-active-face)))
         (button (if (and use-svg (package-installed-p 'svg-lib))
                     (nano-modeline--make-svg-button label face)
                   (nano-modeline--make-text-button label face))))
    (propertize button
                'pointer 'hand
                'label label
                'keymap (let ((map (make-sparse-keymap)))
                          (define-key map [header-line mouse-1] hook)
                          (define-key map [mode-line mouse-1] hook)
                          map)
                'help-echo `(lambda (window object pos)
                              (nano-modeline--update-button-state ,label 'highlight)
                              (let (message-log-max)
                                (message ,help))
                              nil))))

(defun nano-modeline--reset-button-state (&rest args)
  "Reset the state of all the buttons."

  (when (boundp 'nano-modeline--buttons)
    (dolist (buttons (mapcar 'cdr nano-modeline--buttons))
      (dolist (button buttons)
        (unless (eq (plist-get button :state) 'inactive)
          (plist-put button :state 'active)))))
  (force-mode-line-update))

(defun nano-modeline--update-button-state (label state)
  "Update the state of the button LABEL with new STATE and update
other button states."

  (let* ((window (get-buffer-window (current-buffer)))
         (active (eq window nano-modeline--selected-window)))
    (when (and active (boundp 'nano-modeline--buttons))
      (dolist (buttons (mapcar 'cdr nano-modeline--buttons))
        (dolist (button buttons)
          (unless (eq (plist-get button :state) 'inactive)
            (let* ((button-label (plist-get button :label))
                   (button-label (if (functionp button-label)
                                     (funcall button-label)
                                   button-label)))
          (if (string-equal button-label label)
              (plist-put button :state state)
            (plist-put button :state 'active))))))))
  (force-mode-line-update))

(defun nano-modeline-header (left &optional right default)
  "Install a header line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (require 'tooltip)
  
  (if default
      (setq-default header-line-format (nano-modeline--make left right 'header))
    (setq-local header-line-format (nano-modeline--make left right 'header)))
  (make-local-variable 'nano-modeline--buttons)
  (setq nano-modeline--buttons nil)
  (advice-add 'tooltip-hide :before #'nano-modeline--reset-button-state)
  (face-remap-set-base 'header-line 'nano-modeline--empty-face)
  (add-hook 'post-command-hook #'nano-modeline--update-selected-window))

(defun nano-modeline-footer (left &optional right default)
  "Install a footer line made of LEFT and RIGHT parts. Line can be
made DEFAULT."

  (if default
      (setq-default mode-line-format (nano-modeline--make left right 'header))
    (setq-local mode-line-format (nano-modeline--make left right 'header)))
  (make-local-variable 'nano-modeline--buttons)
  (setq nano-modeline--buttons nil)
  (advice-add 'tooltip-hide :before #'nano-modeline--reset-button-state)
  (face-remap-set-base 'mode-line 'nano-modeline--empty-face)
  (face-remap-set-base 'mode-line-inactive 'nano-modeline-empty-face)
  (add-hook 'post-command-hook #'nano-modeline--update-selected-window))

(defun nano-modeline-buffer-name (&optional name)
  "Buffer name"
  
  (propertize
   (cond (name name)
         ((buffer-narrowed-p) (format"%s [narrow]" (buffer-name)))
         (t (buffer-name)))
   'face (nano-modeline-face 'name)))

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


(defun nano-modeline-buttons (buttons &optional use-svg group)
  "Clickable BUTTONS in text or svg mode depending on
USE-SVG. BUTTONS is a list of cons (label. (hook . help)) where
hook is an interactive function that is called when the button is
clicked and help is the tooltip help message. GROUP (default to
0) is an arbitrary optional index of the group this button
belongs to.If you want to have button highlight when the mouse
hovers a button, tooltip mode needs to be active and tooltip
delay needs to be set to 0."
  
  (unless (and (boundp 'nano-modeline--buttons)
               nano-modeline--buttons
               (assoc (or group 0) nano-modeline--buttons))
    (unless (boundp 'nano-modeline--buttons)
      (make-local-variable 'nano-modeline--buttons))
    (let* ((group (or group 0))
           (buttons (mapcar (lambda (button)
                              (list ':label (car button)
                                    ':state 'active
                                    ':help (cddr button)
                                    ':hook (cadr button)))
                            buttons)))
      (if (cdr (assoc group nano-modeline--buttons))
          (setf (cdr (assoc group nano-modeline--buttons)) buttons)
        (add-to-list 'nano-modeline--buttons (cons group buttons)))))

  (let* ((buttons (cdr (assoc (or group 0) nano-modeline--buttons)))
         (buttons (if (and use-svg (package-installed-p 'svg-lib))
                      (mapconcat (lambda (button)
                                   (nano-modeline--make-button button t))
                                 buttons (propertize " " 'face (nano-modeline-face)))
                    (mapconcat (lambda (button)
                                 (nano-modeline--make-button button nil))
                               buttons (propertize " " 'face (nano-modeline-face))))))
    (if use-svg
        (propertize buttons 'face (nano-modeline-face))
      buttons)))

(defun nano-modeline-file-size ()
  "File size in human readable format"

  (if-let* ((file-name (buffer-file-name))
            (file-attributes (file-attributes file-name))
            (file-size (file-attribute-size file-attributes))
            (file-size (file-size-human-readable file-size)))
      (propertize (format "(%s)" file-size)
                  'face (nano-modeline-face 'primary))
    ""))

(defun nano-modeline-cursor-position (&optional format)
  "Cursor position using given FORMAT."

  (let ((format (or format "%l:%c ")))
    (propertize (format-mode-line format)
                'face (nano-modeline-face 'secondary))))

(defun nano-modeline-buffer-line-count ()
  "Buffer total number of lines"
  
  (save-excursion
    (goto-char (point-max))
    (propertize     
     (format-mode-line "(%l lines)")
     'face (nano-modeline-face 'primary))))

(defun nano-modeline-window-dedicated (&optional symbol)
  "Pin symbol when window is dedicated"
  
  (propertize (if (window-dedicated-p) (or symbol " ") "")
              'face (nano-modeline-face 'secondary)))

(defun nano-modeline-git-info (&optional symbol)
  "Git information as (branch, file status)"
  
  (when vc-mode
      (when-let* ((file (buffer-file-name))
                  (branch (substring-no-properties vc-mode 5))
                  (state (vc-state file)))
        (propertize (format "(%s%s, %s)" (or symbol " ") branch state)
                    'face (nano-modeline-face 'primary)))))


(defun nano-modeline-mu4e-search-filter ()
  "Mu4e current search"
  
  (propertize (mu4e-last-query) 'face (nano-modeline-face 'name)))

(defun nano-modeline-mu4e-context ()
  "Mu4e current context"
  
  (let* ((context (mu4e-context-current))
         (name (if context (mu4e-context-name context) "none")))
    (propertize (format "[%s] " name)
                'face (nano-modeline-face 'secondary))))

(defun nano-modeline-mu4e-raw-context ()
  "Mu4e current context (raw form for button)"
  
  (let* ((context (mu4e-context-current))
         (name (if context (mu4e-context-name context) "NONE")))
    (upcase name)))

(defun nano-modeline-mu4e-message-from ()
  "Mu4e current message sender"
  
  (with-current-buffer "*mu4e-headers*"
    (let ((msg (mu4e-message-at-point)))
      (mu4e~headers-contact-str (mu4e-message-field msg :from)))))
                         
(defun nano-modeline-mu4e-view-in-xwidget ()
  (interactive)
  (with-current-buffer "*mu4e-headers*"
    (let ((msg (mu4e-message-at-point)))
      (mu4e-action-view-in-xwidget msg))))
 
(defun nano-modeline-mu4e-context-next ()
  "Switch to next mu4e context"

  (interactive)
  (let* ((current (mu4e-context-name (mu4e-context-current)))
         (contexts (mapcar (lambda (context)
                             (mu4e-context-name context))
                           mu4e-contexts))
         (index (mod (1+ (cl-position current contexts))
                     (length contexts)))
         (current (nth index contexts)))
    (mu4e-context-switch t current)))

(defun nano-modeline-mu4e-message-subject ()
  "Mu4e message subject"
  
  (let* ((msg (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject)))
    (propertize (format "%s" subject)
                'face (nano-modeline-face 'name))))

(defun nano-modeline-mu4e-message-date ()
  "Mu4e message date"
  
  (let* ((msg (mu4e-message-at-point))
         (date (mu4e-message-field msg :date)))
    (propertize (format-time-string " %d/%m " date)
                'face (nano-modeline-face 'secondary))))
 
(defun nano-modeline-pdf-page ()
  "PDF view mode page number / page total"
  
  (let ((page-current (image-mode-window-get 'page))
        (page-total (pdf-cache-number-of-pages)))
    (propertize (format "%d/%d " page-current page-total)
                'face (nano-modeline-face 'secondary))))

(defun nano-modeline-elfeed-entry-status ()
  "Elfeed entry status"
  
  (let* ((feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (plist-get (elfeed-feed-meta feed) :title)))
    (nano-modeline-buffer-status feed-title)))

(defun nano-modeline-elfeed-entry-title ()
  "Elfeed entry title"
  
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (title (string-replace "%" "%%" title)))
    (propertize title 'face (nano-modeline-face 'name))))

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
   'face (nano-modeline-face 'name)))

(defun nano-modeline-elfeed-search-count ()
  "Elfeed search statistics"
  
  (propertize (cond ((zerop (elfeed-db-last-update)) "")
                    ((> (elfeed-queue-count-total) 0) "")
                    (t (concat (elfeed-search--count-unread) " ")))
   'face (nano-modeline-face 'secondary)))

(defun nano-modeline-elpher-protocol ()
  "Elpher protocol"
  
  (propertize (format "(%s)"
   (elpher-address-protocol (elpher-page-address elpher-current-page)))
   'face (nano-modeline-face 'primary)))

(defun nano-modeline-elpher-title ()
  "Elpher protocol"

  (propertize
   (elpher-page-display-string elpher-current-page)
   'face (nano-modeline-face 'name)))

(defun nano-modeline-date (&optional date format)
  "Date using given FORMAT and DATE"

  (propertize (format-time-string (or format "%A %-e %B %Y") date) 
              'face (nano-modeline-face 'secondary)))

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
                'face (nano-modeline-face 'secondary))))

(defun nano-modeline-term-shell-name ()
  "Term shell name"

  (propertize shell-file-name
              'face (nano-modeline-face 'name)))

(defun nano-modeline-term-shell-mode ()
  "Term shell mode"
  
  (propertize (if (term-in-char-mode)
                  "(char mode)"
                "(line mode)")
               'face (nano-modeline-face 'primary)))

(defun nano-modeline-eat-shell-mode ()
  "Eat shell mode"
  
  (propertize (if eat--char-mode
                  "(char mode)"
                "(line mode)")
               'face (nano-modeline-face 'primary)))

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
    (propertize output 'face (nano-modeline-face 'secondary))))

(defun nano-modeline-xwidget-uri ()
  "xwidget URI"
  
  (propertize (xwidget-webkit-uri (xwidget-at (point-min)))
              'face (nano-modeline-face 'name)))

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
   'face (nano-modeline-face 'name)))

(defun nano-modeline-org-outline-path ()
  "Org outline path"
  
  (let ((path (org-with-point-at (org-get-at-bol 'org-marker)
                (org-display-outline-path nil nil " » " t))))
    (propertize (substring-no-properties path)
                'face (nano-modeline-face 'name))))

(defun nano-modeline-org-capture-description ()
  "Org capture descrioption"
  
  (propertize (format "(%s)"
                      (substring-no-properties (org-capture-get :description)))
              'face (nano-modeline-face 'primary)))

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

(defun nano-modeline-elpher-mode ()
  "Nano line for elpher mode"

  (setq elpher-use-header nil)
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "GEM") " "
             (nano-modeline-elpher-title) " "
             (nano-modeline-elpher-protocol))
           '((nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))))

(defun nano-modeline-org-mode ()
  "Nano line for org mode"

  (funcall nano-modeline-position
           '((nano-modeline-buffer-status) " "
             (nano-modeline-org-buffer-name) " "
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
  "Nano line for mu4e headers mode with a button to change context"

  (let ((buttons '((nano-modeline-mu4e-raw-context . (nano-modeline-mu4e-context-next . "Switch to next context")))))
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "MAIL") " "
             (nano-modeline-mu4e-search-filter))
             `((nano-modeline-buttons ,buttons t) " "
             (nano-modeline-window-dedicated)))))

(defun nano-modeline-mu4e-message-mode ()
  "Nano line for mu4e message mode with several buttons for most
common action"

  (let ((buttons '(("archive:bootstrap" . (mu4e-view-mark-for-refile . "Archive message"))
                   ("trash:bootstrap" . (mu4e-view-mark-for-trash . "Delete message"))
                   ("file-richtext:bootstrap". (nano-modeline-mu4e-view-in-xwidget . "View message as HTML"))
                   ("folder:bootstrap". (mu4e-headers-mark-for-move . "Move message"))
                   ("tag:bootstrap". (mu4e-headers-mark-for-tag . "Tag message"))
                   ("reply:bootstrap". (mu4e-compose-reply . "Reply to message"))
                   ("forward:bootstrap". (mu4e-compose-forward . "Forward message")))))
    (funcall nano-modeline-position
             `((nano-modeline-buffer-status "FROM") " "
               (nano-modeline-buffer-name ,(nano-modeline-mu4e-message-from)))
             `((nano-modeline-buttons ,buttons t) " "
               (nano-modeline-window-dedicated)))))

(defun nano-modeline-mu4e-compose-mode ()
  "Nano line for mu4e compose mode"

  (let ((buttons '(("download:bootstrap" . (save-buffer . "Save message"))
                   ("paperclip:bootstrap" . (mml-attach-file . "Attach file"))
                   ("lock:bootstrap" . (mml-secure-message-encrypt . "Encrypt message"))
                   ("check:bootstrap" . (mml-secure-message-sign . "Sign message"))
                   ("send:bootstrap" . (message-send-and-exit . "Send message")))))
    (funcall nano-modeline-position
             `((nano-modeline-buffer-status "DRAFT") " "
               (nano-modeline-buffer-name "Message"))
             `((nano-modeline-buttons ,buttons t) " "
               (nano-modeline-window-dedicated)))))

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
  "Nano line for term (eat) mode"

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

  (let ((buttons '(("CAPTURE" . org-capture-finalize)
                   ("CANCEL" . org-capture-kill))))  
    (funcall nano-modeline-position
             `((nano-modeline-buffer-status "ORG") " "
               (nano-modeline-buffer-name "Capture") " "
               (nano-modeline-org-capture-description))
             `((nano-modeline-buttons ,buttons t) " "
               (nano-modeline-window-dedicated)))))

(defun nano-modeline-org-agenda-mode ()
  "Nano line for org agenda mode"

  (add-hook 'post-command-hook #'force-mode-line-update)
  (funcall nano-modeline-position
            '((nano-modeline-buffer-status "ORG") " "
              (nano-modeline-buffer-name "Agenda"))
            '((nano-modeline-org-agenda-date) " "
              (nano-modeline-window-dedicated))))

(provide 'nano-modeline)
;;; nano-modeline.el ends here

