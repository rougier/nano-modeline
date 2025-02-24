;;; nano-modeline.el --- N Λ N O modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/nano-modeline
;; Version: 2.0
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
;; Nano modeline is a an alternative to the GNU/Emacs modeline. It can
;; be displayed at the bottom (mode-line) or at the top (header-line)
;; depending on the nano-modeline-position custom setting. There are
;; several modelines that can be installed on a per-mode basis or as
;; the default.
;;
;; Everything is configurable via the nano-modeline customization group.
;;
;; Usage example:
;;
;; Use default modeline for the current buffer
;; (nano-modeline)
;;
;; Install the modeline for all prog buffers:
;; (add-hook 'prog-mode-hook
;;    (lambda () (nano-modeline nano-modeline-format-default)))
;;
;; Make the default modeline the default for all buffers:
;; (nano-modeline nil t)
;;
;;; NEWS:
;;
;; Version  2.0
;; - Full rewrite for simplification
;; - Advanced customization interfaces
;; - No more svg buttons (only text buttons)
;; - No mode active/inactive faces (active indicator instead)
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

(defgroup nano nil
  "N Λ N O"
  :group 'convenience)

(defgroup nano-modeline nil
  "N Λ N O Modeline"
  :group 'nano)

(defgroup nano-modeline-faces nil
  "N Λ N O modeline faces"
  :group 'nano-modeline)

(defgroup nano-modeline-modes nil
  "N Λ N O modeline modes"
  :group 'nano-modeline)

(define-widget 'nano-modeline-part-type 'lazy
  "Nano modeline part made of various elements."
  :offset 4
  :tag "Nano modeline part"
  :type '(repeat
          (choice (choice :tag "→ Border"
                     (const :tag "Left" nano-modeline-element-border-left)
                     (const :tag "Right" nano-modeline-element-border-right))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Space"
                     (const :tag "Full" nano-modeline-element-space)
                     (const :tag "Half" nano-modeline-element-half-space)
                     (list  :tag "Custom" (const :tag "" nano-modeline-element-space)
                                          (integer :tag "Width (pixels)")))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Calendar"
                     (const :tag "Selected date"  nano-modeline-element-calendar-date))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Mu4e"
                    (choice :tag "→ Compose"
                        (const :tag "Context (button)" nano-modeline-button-mu4e-compose-context)
                        (const :tag "Subject (live)" nano-modeline-element-mu4e-compose-subject)
                        (const :tag "Attach (button)" nano-modeline-button-mu4e-attach)
                        (const :tag "Sign (button)" nano-modeline-button-mu4e-sign)
                        (const :tag "Encrypt (button)" nano-modeline-button-mu4e-encrypt)
                        (const :tag "Send (button)" nano-modeline-button-mu4e-send))
                     (choice :tag "→ Message"
                        (const :tag "From" nano-modeline-element-mu4e-message-from)
                        (const :tag "To"   nano-modeline-element-mu4e-message-to)
                        (const :tag "Date" nano-modeline-element-mu4e-message-date)
                        (const :tag "Status" nano-modeline-element-mu4e-message-status)
                        (const :tag "Tags" nano-modeline-element-mu4e-message-tags))
                     (choice :tag "→ Headers"
                        (const :tag "Query" nano-modeline-element-mu4e-last-query)
                        (const :tag "Context (button)" nano-modeline-button-mu4e-context)
                        (const :tag "Folding (button)" nano-modeline-button-mu4e-folding)
                        (const :tag "Threads (button)" nano-modeline-button-mu4e-threads)
                        (const :tag "related (button)" nano-modeline-button-mu4e-related)
                        (const :tag "Update (button)"  nano-modeline-button-mu4e-update)))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Buffer"
                     (const :tag "Status" nano-modeline-element-buffer-status)
                     (const :tag "Interactive" nano-modeline-element-buffer-interactive)
                     (const :tag "Name" nano-modeline-element-buffer-name)
                     (const :tag "Mode" nano-modeline-element-buffer-mode)
                     (const :tag "VC mode" nano-modeline-element-buffer-vc-mode)
                     (const :tag "Position" nano-modeline-element-buffer-position))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ GPTel"
                     (const :tag "Status" nano-modeline-element-gptel-status)
                     (const :tag "Backend" nano-modeline-element-gptel-backend)
                     (const :tag "Query" nano-modeline-element-gptel-query-status)
                     (const :tag "Media (button)" nano-modeline-button-gptel-media)
                     (const :tag "Context (button)" nano-modeline-button-gptel-context)
                     (const :tag "Model (button)" nano-modeline-button-gptel-model))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Terminal"
                     (const :tag "Status" nano-modeline-element-terminal-status)
                     (const :tag "Name" nano-modeline-element-terminal-name)
                     (const :tag "Mode" nano-modeline-element-terminal-mode)
                     (const :tag "Working directory" nano-modeline-element-terminal-directory))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Org capture"
                     (const :tag "Description"  nano-modeline-element-org-capture-description)
                     (const :tag "Save (button)" nano-modeline-button-org-capture-save)
                     (const :tag "Kill (button)" nano-modeline-button-org-capture-kill)
                     (const :tag "Refile (button)" nano-modeline-button-org-capture-refile))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Elpher"
                     (const :tag "Protocol" nano-modeline-element-elpher-protocol)
                     (const :tag "Page title" nano-modeline-element-elpher-title)
                     (const :tag "Go back (button)" nano-modeline-button-elpher-back))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ NANO Agenda"
                     (const :tag "Date" nano-modeline-element-nano-agenda-date)
                     (const :tag "Go to previous month (button)" nano-modeline-button-nano-agenda-prev-month)
                     (const :tag "Go to previous day (button)" nano-modeline-button-nano-agenda-prev-day)
                     (const :tag "Go to today (button)" nano-modeline-button-nano-agenda-today)
                     (const :tag "Go to next day (button)" nano-modeline-button-nano-agenda-next-day)
                     (const :tag "Go to next month (button)" nano-modeline-button-nano-agenda-next-month))
                     ;; ----------------------------------------------------------
                  (choice :tag "→ Elfeed"
                     (const :tag "Update (button)" nano-modeline-button-elfeed-update)
                     (const :tag "Search filter" nano-modeline-element-elfeed-search-filter)
                     (const :tag "Search Count" nano-modeline-element-elfeed-search-count)
                     (const :tag "Entry feed" nano-modeline-element-elfeed-entry-feed)
                     (const :tag "Entry count" nano-modeline-element-elfeed-entry-count)
                     (const :tag "Entry title" nano-modeline-element-elfeed-entry-title))
                     ;; ----------------------------------------------------------
                  (choice :tag "→ Window"
                     (const :tag "Status" nano-modeline-element-window-status)
                     (const :tag "Close" nano-modeline-button-window-close))
                  (list  :tag "Button" (const :tag "" nano-modeline-button)
                                       (string :tag "Label")
                                       (function :tag "Action")
                                       (symbol :tag "state")
                                       (string :tag "Help message"))
                  ;; ----------------------------------------------------------
                  (choice :tag "→ Custom"
                     (string :tag "String")
                     (function :tag "Function")))))

(define-widget 'nano-modeline-type 'lazy
  "Nano modeline made of left and right parts"
  :offset 4
  :tag "Nano modeline"
  :type '(cons
          (nano-modeline-part-type :tag "Left part")
          (nano-modeline-part-type :tag "Right part")))

(defcustom nano-modeline-symbol-list
  '((buffer-read-only  . "RO")
    (buffer-read-write . "RW")
    (buffer-modified   . "**")
    (buffer-terminal   . ">_")
    (buffer-clone      . "CC")
    (buffer-narrow     . "NW")
    (window-close . "CLOSE")
    (window-active . "●")
    (window-inactive . "")
    (window-dedicated . "󰐃")
    (vc-branch  . "")
    (vc-hash  . "#")
    (mail-html . (" " . (4 . 0)))
    (mail-tag . (" " . (4 . 0)))
    (mail-attach . (" " . (4 . 0)))
    (mail-encrypt . (" " . (4 . 0)))
    (mail-sign . (" " . (4 . 0)))
    (mail-send  . ("󰒊 " . (4 . 0)))
    (mail-update . (" " . (4 . 0)))
    (mail-fold . (" " . (4 . 0)))
    (mail-unfold . (" " . (4 . 0))))
  "Various symbols used in the modeline. It is possible to add padding to
left and right for symbols that do not align perfectly (NERD
fonts). Default symbols make use of NERD font and may appear as tofu if
the fontis not installed on your system. Either install NERD font or use
other characters."
  :type '(alist :key-type symbol
                :value-type (choice (string :tag "Label")
                                    (cons  :tag "Label with padding"
                                          (string :tag "Label")
                                          (cons :tag "Padding in pixels"
                                                (integer :tag "Left")
                                                (integer :tag "Right")))))
  :group 'nano-modeline)

(defcustom nano-modeline-position 'header
  "Default position for the nano modeline"

  :type '(choice (const :tag "Top"    header)
                 (const :tag "Bottom" footer))
  :group 'nano-modeline)

(defcustom nano-modeline-borders (cons (display-graphic-p) (display-graphic-p))
  "Whether to add left / right borders to modlines"

  :type '(cons (boolean :tag "Left")
               (boolean :tag "Right"))
  :group 'nano-modeline)


(defcustom nano-modeline-border-color (face-background 'default)
  "Border color"

  :type '(color)
  :group 'nano-modeline)


(defcustom nano-modeline-alignment '(fringe . fringe)
  "Left and right alignment of the mode-line (or header-line).

fringes-outside-margins t
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┬───┐
   │                         'window alignment                           │
   └───┴────────┴───────────────────────────────────────┴────────┴───┴───┘
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┐
   │                         'fringe alignment                       │
   └───┴────────┴───────────────────────────────────────┴────────┴───┘
       ┌────────┬───────────────────────────────────────┬────────┐
       │                     'margin alignment                   │
       └────────┴───────────────────────────────────────┴────────┘
                ┌───────────────────────────────────────┐
                │             'text alignment           │
                └───────────────────────────────────────┘
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┬───┐
   │   │        │                                       │        │   │   │
   │ • │    •   │               Text area               │   •    │ • │ • │
   │ │ │    │   │                                       │   │    │ │ │ │ │
   └─┼─┴────┼───┴───────────────────────────────────────┴───┼────┴─┼─┴─┼─┘
     │ Left margin                                    Right margin │   │
   Left fringe                                            Right fringe │
                                                                Scroll bar

fringes-outside-margins nil
   ┌───┬────────┬───────────────────────────────────────┬────────┬───┬───┐
   │                         'window alignment                           │
   └───┴────────┴───────────────────────────────────────┴────────┴───┴───┘
   ┌────────┬───┬───────────────────────────────────────┬───┬────────┐
   │                         'margin alignment                       │
   └────────┴───┴───────────────────────────────────────┴───┴────────┘
            ┌───┬───────────────────────────────────────┬───┐
            │                'fringe alignment              │
            └───┴───────────────────────────────────────┴───┘
                ┌───────────────────────────────────────┐
                │             'text alignment           │
                └───────────────────────────────────────┘
   ┌────────┬───┬───────────────────────────────────────┬───┬────────┬───┐
   │        │   │                                       │   │        │   │
   │   •    │ • │               Text area               │ • │    •   │ • │
   │   │    │ │ │                                       │ │ │    │   │ │ │
   └───┼────┴─┼─┴───────────────────────────────────────┴─┼─┴────┼───┴─┼─┘
       │ Left fringe                                Right fringe │     │
    Left margin                                           Right margin │
                                                                Scroll bar
"
  :type '(cons (choice :tag "Left"
                       (const window)
                       (const margin)
                       (const fringe)
                       (const text))
               (choice :tag "Right"
                       (const window)
                       (const margin)
                       (const fringe)
                       (const text)))
  :group 'nano-modeline)


(defcustom nano-modeline-padding '(0.20 . 0.25)
  "Default vertical space adjustment (in fraction of character height) for
the buffer status element."
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'nano-modeline)

(defface nano-modeline-face-buffer-read-only
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for read only buffer"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-buffer-read-write
  `((t (:foreground ,(face-background 'font-lock-comment-face nil 'default)
        :background ,(face-foreground 'font-lock-comment-face nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for read-write buffer"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-buffer-modified
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'warning nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for modified buffer"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-buffer-interactive
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'link nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for interactive buffer"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-buffer-marked
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'error nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for marked buffer"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-tag
  `((t ( :foreground ,(face-background 'default)
         :background ,(face-foreground 'link nil 'default)
         :weight ,(face-attribute 'bold :weight nil 'default))))
  "Default face"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-default
  `((t (:foreground ,(face-foreground 'default))))
  "Default face"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-primary
  `((t (:foreground ,(face-foreground 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Face for primary information"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-secondary
  `((t (:foreground ,(face-foreground 'font-lock-comment-face nil 'default))))
  "Face for secondary information"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-button-active
  `((t (:foreground ,(face-background 'shadow nil 'default)
        :background ,(face-foreground 'shadow nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Active button face"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-button-progress
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'error nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Progress button face"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-button-dangerous
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'error nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Dangerous button face"
  :group 'nano-modeline-faces)

(defface nano-modeline-face-button-inactive
  `((t (:foreground ,(face-foreground 'font-lock-comment-face nil 'default)
        :background ,(face-background 'default))))
  "Inactive button face."
  :group 'nano-modeline-faces)

(defface nano-modeline-face-button-highlight
  `((t (:foreground ,(face-background 'default)
        :background ,(face-foreground 'warning nil 'default)
        :weight ,(face-attribute 'bold :weight nil 'default))))
  "Highlight button face."
  :group 'nano-modeline-faces)

(defcustom nano-modeline-format-default
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-buffer-name
          nano-modeline-element-space
          nano-modeline-element-buffer-mode
          nano-modeline-element-space
          nano-modeline-element-buffer-vc-mode)
        '(nano-modeline-element-buffer-position
          nano-modeline-element-window-status
          nano-modeline-element-space))
    "Default format"
    :type 'nano-modeline-type
    :group 'nano-modeline-modes)

(defcustom nano-modeline-format-empty
  (cons '()
        '())
    "Empty format"
    :type 'nano-modeline-type
    :group 'nano-modeline-modes)

(defcustom nano-modeline-format-terminal
  (cons '(nano-modeline-element-terminal-status
          nano-modeline-element-space
          nano-modeline-element-terminal-name
          nano-modeline-element-space
          nano-modeline-element-terminal-mode)
        '(nano-modeline-element-terminal-directory
          nano-modeline-element-window-status
          nano-modeline-element-space))
  "Modeline format for terminals"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-calendar
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-calendar-date)
        '(nano-modeline-element-window-status
          nano-modeline-element-space))
  "Modeline format for calendar"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-org-capture
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-buffer-name
          nano-modeline-element-space
          nano-modeline-element-org-capture-description)
        '(nano-modeline-button-org-capture-kill
          nano-modeline-element-half-space
          nano-modeline-button-org-capture-refile
          nano-modeline-element-half-space
          nano-modeline-button-org-capture-save
          nano-modeline-element-half-space))
  "Modeline format for org capture"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-org-lookup
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-buffer-name
          nano-modeline-element-space
          nano-modeline-element-buffer-mode)
        '(nano-modeline-button-window-close
          nano-modeline-element-half-space))
  "Modeline format for org lookup"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-elpher
  (cons '(nano-modeline-element-elpher-protocol
          nano-modeline-element-space
          nano-modeline-element-elpher-title)
        '(nano-modeline-button-elpher-back
          nano-modeline-element-window-status
          nano-modeline-element-half-space))
    "Elpher format"
    :type 'nano-modeline-type
    :group 'nano-modeline-modes)

(defcustom nano-modeline-format-nano-agenda
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-nano-agenda-date)
        '(nano-modeline-button-nano-agenda-prev-month
          nano-modeline-element-half-space
          nano-modeline-button-nano-agenda-prev-day
          nano-modeline-element-half-space
          nano-modeline-button-nano-agenda-today
          nano-modeline-element-half-space
          nano-modeline-button-nano-agenda-next-day
          nano-modeline-element-half-space
          nano-modeline-button-nano-agenda-next-month
          nano-modeline-element-half-space))
    "NANO agenda format"
    :type 'nano-modeline-type
    :group 'nano-modeline-modes)

(defcustom nano-modeline-format-elfeed-search
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-elfeed-search-filter)
        '(nano-modeline-element-elfeed-search-count
          nano-modeline-element-space
          nano-modeline-button-elfeed-update
          nano-modeline-element-window-status
          nano-modeline-element-half-space))
  "Modeline format for elfeed search"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-elfeed-entry
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-elfeed-entry-feed)
        '(nano-modeline-element-elfeed-entry-count
          nano-modeline-element-window-status
          nano-modeline-element-space))
  "Modeline format for elfeed search"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-mu4e-headers
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-mu4e-last-query)
        '(nano-modeline-button-mu4e-context
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-folding
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-threads
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-related
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-update
          nano-modeline-element-half-space))
  "Modeline for mu4e headers mode"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-mu4e-message
  (cons '(;; nano-modeline-element-buffer-status
          nano-modeline-element-mu4e-message-status
          nano-modeline-element-space
          nano-modeline-element-mu4e-message-from
          " to "
          nano-modeline-element-mu4e-message-to)
        '(nano-modeline-element-mu4e-message-tags
          nano-modeline-element-half-space))
  "Modeline for mu4e message mode"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-mu4e-compose
  (cons '(nano-modeline-element-buffer-status
          nano-modeline-element-space
          nano-modeline-element-mu4e-compose-subject)
        '(nano-modeline-button-mu4e-compose-context
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-attach
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-encrypt
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-sign
          nano-modeline-element-half-space
          nano-modeline-button-mu4e-send
          nano-modeline-element-half-space))
  "Modeline for mu4e compose mode"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defcustom nano-modeline-format-gptel
  (cons '(nano-modeline-element-buffer-interactive
          nano-modeline-element-space
          nano-modeline-element-gptel-backend
          nano-modeline-element-space
          nano-modeline-element-gptel-query-status)
        '(nano-modeline-button-gptel-media
          nano-modeline-element-half-space
          nano-modeline-button-gptel-context
          nano-modeline-element-half-space
          nano-modeline-button-gptel-model
          nano-modeline-element-half-space))
  "Modeline for gptel (requires gptel-use-header-line to be nil)"
  :type 'nano-modeline-type
  :group 'nano-modeline-modes)

(defun nano-modeline-align-to (direction what &optional char-size pixel-size)
  "This methods return a display space specification to align some text on
the DIRECTION ('left or 'right) of WHAT ('window, 'margin, 'fringe or
'text) plus CHAR-SIZE and PIXEL-SIZE."

  (let* ((char-size (or char-size 0))
         (pixel-size (or pixel-size 0)))
    (cond ((eq 'left direction)
           (cond ((eq what 'window) (if fringes-outside-margins
                                        `(space :align-to (+ left-fringe
                                                             (,pixel-size)
                                                             ,char-size))
                                      `(space :align-to (+ left-margin
                                                           (,pixel-size)
                                                           ,char-size))))
                 ((eq what 'fringe) `(space :align-to (+ left-fringe
                                                         (,pixel-size)
                                                         ,char-size)))
                 ((eq what 'margin) `(space :align-to (+ left-margin
                                                         (,pixel-size)
                                                         ,char-size)))
                 (t                 `(space :align-to (+ left
                                                         (,pixel-size)
                                                         ,char-size)))))
          ((eq 'right direction)
           (cond ((eq what 'window) `(space :align-to (+ scroll-bar
                                                  (1.0 . scroll-bar)
                                                  (,pixel-size)
                                                  ,char-size)))
          ((eq what 'fringe) `(space :align-to (+ right-fringe
                                                  (1.0 . right-fringe)
                                                  (,pixel-size)
                                                  ,char-size)))
          ((eq what 'margin) `(space :align-to (+ right-margin
                                                  (1.0 . right-margin)
                                                  (,pixel-size)
                                                  ,char-size)))
          (t                 `(space :align-to (+ right
                                                  (,pixel-size)
                                                  ,char-size))))))))

(defun nano-modeline-padding-to (&optional left right face)
  "This function returns the left and right padding to precisely align the
mode-line (or header-line) to window, margin, fringe or text extents,
depending on LEFT and RIGHT. LEFT and RIGHT can be constant ('window,
'fringe, 'margin or 'text) or a cons specifying (what . (char-size
. pixel-size)). It returns two strings that must be respectively
prepended and appended to the mode-line (or header-line). An optional
FACE can be given to be used for the prefix and the suffix."

  (let* ((left (or left (car nano-modeline-alignment)))
         (left (if (not (consp left))
                   (cons left '(0 . 0))
                 left))
         (right (or right (cdr nano-modeline-alignment)))
         (right (if (not (consp right))
                    (cons right '(0 . 0))
                  right))
         (face (or face `(:foreground ,(face-foreground 'default)
                          :background ,(face-background 'default)
                          :underline nil :overline nil :inherit nil :box nil))))
    (cons (concat (propertize " " 'display (nano-modeline-align-to 'left 'window))
                  (propertize " " 'display (nano-modeline-align-to 'left (car left) (cadr left) (cddr left))
                                  'face face))
          (concat (propertize " " 'display (nano-modeline-align-to 'right (car right) (cadr right) (cddr right)))
                  (propertize " " 'display (nano-modeline-align-to 'right 'window) 'face face)))))

(defun nano-modeline--eval-element (element)
  "Evaluate an ELEMENT depending on its type."

  (cond ((stringp element)   element)
        ((functionp element) (funcall element))
        ((consp element)     (apply element))
        (t                   "(error)")))

(defun nano-modeline-make (format)
  "This function builds a modeline with LEFT on the left and RIGHT on the
right. It takes care of truncating first the left part and then the
right part if the combination of left and right doesn't fit the
modeline."

  (let* ((left (concat (when (car nano-modeline-borders)
                         (nano-modeline-element-border-left))
                  (mapconcat #'nano-modeline--eval-element (car format) "")))
         (right (concat (mapconcat #'nano-modeline--eval-element (cdr format) "")
                        (when (cdr nano-modeline-borders)
                          (nano-modeline-element-border-right))))
         (alignment nano-modeline-alignment)
         (padding (nano-modeline-padding-to (car alignment) (cdr alignment)))

         ;; Compute modeline width taking into account margin and alignment
         (width (+ (window-width)
                   (cond ((and fringes-outside-margins (eq 'fringe (car alignment)))
                          (or (car (window-margins)) 0))
                         ((and (not fringes-outside-margins) (eq 'margin (car alignment)))
                          (+ 1 (or (car (window-margins)) 0)))
                         (t 0))
                   (cond ((and fringes-outside-margins (eq 'fringe (cdr alignment)))
                          (or (cdr (window-margins)) 0))
                         ((and (not fringes-outside-margins) (eq 'margin (cdr alignment)))
                          (+ 1 (or (cdr (window-margins)) 0)))
                         (t 0))))

         ;; Truncate left member (first) if necessary, minimum size is 7
         ;; -> 7 corresponds to the size of border + buffer status + 1
         (right-width (floor (/ (string-pixel-width right) (frame-char-width))))
         (max-width (max (- width right-width) 7))
         (left (truncate-string-to-width left max-width nil nil "…"))

         ;; Truncate right member (second) if necessary
         (right (if (> (+ right-width (length left)) width)
                    (truncate-string-to-width right (- width (length left)) nil nil "…")
                  right))
         (pixel-adjust (- (string-pixel-width right))))
    (concat (car padding)
            left
            (propertize " "
                        'display (nano-modeline-align-to
                                  'right (cdr alignment) 0 pixel-adjust))
            right
            (cdr padding))))

(defun nano-modeline-symbol (name)
  "Retrieve SYMBOL from the nano-modeline-symbols list"
  (or (alist-get name nano-modeline-symbol-list) "??"))

(defun nano-modeline-element-buffer-status (&optional symbol face raise)
  "Return a prefix indicating if buffer is read-only, read-write or modified"

  (let* ((raise (or raise nano-modeline-padding))
         (face  (or face (cond (buffer-read-only    'nano-modeline-face-buffer-read-only)
                               ((buffer-modified-p) 'nano-modeline-face-buffer-modified)
                               (t                   'nano-modeline-face-buffer-read-write))))
         (symbol (or symbol (cond ((buffer-narrowed-p)  (nano-modeline-symbol 'buffer-narrow))
                                  ((buffer-base-buffer) (nano-modeline-symbol 'buffer-clone))
                                  (buffer-read-only     (nano-modeline-symbol 'buffer-read-only))
                                  ((buffer-modified-p)  (nano-modeline-symbol 'buffer-modified))
                                  (t                    (nano-modeline-symbol 'buffer-read-write))))))
    (propertize (concat
                 (propertize " " 'display `(raise ,(car raise)))
                 symbol
                 (propertize " " 'display `(raise ,(- (cdr raise)))))
                'face face)))

(defun nano-modeline-element-buffer-interactive (&optional symbol face raise)
  "Return a prefix indicating if buffer is interactive, read-write or modified"

  (let* ((raise (or raise nano-modeline-padding))
         (face  (or face (cond (buffer-read-only    'nano-modeline-face-buffer-interactive)
                               ((buffer-modified-p) 'nano-modeline-face-buffer-interactive)
                               (t                   'nano-modeline-face-buffer-interactive))))
         (symbol (or symbol (cond ((buffer-narrowed-p)  (nano-modeline-symbol 'buffer-narrow))
                                  ((buffer-base-buffer) (nano-modeline-symbol 'buffer-clone))
                                  (buffer-read-only     (nano-modeline-symbol 'buffer-read-only))
                                  ((buffer-modified-p)  (nano-modeline-symbol 'buffer-modified))
                                  (t                    (nano-modeline-symbol 'buffer-read-write))))))
    (propertize (concat
                 (propertize " " 'display `(raise ,(car raise)))
                 symbol
                 (propertize " " 'display `(raise ,(- (cdr raise)))))
                'face face)))

(defun nano-modeline-element-buffer-name ()
  "Return a string with the buffer name"

  (propertize (format-mode-line "%b") 'face 'nano-modeline-face-primary))

(defun nano-modeline-element-buffer-mode ()
  "Return a string describing current major mode."

  (propertize
   (concat
    "(" (downcase (cond ((consp mode-name) (car mode-name))
                        ((stringp mode-name) mode-name)
                        (t "unknow")))
    (when (buffer-narrowed-p)  "/narrow") " mode)")
   'face 'nano-modeline-face-default))

(defun nano-modeline-element-buffer-vc-mode ()
  "VC information as (branch, file status)"

  (when vc-mode
      (when-let* ((file (buffer-file-name))
                  (branch (substring-no-properties vc-mode 5))
                  (state (vc-state file)))
        (propertize (format "(%s%s, %s)" (nano-modeline-symbol 'vc-branch) branch state)
                    'face 'nano-modeline-face-default))))

(defun nano-modeline-element-buffer-position ()
  "Return a string describing current position in buffer."
  (propertize (format-mode-line " %C:%l") 'face 'nano-modeline-face-secondary))

(defun nano-modeline-element-window-spacing ()
  "Return conditional spacing"
  '(:eval (let (mode-line-format)
            (let ((padding (nano-modeline-padding-to)))
              (concat (car padding)
                      (if (or (window-in-direction 'down)
                              isearch-mode)
                          (propertize " " 'face '(:height 80))
                        (propertize " " 'face '(:height 10)))
                      (cdr padding))))))

(defun nano-modeline-element-window-status ()
  "Return a string describing window status"

  (let* ((dedicated (nano-modeline-symbol 'window-dedicated))
         (inactive (nano-modeline-symbol 'window-inactive))
         (active (nano-modeline-symbol 'window-active))
         (status (cond ((minibuffer-window-active-p (minibuffer-window))
                        (if (eq (old-selected-window) (selected-window))
                            (if (window-dedicated-p)
                                (cons dedicated 'nano-modeline-face-primary)
                              (cons active 'nano-modeline-face-primary))
                          (if (window-dedicated-p)
                             (cons dedicated 'nano-modeline-face-secondary))))
                       ((and (not (one-window-p)) (mode-line-window-selected-p))
                        (if (window-dedicated-p)
                            (cons dedicated 'nano-modeline-face-primary)
                          (cons active 'nano-modeline-face-primary)))
                       ((window-dedicated-p)
                        (cons dedicated 'nano-modeline-face-secondary))
                       ((not (one-window-p))
                        (cons inactive 'nano-modeline-face-secondary)))))
    (when (and (consp status) (> (length (car status)) 0))
      (propertize (concat " " (car status)) 'face (cdr status)))))


(defun nano-modeline-element-space (&optional width)
  "Return a space string of given pixel WIDTH."

  (let ((width (or width (frame-char-width))))
    (propertize " " 'display `(space :width (,width)))))

(defun nano-modeline-element-half-space ()
  "Return half a space string."

  (nano-modeline-element-space (floor (/ (frame-char-width) 2))))

(defun nano-modeline-element-border-left (&optional width face raise)
  "Return a string for a left border with given WIDTH (pixel) and FACE"

  (let* ((color nano-modeline-border-color)
         (face (or face `(:background ,color :underline ,color)))
        (width (or width 1))
        (raise (or raise 0)))
  (propertize " " 'display `((raise ,raise) (space :width (,width))) 'face face )))

(defun nano-modeline-element-border-right (&optional width face raise)
  "Return a string for a right border with given WIDTH (pixel) and FACE"

  (let* ((color nano-modeline-border-color)
         (face (or face `(:background ,color :underline ,color)))
         (width (or width 1))
         (raise (or raise 0))
         (align (cdr nano-modeline-alignment)))
    (concat
     (propertize " " 'display `((raise ,raise) (space :width (,width))) 'face face))))


(defun nano-modeline--button (label face)
  "Make a text button from LABEL and FACE."

  (let* ((padding (if (consp label)
                      (cdr label)
                    (cons (floor (/ (frame-char-width) 2))
                          (- (frame-char-width)
                             (floor (/ (frame-char-width) 2))))))
         (color nano-modeline-border-color)
         (label (if (consp label)
                    (car label)
                  label)))
    (propertize (concat (propertize " "   'display `((raise 0.1) (space :width (,(car padding)))))
                        (propertize label 'display `((raise 0.1)))
                        (propertize " "   'display `((raise 0.1) (space :width (,(cdr padding))))))
                'face `( :inherit ,face
                         ;; :weight regular
                         :height 0.75
                         :overline ,color ;; ,(face-foreground 'header-line nil 'default)
                         :underline ,(if (bound-and-true-p nano-box-state)
                                         (face-background 'default)
                                       color)
                         :box (:color ,(face-background 'header-line nil 'default)
                                      :line-width (0 . 4))))))

(defun nano-modeline-element-tags (tags &optional face)
  "Make a tags string from TAGS and FACE."

  (let ((face (or face 'nano-modeline-face-tag)))
    (mapconcat (lambda (tag)
                 (let ((tag (concat (car (nano-modeline-symbol 'mail-tag)) tag)))
                   (nano-modeline--button tag face)))
               tags
               (nano-modeline-element-half-space))))


(defun nano-modeline-button (label &optional action state help)
  "Make a text button from LABEL and STATE that triggers ACTION when
pressed. A HELP text can be provided as a tootlip."

  (let ((buffer (current-buffer)))
    (propertize (nano-modeline--button label
                                       (cond ((eq state 'active)    'nano-modeline-face-button-active)
                                             ((eq state 'disabled)  'nano-modeline-face-button-inactive)
                                             ((eq state 'dangerous) 'nano-modeline-face-button-progress)
                                             ((eq state 'progress)  'nano-modeline-face-button-progress)
                                             (t                     'nano-modeline-face-button-inactive)))
                'keymap (unless (or (eq state 'progress) (eq state 'disabled))
                          (let ((map (make-sparse-keymap)))
                            (define-key map (kbd "<header-line> <mouse-1>")
                                        `(lambda ()
                                           (interactive)
                                           (when (and (get-buffer-window ,buffer)
                                                      (functionp ',action))
                                             (with-current-buffer ,buffer
                                               (funcall ',action)))))
                            map))
                'pointer (unless (or (eq state 'progress) (eq state 'disabled))
                           'hand)
                'mouse-face (unless (or (eq state 'progress) (eq state 'disabled))
                              '(:inherit nano-modeline-face-button-highlight))
                'help-echo help)))


(defun nano-modeline-button-window-close ()
  "Return a string button that closes the window."

  (let ((buffer (current-buffer)))
    (nano-modeline-button (nano-modeline-symbol 'window-close)
                               `(lambda ()
                                  (when (get-buffer-window ,buffer)
                                    (delete-window (get-buffer-window ,buffer))))
                               'active)))

;; --- Terminal ---------------------------------------------------------------

(defun nano-modeline-element-terminal-status ()
  "Terminal status"
  (nano-modeline-element-buffer-status (nano-modeline-symbol 'buffer-terminal)
                                       'nano-modeline-face-buffer-read-only))

(defun nano-modeline-element-terminal-name ()
  "Terminal shell name"
  (propertize (format "%s" shell-file-name) 'face 'nano-modeline-face-primary))

(defun nano-modeline-element-terminal-directory (&optional max-length)
  "Current working directory"

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
    (propertize output 'face 'nano-modeline-face-secondary)))

(defun nano-modeline-element-terminal-mode ()
  "Terminal mode"
  (let ((mode (cond ((derived-mode-p '(term-mode))
                     (cond ((term-in-char-mode) "char")
                           ((term-in-line-mode) "line")
                           (t                   "????")))
                    ((derived-mode-p '(eat-mode))
                     (cond (eat--semi-char-mode "semi-char")
                           (eat--char-mode "char")
                           (eat--line-mode "line")))
                    (t nil))))
    (when mode
      (propertize (format "(%s mode)" mode) 'face 'nano-modeline-face-default))))

;; --- Org capture ------------------------------------------------------------

(defun nano-modeline-element-org-capture-description ()
  "Org capture description"

  (let* ((header (nth 4 (org-heading-components)))
         (header (or header ""))
         (header (org-link-display-format header))
         (header (replace-regexp-in-string org-ts-regexp3 "" header))
         (header (string-trim header))
         (header (substring-no-properties header)))
    (propertize (format "(%s)" header)
                'face 'nano-modeline-face-secondary)))

(defun nano-modeline-button-org-capture-save ()
  "Finalize the capture process."

    (nano-modeline-button "SAVE"
                          #'org-capture-finalize
                          'active
                          "Finalize the capture process"))

(defun nano-modeline-button-org-capture-kill ()
  "Abort the current capture process"

    (nano-modeline-button "KILL"
                          #'org-capture-kill
                          'active
                          "Abort the current capture process"))

(defun nano-modeline-button-org-capture-refile ()
  "Abort the current capture process"

    (nano-modeline-button "REFILE"
                          #'org-capture-refile
                          'active
                          "Finalize the current capture and then refile the entry."))

;; --- Calendar ---------------------------------------------------------------
(defun nano-modeline-element-calendar-date (&optional format)
  "Calendar date"

  (let* ((date (calendar-cursor-to-date))
         (date (when date
                 (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
         (format (or format "%d %B %Y")))
    (propertize (format-time-string format date)
                'face 'nano-modeline-face-primary)))


;; --- Elpher -----------------------------------------------------------------

(defun nano-modeline-element-elpher-protocol ()
  "Elpher protocol"

  (let* ((protocol (elpher-address-protocol (elpher-page-address elpher-current-page)))
         (symbol (cond ((equal protocol "gemini") "GEM")
                       ((equal protocol "gopher") "/"))))
    (nano-modeline-element-buffer-status symbol)))

(defun nano-modeline-element-elpher-title ()
  "Elpher page title"

  (propertize
   (elpher-page-display-string elpher-current-page)
   'face 'nano-modeline-face-primary))

(defun nano-modeline-button-elpher-back ()
  "Go to previous site."

  (if elpher-history
      (nano-modeline-button "BACK"
                            #'elpher-back
                            'active
                            "Go to previous site")
    (nano-modeline-button "BACK" nil 'disabled "")))


;; --- Nano Agenda -----------------------------------------------------------

(defun nano-modeline-element-nano-agenda-date (&optional format)
  "Current date"

  (let ((format (or format "%A %d %B %Y")))
    (propertize (format-time-string format nano-agenda-date)
                'face 'nano-modeline-face-primary)))

(defun nano-modeline-button-nano-agenda-prev-month ()
  "Go to previous month"

  (nano-modeline-button "<<"
                        #' nano-agenda-goto-prev-month
                        'active
                        "Go to previous month"))

(defun nano-modeline-button-nano-agenda-today ()
  "Go to today"

  (nano-modeline-button "TODAY"
                        #' nano-agenda-goto-today
                        'active
                        "Go to today"))

(defun nano-modeline-button-nano-agenda-prev-day ()
  "Go to previous day"

  (nano-modeline-button "<"
                        #' nano-agenda-goto-prev-day
                        'active
                        "Go to previous day"))

(defun nano-modeline-button-nano-agenda-next-day ()
  "Go to nextious day"

  (nano-modeline-button ">"
                        #' nano-agenda-goto-next-day
                        'active
                        "Go to next day"))

(defun nano-modeline-button-nano-agenda-next-month ()
  "Go to nextious month"

  (nano-modeline-button ">>"
                        #' nano-agenda-goto-next-month
                        'active
                        "Go to next month"))



;; --- Elfeed -----------------------------------------------------------------
(defun nano-modeline-element-elfeed-entry-feed ()
  "Elfeed entry status"

  (let* ((feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (plist-get (elfeed-feed-meta feed) :title)))
    (propertize feed-title 'face 'nano-modeline-face-primary)))

(defun nano-modeline-element-elfeed-entry-title ()
  "Elfeed entry title"

  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (title (string-replace "%" "%%" title)))
    (propertize title 'face 'nano-modeline-face-default)))

(defun nano-modeline-element-elfeed-entry-count ()
  "Count the number of entries and feeds being currently displayed."

  (with-current-buffer "*elfeed-search*"
    (cond ((zerop (elfeed-db-last-update)) " ")
          ((> (elfeed-queue-count-total) 0) " ")
          (t  (propertize (if (and elfeed-search-filter-active elfeed-search-filter-overflowing)
                              "?/?"
                            (cl-loop with feeds = (make-hash-table :test 'equal)
                                     for entry in elfeed-search-entries
                                     for feed = (elfeed-entry-feed entry)
                                     for url = (elfeed-feed-url feed)
                                     count entry into entry-count
                                     count (elfeed-tagged-p 'unread entry) into unread-count
                                     do (puthash url t feeds)
                                     finally
                                     (cl-return
                                      (format "%s/%s" (+ 1 unread-count) entry-count))))
                          'face 'nano-modeline-face-secondary)))))

(defun nano-modeline-button-elfeed-update ()
  "Button to start an update."

  (with-current-buffer "*elfeed-search*"

    (if (or (zerop (elfeed-db-last-update))
            (= (elfeed-queue-count-total) 0))
        (nano-modeline-button (nano-modeline-symbol 'mail-update)
                              #'elfeed-update
                              'active)
      (let* (;; (total (length elfeed-feeds))
             (total (if elfeed-use-curl
                        elfeed-curl-max-connections
                      url-queue-parallel-processes n))
             (in-process (- total (elfeed-queue-count-active)))
             (label (format " %d/%d" in-process total)))
        (nano-modeline-button
         (cons (concat (car (nano-modeline-symbol 'mail-update)) label) '(4 . 4))
         nil 'progress)))))

(defun nano-modeline-element-elfeed-search-filter ()
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
   'face 'nano-modeline-face-primary))

(defun nano-modeline-element-elfeed-search-count ()
  "Elfeed search statistics"
  (propertize (or (cond ((zerop (elfeed-db-last-update)) " ")
                        ((> (elfeed-queue-count-total) 0) " ")
                        (t (elfeed-search--count-unread))) " ")
   'face 'nano-modeline-face-secondary))


;; --- Mu4e -------------------------------------------------------------------
(defun nano-modeline-element-mu4e-message-status ()
  "Return a status for the message at point"

  (let* ((msg (mu4e-message-at-point))
         (docid (mu4e-message-field msg :docid))
         (mark (when docid
                 (with-current-buffer "*mu4e-headers*"
                   (gethash docid mu4e--mark-map)))))
    (nano-modeline-element-buffer-status nil
                 (when mark 'nano-modeline-face-buffer-marked))))

(defun nano-modeline-element-mu4e-message-tags ()
  "Return a status for the message at point"

  (let* ((msg (mu4e-message-at-point))
         (tags (mu4e-message-field msg :tags)))
    (when tags
      (mapconcat (lambda (tag)
                   (let ((tag (concat (car (nano-modeline-symbol 'mail-tag)) tag)))
                     (nano-modeline-button tag
                                           `(lambda () (mu4e-search ,(format "tag:%s" tag)))
                                           'active)))
                 tags
               (nano-modeline-element-half-space)))))


(defun nano-modeline-action-mu4e-update ()
  (message "The default mu4e update function does nothing. You need to re-write this function or replace the default action."))

(defun nano-modeline-action-mu4e-xwidget-view ()
  (interactive)
  (with-current-buffer "*mu4e-headers*"
    (mu4e-action-view-in-xwidget (mu4e-message-at-point))))

(defun nano-modeline-action-mu4e-next-context ()
  "Switch to next mu4e context"

  (let* ((current (mu4e-context-name (mu4e-context-current)))
         (contexts (mapcar (lambda (context)
                             (mu4e-context-name context))
                           mu4e-contexts))
         (index (mod (1+ (cl-position current contexts))
                     (length contexts)))
         (current (nth index contexts)))
    (mu4e-context-switch t current)))

(defun nano-modeline-action-mu4e-compose-next-context ()
  "Switch to next mu4e context"

  (let* ((current (mu4e-context-name (mu4e-context-current)))
         (contexts (mapcar (lambda (context)
                             (mu4e-context-name context))
                           mu4e-contexts))
         (index (mod (1+ (cl-position current contexts))
                     (length contexts)))
         (current (nth index contexts)))
    (mu4e-context-switch t current)
    (mu4e-compose-context-switch t current)))

(defun nano-modeline-action-mu4e-toggle-related ()
  "Toggle inclusion of related emails."

  (setq mu4e-search-include-related
        (not mu4e-search-include-related))
  (mu4e-search-rerun))

(defun nano-modeline-action-mu4e-toggle-threads ()
  "Toggle thread grouping mode."

  (setq mu4e-search-threads
        (not mu4e-search-threads))
  (mu4e-search-rerun))

(defun nano-modeline-action-mu4e-toggle-folding ()
  "Toggle thread folding mode (global)"

 (mu4e-thread-fold-toggle-all))

(defun nano-modeline-action-mu4e-sign ()
  "Sign email"
  (require 'mml)
  (mml-secure-message-sign))

(defun nano-modeline-action-mu4e-encrypt ()
  "Encrypt email"
  (require 'mml)
  (mml-secure-message-encrypt))

(defun nano-modeline-action-mu4e-attach ()
  "Attach file."
  (require 'mml)
  (mml-attach-file (mml-minibuffer-read-file "Attach file: ")))

(defun nano-modeline-action-mu4e-send ()
  "Send email and exit"
  (message-send-and-exit))

(defun nano-modeline-element-mu4e-last-query ()
  "Last search query"
    (propertize (mu4e-last-query) 'face 'nano-modeline-face-primary))

(defun nano-modeline-element-mu4e-message-to ()
  "Recipients of a message, separating me from others"

;;  (if (not (get-buffer "*mu4e-headers*"))
;;      "…"
;;    (with-current-buffer "*mu4e-headers*"
      (let* ((msg (mu4e-message-at-point))
             (list (memq 'list (plist-get msg :flags)))
             (cc (mapcar (lambda (item)
                           (downcase (plist-get item :email)))
                         (plist-get msg :cc)))
             (to (mapcar (lambda (item)
                           (downcase (plist-get item :email)))
                         (plist-get msg :to)))
             (to-names (mapcar (lambda (item)
                                 (if (stringp (plist-get item :name))
                                     (capitalize (downcase (plist-get item :name)))
                                   (plist-get item :email)))
                               (plist-get msg :to)))
             (all (cl-union to cc))
             (me (mapcar #'downcase (mu4e-personal-addresses)))
             (me (cl-intersection all me :test #'string-equal))
             (others (cl-set-difference all me :test #'string-equal)))
        (cond (list
               (concat "" (car to-names)))
              ((= (length others) 0)
               "me")
              ((and (> (length others) 0) (< (length others) (length all)))
               (format "me (+%d recipients)" (length others)))
              ((and (= (length others) 1))
               (format "%s" (car to-names)))
              (t
               (format "%s (+%d recipients)" (car to-names) (1- (length others)))))))
;;))

(defun nano-modeline-element-mu4e-message-from ()
  "Message sender"

;;  (if (not (get-buffer "*mu4e-headers*"))
;;      "Message"
;;    (with-current-buffer "*mu4e-headers*"
      (let* ((msg (mu4e-message-at-point))
             (me (mapcar #'downcase (mu4e-personal-addresses)))
             (from (mu4e-message-field msg :from))
             (from-name (plist-get (car from) :name))
             (from-email (plist-get (car from) :email)))
        (propertize
         (cond ((member from-email me) "Me")
               ((stringp from-name)    (capitalize (downcase from-name)))
               (t                      from-email))
         'face 'nano-modeline-face-primary)))
;;))

(defun nano-modeline-element-mu4e-message-subject ()
  "Message subject"

;;  (if (not (get-buffer "*mu4e-headers*"))
;;      "(none)"
;;    (with-current-buffer "*mu4e-headers*"
      (let* ((msg (mu4e-message-at-point))
             (subject (mu4e-message-field msg :subject)))
        (propertize (format "%s" subject)
                    'face 'nano-modeline-face-default)))
;;))

(defun nano-modeline-element-mu4e-compose-subject ()
  "Compose subject (live)"

  (if (not (derived-mode-p '(mu4e-compose-mode)))
      ""
      (save-excursion
        (message-position-on-field "Subject")
        (message-beginning-of-line)
        (if (eq (point) (line-beginning-position))
            (propertize "(no subject)"
                        'face 'nano-modeline-face-default)
          (propertize (buffer-substring (point) (line-end-position))
                      'face 'nano-modeline-face-primary)))))

(defun nano-modeline-element-mu4e-message-date ()
  "Message date"

  (if (not (get-buffer "*mu4e-headers*"))
      (propertize "No message available" 'face 'nano-modeline-face-secondary)
    (with-current-buffer "*mu4e-headers*"
      (let* ((msg (mu4e-message-at-point))
             (date (mu4e-message-field msg :date)))
        (propertize (format-time-string "%d %b %Y at %H:%M" date)
                    'face 'nano-modeline-face-secondary)))))


(defun nano-modeline-button-mu4e-context ()
  "Switch to next context."

  (let* ((context (mu4e-context-current))
         (name (if context (upcase (mu4e-context-name context))
                 "NONE")))
    (nano-modeline-button name
                          #'nano-modeline-action-mu4e-next-context
                          'active
                          "Click for next context")))

(defun nano-modeline-button-mu4e-compose-context ()
  "Switch to next context."

  (let* ((context (mu4e-context-current))
         (name (if context (upcase (mu4e-context-name context))
                 "NONE")))
    (nano-modeline-button name
                                #'nano-modeline-action-mu4e-compose-next-context
                                'active
                                "Click for next context")))

(defun nano-modeline-button-mu4e-xwidget ()
  "Show mai using xwidget"
  (nano-modeline-button (nano-modeline-symbol 'mail-html)
                                #'nano-modeline-action-mu4e-xwidget-view
                                'active))

(defun nano-modeline-button-mu4e-related ()
  "Toggle related mails"
  (nano-modeline-button "R"
                                #'nano-modeline-action-mu4e-toggle-related
                                (if mu4e-search-include-related
                                    'active 'inactive)))

(defun nano-modeline-button-mu4e-threads ()
  "Toggle threads view"
  (nano-modeline-button "T"
                                #'nano-modeline-action-mu4e-toggle-threads
                                (if mu4e-search-threads
                                    'active 'inactive)))

(defun nano-modeline-button-mu4e-folding ()
  "Toggle folding"
  (if mu4e-thread--fold-status
      (nano-modeline-button (nano-modeline-symbol 'mail-unfold)
                                    #'nano-modeline-action-mu4e-toggle-folding
                                    'active)
    (nano-modeline-button (nano-modeline-symbol 'mail-fold)
                                  #'nano-modeline-action-mu4e-toggle-folding
                                  'inactive)))

(defun nano-modeline-button-mu4e-update ()
  "Button to start an update (independently of the method). If a local
variable 'nano-modeline-mu4e-update-progress is defined, it will be
displayed in the button label.

User is responsible to define the overwrite the function
nano-modeline-action-mu4e-update that will be triggered when button is
pressed."

  (if (and (boundp 'nano-modeline-mu4e-update-progress)
           (stringp nano-modeline-mu4e-update-progress)
           (> (length nano-modeline-mu4e-update-progress) 0))

      (nano-modeline-button
       (cons (concat (car (nano-modeline-symbol 'mail-update))
                     nano-modeline-mu4e-update-progress)
             '(4 . 4))
       nil 'progress)
    (nano-modeline-button (nano-modeline-symbol 'mail-update)
                                  #'nano-modeline-action-mu4e-update
                                  'active)))

(defun nano-modeline-button-mu4e-attach ()
  (nano-modeline-button (nano-modeline-symbol 'mail-attach)
                                #'nano-modeline-action-mu4e-attach
                                'active
                                "Attach some files"))

(defun nano-modeline-button-mu4e-encrypt ()
  (nano-modeline-button (nano-modeline-symbol 'mail-encrypt)
                                #'nano-modeline-action-mu4e-encrypt
                                'active
                                "Encrypt email"))

(defun nano-modeline-button-mu4e-sign ()
  (nano-modeline-button (nano-modeline-symbol 'mail-sign)
                                #'nano-modeline-action-mu4e-sign
                                'active
                                "Sign email"))

(defun nano-modeline-button-mu4e-send ()
  (nano-modeline-button (nano-modeline-symbol 'mail-send)
                                #'nano-modeline-action-mu4e-send
                                'active
                                "Send email"))

;; --- GPTel -----------------------------------------------------------------
(defun nano-modeline-element-gptel-backend ()
  "gptel backend"
  (propertize
   (format "%s" (gptel-backend-name gptel-backend))
   'face 'nano-modeline-face-primary))

(defun nano-modeline-element-gptel-query-status ()
  "gptel query status (ready, waiting,typing)"
  (propertize
   (format "(%s)" (if (stringp mode-line-process)
                      (downcase
                       (string-trim
                        (substring-no-properties mode-line-process)))
                    "ready"))))

(defun nano-modeline-button-gptel-media ()
  "Toggle media use"
  (if (and (gptel--model-capable-p 'media) gptel-track-media)
      (nano-modeline-button "MEDIA"
                            #'nano-modeline-action-gptel-toggle-media
                            'active)
    (nano-modeline-button "NO MEDIA"
                          #'nano-modeline-action-gptel-toggle-media
                          'inactive)))

(defun nano-modeline-action-gptel-toggle-media ()
  "Toggle use of media"

  (setq-local gptel-track-media (not gptel-track-media))
  (if gptel-track-media
      (message
       (concat
        "Sending media from included links. To include media, create "
        "a \"standalone\" link in a paragraph by itself, separated from surrounding text."))
    (message "Ignoring image links. Only link text will be sent."))
  (force-mode-line-update))

(defun nano-modeline-button-gptel-context ()
  "Finalize the capture process."

  (let* ((context (or (car-safe (rassoc gptel--system-message gptel-directives))
                      (gptel--describe-directive gptel--system-message 15)))
         (prompt-re "I want you\\( to\\)* act as a\\(n\\)* \\([A-Za-z ]*\\).")
         (context (if (and (stringp gptel--system-message)
                           (string-match prompt-re gptel--system-message))
                      (match-string 3 gptel--system-message)
                    context)))
    (nano-modeline-button (upcase (format "%s" context))
                          #'gptel-system-prompt 'active "Set context")))

(defun nano-modeline-button-gptel-model ()
  "Finalize the capture process."

  (let ((model (gptel--model-name gptel-model)))
    (nano-modeline-button (string-trim (upcase (format" %s" model)))
                          #'gptel-menu 'active "Set model")))

(defun nano-modeline (&optional format position default)
  "Install a modeline described by FORMAT at the given POSITON. If
DEFAULT is true, this is made the default mode/header line."

  (interactive)
  (let* ((position (or position nano-modeline-position))
         (color nano-modeline-border-color)
         (format (or format nano-modeline-format-default))
         (face  `(:box (:color ,color :line-width (1 . 1))
                  :overline nil
                  :underline nil
                  :inherit nano-subtle))
         (face-relative  `(:box (:color ,color :line-width (0 . 1))
                           :overline nil
                           :underline nil
                          :inherit nano-subtle)))

    (face-remap-reset-base 'header-line)
    (face-remap-reset-base 'mode-line)

    ;; Install specific faces
    (if (not (eq position 'footer))
        (if default
            (progn
              (apply #'set-face-attribute 'header-line nil face))
          (progn
            (face-remap-add-relative 'header-line face-relative)))
      (if default
          (progn
            (apply #'set-face-attribute 'mode-line-active nil face)
            (apply #'set-face-attribute 'mode-line-inactive nil face))
        (progn
          (face-remap-add-relative 'mode-line-active face-relative)
          (face-remap-add-relative 'mode-line-inactive  face-relative))))

    ;; Instal modeline
    (if (not (eq position 'footer))
        (progn
          (setq header-line-format `(:eval (nano-modeline-make ',format)))
          (when default
              (setq-default mode-line-format nil)
              (setq-default header-line-format `(:eval (nano-modeline-make ',format)))))
      (progn
        (setq mode-line-format `(:eval (nano-modeline-make ',format)))
        (when default
          (setq-default header-line-format nil)
          (setq-default mode-line-format `(:eval (nano-modeline-make ',format))))))))



(provide 'nano-modeline)
;;; nano-modeline.el ends here
