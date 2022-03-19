;;; translate-mode.el --- Paragraph-oriented minor mode for doing translation jobs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ray Wang <rayw.public@gmail.com>

;; Author: Ray Wang <rayw.public@gmail.com>
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0
;; Keywords: translate, convenience, editing
;; URL: https://github.com/rayw000/translate-mode

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; A useful minor mode for doing translation jobs.

;; Usage:
;;
;;   Open the translating file you are working with, and run command
;;
;;     (translate-init)
;;
;;   You can also enable `translate-mode' in your working buffer first, and then use command
;;
;;     (translate-open-original-file)
;;
;;   or
;;
;;     (translate-select-original-buffer)
;;
;;   to setup a buffer for referring the original article.
;;
;;   See: https://github.com/rayw000/translate-mode for more details.

;;; Code:

(require 'master)
(require 'pulse)
(require 'cl-lib)

;;; Code:

(defgroup translate nil
  "Minor mode for doing translation jobs."
  :prefix "translate-"
  :group 'editing)

(defface translate-paragraph-highlight
  '((t :background "grey15" :extend t))
  "Default face for highlighting the current paragraph in `translate-mode'."
  :group 'translate)

(defvar translate-enable-highlight t
  "Enable highlighting if non-nil.")
(defvar translate-original-buffer-read-only t
  "Make original buffer read-only if non-nil.")

(defvar translate--window-layout-config (current-window-configuration))

(defun translate--restore-window-layout ()
  "Save window layout for later restoring."
  (set-window-configuration translate--window-layout-config))

(defvar translate-previous-line-function 'previous-line
  "Previous line function. Default to 'previous-line.")
(defvar translate-next-line-function 'next-line
  "Next line function. Default to 'next-line.")
(defvar translate-scroll-up-function 'scroll-up-command
  "Scroll up function. Default to 'scroll-up-command.")
(defvar translate-scroll-down-function 'scroll-down-command
  "Scroll down function. Default to 'scroll-down-command.")
(defvar translate-forward-paragraph-function 'forward-paragraph
  "Forward paragraph function. Default to 'forward-paragraph.")
(defvar translate-backward-paragraph-function 'backward-paragraph
  "Backward paragraph function. Default to 'backward-paragraph.")
(defvar translate-beginning-of-buffer-function 'beginning-of-buffer
  "Beginning of buffer function. Default to 'beginning-of-buffer.")
(defvar translate-end-of-buffer-function 'end-of-buffer
  "End of buffer function. Default to 'end-of-buffer.")
(defvar translated-newline-function 'newline
  "Newline function. Default to 'newline.")
(defvar translate-recenter-function 'recenter
  "Recenter function. Default to 'recenter.")

(defun translate-toggle-highlight ()
  "Toggle paragraph highlighting."
  (interactive)
  (if translate-enable-highlight
      (translate--clear-highlighting)
    (progn
      (translate--highlight-paragraph-overlay-at-point)
      (master-says 'translate--highlight-paragraph-overlay-at-point)))
  (setq translate-enable-highlight (not translate-enable-highlight)))

(defun translate--redraw-highlighting ()
  "Redraw highlight overlays."
  (when translate-enable-highlight
    (translate--highlight-paragraph-overlay-at-point)
    (master-says 'translate--highlight-paragraph-overlay-at-point)))

(defun translate--clear-highlighting ()
  "Clear highlight overlays."
  (remove-overlays)
  (master-says 'remove-overlays))

(defun translate--master-slave-call (func &optional args)
  "Call FUNC both in translation buffer and original buffer with ARGS.

 And redraw highlightings."
  (call-interactively func)
  (master-says func args)
  (translate--redraw-highlighting))

(defun translate-previous-line (&optional arg)
  "Backward ARGS lines in both buffers.

ARG defaults to 1."
  (interactive "p")
  (translate--master-slave-call translate-previous-line-function (list arg)))

(defun translate-next-line (&optional arg)
  "Forward ARGS lines in both buffers.

ARG defaults to 1."
  (interactive "p")
  (translate--master-slave-call translate-next-line-function (list arg)))

(defun translate-scroll-up ()
  "Scroll upward in both buffers."
  (interactive)
  (translate--master-slave-call translate-scroll-up-function))

(defun translate-scroll-down ()
  "Scroll downward in both buffers."
  (interactive)
  (translate--master-slave-call translate-scroll-down-function))

(defun translate-forward-paragraph ()
  "Forward paragraph in both buffers."
  (interactive)
  (translate--master-slave-call translate-forward-paragraph-function))

(defun translate-backward-paragraph ()
  "Backward paragraph in both buffers."
  (interactive)
  (translate--master-slave-call translate-backward-paragraph-function))

(defun translate-beginning-of-buffer ()
  "Go to beginning of buffer in both buffers."
  (interactive)
  (translate--master-slave-call translate-beginning-of-buffer-function))

(defun translate-end-of-buffer ()
  "Go to end of buffer in both buffers."
  (interactive)
  (translate--master-slave-call translate-end-of-buffer-function))

(defun translate-recenter (&optional arg)
  "Recenter in both buffers."
  (interactive)
  (translate--master-slave-call translate-recenter-function arg))

(defun translate-newline ()
  "Do something on newline action."
  (interactive)
  (translate--redraw-highlighting)
  (call-interactively #'translate-sync-cursor-to-current-paragraph))

(defun translate--pulse-overlay ()
  "Blink overlay at point."
  (interactive)
  (pulse-momentary-highlight-overlay (translate--get-overlay-at-point)))

(defun translate-sync-cursor-to-current-paragraph ()
  "Move cursor in original buffer to the same n-th paragraph as translation buffer."
  (interactive)
  (let ((i 0)
        (point (point)))
    (save-excursion (goto-char (point-min))
                    (while (and (not (eobp))
                                (< (point) point))
                      (call-interactively translate-forward-paragraph-function)
                      (setq i (1+ i))))
    (master-says translate-beginning-of-buffer-function)
    (master-says translate-forward-paragraph-function (list i))
    (master-says translate-recenter-function)
    (translate--redraw-highlighting)
    (master-says 'translate--pulse-overlay)
    (message "Sync to paragraph %s" (1+ i))))

(defun translate--get-overlay-at-point ()
  "Get the paragraph the point belongs to as an overlay."
  (save-excursion
    (let ((beg (progn (call-interactively translate-backward-paragraph-function 1)
                      (while (and (not (eobp))
                                  (looking-at "^$"))
                        (forward-line))
                      (point)))
          (end (progn (call-interactively translate-forward-paragraph-function 1)
                      (point))))
      (make-overlay beg end))))

(defun translate--highlight-paragraph-overlay-at-point ()
  "Highligh overlay at point."
  (remove-overlays)
  (overlay-put (translate--get-overlay-at-point) 'face 'translate-paragraph-highlight))

(defun translate-open-original-file ()
  "Prompt to open a file and set it as the original buffer for translation referring."
  (interactive)
  (let ((buffer (find-file-noselect
                 (read-file-name (format-prompt "Open original file for translatin: " "")))))
    (set-window-buffer (next-window) buffer)
    (when translate-original-buffer-read-only
      (with-current-buffer buffer
        (read-only-mode 1)))
    (master-set-slave buffer)
    buffer))

(defun translate-select-original-buffer ()
  "Prompt to select the original buffer for referring."
  (interactive)
  (let ((buffer (completing-read
                 "Select original buffer: "
                 (cl-map 'list 'buffer-name (buffer-list)) nil t "")))
    (set-window-buffer (next-window) buffer)
    (when translate-original-buffer-read-only
      (with-current-buffer buffer
        (read-only-mode 1)))
    (master-set-slave buffer)
    buffer))

(defun translate-init ()
  "Initialize `translate-mode'.

It's optional unless you want to be prompted to open origianl file
 before `translate-mode' is enabled."
  (interactive)
  (let ((translate-buffer (current-buffer))
        (original-buffer (translate-open-original-file)))
    (delete-other-windows)
    (split-window-right)
    (windmove-right)
    (set-window-buffer (get-buffer-window) translate-buffer)
    (set-window-buffer (next-window) original-buffer)
    (master-mode 1)
    (master-set-slave original-buffer)
    (translate-mode 1)))

(defun translate-cleanup ()
  "Restore the window layout and disable `master-mode'."
  (unless translate-mode
    (translate--restore-window-layout)
    (master-mode -1)))

(add-hook 'translate-mode-hook 'translate-cleanup)

(defvar translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap previous-line] #'translate-previous-line)
    (define-key map [remap next-line] #'translate-next-line)
    (define-key map [remap scroll-up-command] #'translate-scroll-up)
    (define-key map [remap scroll-down-command] #'translate-scroll-down)
    (define-key map [remap forward-paragraph] #'translate-forward-paragraph)
    (define-key map [remap backward-paragraph] #'translate-backward-paragraph)
    (define-key map [remap beginning-of-buffer] #'translate-beginning-of-buffer)
    (define-key map [remap end-of-buffer] #'translate-end-of-buffer)
    (define-key map [remap recenter-top-bottom] #'translate-recenter)
    map)
  "Keymap for `translate-mode' buffers.")

;;;###autoload
(define-minor-mode translate-mode
  "Toggle translate mode."
  :lighter "Tr"
  :init-value nil
  :keymap translate-mode-map
  :group 'translate)

(provide 'translate-mode)

;;; translate-mode.el ends here
