;;; translate-mode.el --- Paragraph-oriented side-by-side doc translation workflow -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ray Wang <rayw.public@gmail.com>

;; Author: Ray Wang <rayw.public@gmail.com>
;; Package-Requires: ((emacs "24.3"))
;; Version: 0
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
;; Paragraph-oriented minor mode for side-by-side document translation workflow

;; Usage:
;;
;;   Open the translation file you are working with, and run command
;;
;;     (translate-select-reference-buffer)
;;
;;   to select an existed reference buffer, or
;;
;;     (translate-open-reference-file)
;;
;;   to open a reference file.
;;
;;   See: https://github.com/rayw000/translate-mode for more details.

;;; Code:

(require 'master)
(require 'pulse)
(require 'cl-lib)

(defgroup translate nil
  "Minor mode for doing translation jobs."
  :prefix "translate-"
  :group 'editing)

(defface translate-paragraph-highlight-face
  '((t :background "grey15" :extend t))
  "Default face for highlighting the current paragraph in `translate-mode'."
  :group 'translate)

(defvar translate-enable-highlight t
  "Enable highlighting if non-nil.")
(defvar translate-reference-buffer-read-only t
  "Make reference buffer read-only if non-nil.")

(defvar translate--window-layout-config (current-window-configuration))
(defvar translate--overlay nil
  "Will be used as a buffer local variable.")

(defun translate--restore-window-layout ()
  "Save window layout for later restoring."
  (set-window-configuration translate--window-layout-config))

(defvar translate-previous-line-function 'previous-line
  "Previous line function. Default to 'previous-line.")
(defvar translate-next-line-function 'next-line
  "Next line function. Default to 'next-line.")
(defvar translate-scroll-up-function (if (bound-and-true-p cua-mode)
                                         'cua-scroll-up
                                       'scroll-up-command)
  "Scroll up function. Default to 'scroll-up-command.")
(defvar translate-scroll-down-function (if (bound-and-true-p cua-mode)
                                           'cua-scroll-down
                                         'scroll-down-command)
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
(defvar translate-recenter-top-bottom-function 'recenter-top-bottom
  "Recenter top and bottom function. Default to 'recenter-top-bottom.")

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
  (delete-overlay translate--overlay)
  (when (and (boundp 'master-of)
             (bufferp master-of))
    (with-current-buffer master-of
      (delete-overlay translate--overlay))))

(defun translate--master-slave-call (func &optional args)
  "Call FUNC both in the translation buffer and the reference buffer with ARGS.

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
  "Recenter in both buffers.

ARG is the argument to pass to `translate-recenter-function'."
  (interactive)
  (translate--master-slave-call translate-recenter-function arg)
  (translate-sync-cursor-to-current-paragraph))

(defun translate-recenter-top-bottom (&optional arg)
  "Recenter in both buffers using `recenter-top-bottom'.
ARG is the argument to pass to `recenter-top-bottom' in each window."
  (interactive)
  (with-current-buffer (get-buffer master-of)
    ;; make `recenter-last-op' local so that it has the same value after
    ;; being called in each window
    (make-local-variable 'recenter-last-op))
  (translate-sync-cursor-to-current-paragraph)
  (translate--master-slave-call translate-recenter-top-bottom-function arg)
  (master-says 'translate--pulse-overlay))

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
  "Move cursor in the reference buffer to the same n-th paragraph as translation buffer."
  (interactive)
  (let ((i 0)
        (point (progn (while (looking-at "^$")
                        (translate-previous-line))
                      (point))))
    (save-excursion (goto-char (point-min))
                    (while (and (not (eobp))
                                (< (point) point))
                      (call-interactively translate-forward-paragraph-function)
                      (setq i (1+ i))))
    (master-says translate-beginning-of-buffer-function)
    (master-says translate-forward-paragraph-function (list i))
    (master-says translate-backward-paragraph-function)
    (master-says translate-recenter-function)
    (translate--redraw-highlighting)
    (message "Sync to paragraph %s" i)))

(defun translate--get-overlay-at-point ()
  "Get the paragraph the point belongs to as an overlay."
  (save-excursion
    (let ((pair (translate--get-paragraph-beg-end-at-point)))
      (if translate--overlay
          (move-overlay translate--overlay (car pair) (cdr pair))
        (setq-local translate--overlay (make-overlay (car pair) (cdr pair)))))))

(defun translate--get-paragraph-beg-end-at-point ()
  "Get the begin and end positions of a paragraph."
  (let* ((beg (cond ((not (looking-at "^$"))
                     (save-excursion
                       (forward-char)
                       (call-interactively translate-backward-paragraph-function)
                       (point)))
                    (t (point))))
         (end (cond ((not (looking-at "^$"))
                     (save-excursion
                       (call-interactively translate-forward-paragraph-function)
                       (point)))
                    (t beg))))
    (cons beg end)))

(defun translate--highlight-paragraph-overlay-at-point ()
  "Highligh overlay at point."
  (overlay-put (translate--get-overlay-at-point) 'face 'translate-paragraph-highlight-face))

;;;###autoload
(defun translate-get-reference-paragraph-text-at-point ()
  "Get text of the paragraph at point in the reference buffer."
  (if master-of
      (with-current-buffer (get-buffer master-of)
        (let ((pair (translate--get-paragraph-beg-end-at-point)))
          (buffer-substring-no-properties (car pair) (cdr pair))))
    (error "You don't have a reference buffer. See `translate-select-reference-buffer' or `translate-open-reference-file'")))

(defun translate--prepare-window-layout-and-set-buffer (buffer)
  "Prepare window layout and set the new created buffer into windows.

BUFFER is the newly created buffer which is supposed to be set to the new window."
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (set-window-buffer (next-window) buffer)
  (master-mode 1)
  (master-set-slave buffer)
  (translate--toggle-reference-mode 1)
  (with-current-buffer buffer
    (when translate-reference-buffer-read-only
      (read-only-mode 1)))
  (translate-mode 1))

;;;###autoload
(defun translate-open-reference-file (&optional filename)
  "Prompt to open the file FILENAME and set it as the reference buffer."
  (interactive)
  (let ((buffer (find-file-noselect
                 (or filename
                     (read-file-name "Open reference file: ")))))
    (translate--prepare-window-layout-and-set-buffer buffer)
    buffer))

;;;###autoload
(defun translate-select-reference-buffer (&optional buf)
  "Prompt to select the reference buffer BUF."
  (interactive)
  (let ((buffer (or buf
                    (completing-read
                     "Select reference buffer: "
                     (cl-map 'list 'buffer-name (buffer-list)) nil t ""))))
    (translate--prepare-window-layout-and-set-buffer buffer)
    buffer))

(defun translate--toggle-reference-mode (&optional arg)
  "Toggle `translate-reference-mode' from translation buffer.

ARG will be directly passed to `translate-reference-mode'."
  (master-says 'translate-reference-mode (list arg)))

(defun translate-cleanup ()
  "Clear highlightings and disable master mode."
  (ignore-errors
    (translate--clear-highlighting)
    (translate--toggle-reference-mode -1))
  (master-mode -1))

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
    (define-key map [remap recenter-top-bottom] #'translate-recenter-top-bottom)
    map)
  "Keymap for `translate-mode' buffers.")

(defvar translate-reference-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `translate-reference-mode' buffers.")

;;;###autoload
(define-minor-mode translate-mode
  "Minor mode for translation buffer."
  :lighter " Tr"
  :init-value nil
  :keymap translate-mode-map
  :group 'translate
  :after-hook (if translate-mode
                  (progn
                    (make-local-variable 'translate--overlay)
                    (setq-local translate--overlay (make-overlay 0 0 (current-buffer)))
                    (when (bound-and-true-p cua-mode)
                      ;; FIXME: these are hardcoded because remapping
                      ;; cua-scroll-up/down has no effect, including in
                      ;; cua-global-keymap and cua--cua-keys-keymamp
                      (define-key translate-mode-map (kbd "<next>") #'translate-scroll-up)
                      (define-key translate-mode-map (kbd "<prior>") #'translate-scroll-down)))
                (translate-cleanup)))

;;;###autoload
(define-minor-mode translate-reference-mode
  "Minor mode for artcle referring buffer."
  :lighter " TrR"
  :init-value nil
  :keymap translate-reference-mode-map
  :group 'translate
  :after-hook (when translate-reference-mode
                (make-local-variable 'translate--overlay)
                (setq-local translate--overlay (make-overlay 0 0 (current-buffer)))))

(provide 'translate-mode)

;;; translate-mode.el ends here
