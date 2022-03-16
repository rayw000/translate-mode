(require 'master)

(defgroup translate nil
  "Doing translate jobs."
  :prefix "translate-"
  :group 'editing)

(defvar translate--window-layout-config (current-window-configuration))
(defvar translate-enable-highlight t)

(defun translate--restore-window-layout ()
  (set-window-configuration translate--window-layout-config))

(defvar translate-previous-line-function 'previous-line)
(defvar translate-next-line-function 'next-line)
(defvar translate-scroll-up-function 'scroll-up)
(defvar translate-scroll-down-function 'scroll-down)
(defvar translate-forward-paragraph-function 'forward-paragraph)
(defvar translate-backward-paragraph-function 'backward-paragraph)
(defvar translate-beginning-of-buffer-function 'beginning-of-buffer)
(defvar translate-end-of-buffer-function 'end-of-buffer)
(defvar translated-newline-function 'markdown-enter-key)
(defvar translate-recenter-function 'recenter)

(defun translate-toggle-highlight ()
  (interactive)
  (if translate-enable-highlight
      (translate--clear-highlighting)
    (progn
      (translate--highlight-paragraph-overlay-at-point)
      (master-says 'translate--highlight-paragraph-overlay-at-point)))
  (setq translate-enable-highlight (not translate-enable-highlight)))

(defun translate--redraw-highlighting ()
  (when translate-enable-highlight
    (translate--highlight-paragraph-overlay-at-point)
    (master-says 'translate--highlight-paragraph-overlay-at-point)))

(defun translate--clear-highlighting ()
  (remove-overlays)
  (master-says 'remove-overlays))

(defun translate--master-slave-call (func &optional arg)
  (call-interactively func)
  (master-says func)
  (translate--redraw-highlighting))

(defun translate-previous-line ()
  "Backward line in both buffers."
  (interactive)
  (translate--master-slave-call translate-previous-line-function))

(defun translate-next-line ()
  "Forward line in both buffers."
  (interactive)
  (translate--master-slave-call translate-next-line-function))

(defun translate-scroll-up ()
  "Scroll up in both buffers."
  (interactive)
  (translate--master-slave-call translate-scroll-up-function))

(defun translate-scroll-down ()
  "Scroll down in both buffers."
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

(defun translate-recenter ()
  "Recenter in both buffers."
  (interactive)
  (translate--master-slave-call translate-recenter-function))

(defun translate-newline ()
  "Do something on newline action."
  (interactive)
  (translate--redraw-highlighting)
  (call-interactively translate-sync-paragraph-to-current-pos))

(defun translate-sync-paragraph-to-current-pos ()
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
    (translate--redraw-highlighting)
    (message "Sync to paragraph %s" (1+ i))))

(defun translate--highlight-paragraph-overlay-at-point ()
  (save-excursion
    (remove-overlays)
    (let ((beg (progn (call-interactively translate-backward-paragraph-function 1)
                      (while (and (not (eobp))
                                  (looking-at "^$"))
                        (forward-line))
                      (point)))
          (end (progn (call-interactively translate-forward-paragraph-function 1)
                      (point))))
      (overlay-put (make-overlay beg end) 'face '(:background "grey20")))))

(defun translate-open-original-file ()
  (interactive)
  (let ((buffer (find-file-noselect
                 (read-file-name (format-prompt "Open original file for translatin: " "")))))
    (master-set-slave buffer)
    buffer))

(defun translate-init ()
  (interactive)
  (let ((translate-buffer (current-buffer))
        (original-buffer (translate-open-original-file)))
    (delete-other-windows)
    (split-window-right-and-focus)
    (set-window-buffer (get-buffer-window) translate-buffer)
    (set-window-buffer (next-window) original-buffer)
    (master-mode 1)
    (master-set-slave original-buffer)
    (translate-mode 1)))

(defun translate-cleanup ()
  (interactive)
  (unless translate-mode
    (translate--restore-window-layout)
    (master-mode -1)))

(add-hook 'translate-mode-hook 'translate-cleanup)

(defvar translate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap previous-line] #'translate-previous-line)
    (define-key map [remap next-line] #'translate-next-line)
    (define-key map [remap scroll-up] #'translate-scroll-up)
    (define-key map [remap scroll-down] #'translate-scroll-down)
    (define-key map [remap forward-paragraph] #'translate-forward-paragraph)
    (define-key map [remap backward-paragraph] #'translate-backward-paragraph)
    (define-key map [remap beginning-of-buffer] #'translate-beginning-of-buffer)
    (define-key map [remap end-of-buffer] #'translate-end-of-buffer)
    (define-key map [remap recenter] #'translate-recenter)
    map)
  "Keymap for `translate-mode' buffers.")

(define-minor-mode translate-mode
  "Toggle translate mode."
  :lighter "Tr"
  :init-value nil
  :keymap translate-mode-map
  :group 'translate)
