(require 'scroll-all)

(defgroup translation nil
  "Minor mode for doing translation jobs."
  :prefix "translation-"
  :group 'editing)

(defvar translation-original-file-read-only t)
(defvar translation--auto-sync-paragraph-when-scrolling t)

(defvar translation--window-layout-config nil
  "Window layout saved into and restored from.")

(defun translation-sync-paragraph ()
  (let ((translation--auto-sync-paragraph-when-scrolling t))
    (translation--sync-paragraph)))

(defun translation--sync-paragraph ())

(defun translation-cursor-in-same-paragraph ())

(defun translation-page-down ()
  (scroll-all-page-down-all))
(defun translation-page-up ()
  (scroll-all-page-up-all))

(defun translation-goto-beginning-of-buffer ()
  (scroll-all-beginning-of-buffer-all))
(defun translation-goto-end-of-buffer ()
  (scroll-all-end-of-buffer-all))
(defun translation-goto-beginning-of-line ())
(defun translation-goto-end-of-line ())
(defun translation-goto-next-line ())
(defun translation-goto-prev-line ())
(defun translation-switch-buffer ())

(defun translation--init ()
  (setq translation--window-layout-config (current-window-configuration))
  (setq translation--buffer-original (find-file-noselect
                                      (read-file-name (format-prompt "Original: " "")))
        translation--buffer-translation (find-file-noselect
                                         (read-file-name (format-prompt "Translation: " ""))))
  (if translation-original-file-read-only
      (with-current-buffer translation--buffer-original
        (read-only-mode 1)))
  (delete-other-windows)
  (split-window-right-and-focus)
  (set-window-buffer (get-buffer-window) translation--buffer-translation)
  (set-window-buffer (next-window) translation--buffer-original)

  (with-current-buffer translation--buffer-translation
    (advice-add 'newline :after #'(lambda () (with-current-buffer translation--buffer-original
                                               (forward-paragraph))))))


(defun translation--restore-window-layout ()
  (set-window-configuration translation--window-layout-config))

(setq translation-mode-map (make-sparse-keymap))



(define-minor-mode translation-mode
  "Translation minor mode."
  :init-value nil
  :lighter "Trans"
  :group 'translation
  :keymap translation-mode-map
  :after-hook (if translation-mode
                  (translation--init)
                (translation--restore-window-layout)))
