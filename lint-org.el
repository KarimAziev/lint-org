;;; lint-org.el --- Lint multiple org files -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/lint-org
;; Version: 0.1.0
;; Keywords: outlines convenience
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Lint multiple org files

;;; Code:




(require 'org-lint)

(defvar lint-org-report-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'lint-org-jump-to-source)
    (define-key map (kbd "TAB") #'lint-org-show-source)
    (define-key map (kbd "C-j") #'lint-org-show-source)
    map)
  "Keymap for Org lint report interactions.")

(defun lint-org-show-source ()
  "Jump to the source code from an Org-mode lint message."
  (interactive)
  (lint-org-jump-to-source t))

(defun lint-org-jump-to-source (&optional no-select)
  "Jump to the source code from an Org-mode lint message.

Optional argument NO-SELECT is a boolean; when non-nil, the source buffer is not
selected after jumping."
  (interactive)
  (let* ((mk
          (get-text-property 0 'org-lint-marker (aref
                                                 (tabulated-list-get-entry) 0)))
         (buff (marker-buffer mk))
         (wnd (selected-window)))
    (with-current-buffer buff
      (let ((pos (marker-position mk)))
        (unless (<= (point-min) pos (point-max))
          (widen))
        (goto-char pos)
        (org-fold-show-set-visibility 'local)
        (if-let ((buff-wnd (get-buffer-window buff)))
            (select-window buff-wnd)
          (select-window (or (car (delq nil
                                        (mapcar (lambda (fn)
                                                  (funcall fn wnd))
                                                '(window-right window-left))))
                             (split-window-sensibly)))
          (pop-to-buffer-same-window buff))
        (dolist (wnd (get-buffer-window-list buff nil t))
          (set-window-point wnd pos))
        (recenter)
        (pulse-momentary-highlight-one-line)))
    (when no-select
      (select-window wnd))))

(defun lint-org-generate-reports (buffer checkers)
  "Generate lint reports for an Org buffer.

Argument BUFFER is the buffer in which to generate reports.

Argument CHECKERS is a list of checker functions to use for generating reports."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((ast (org-element-parse-buffer nil nil 'defer))
            (name (buffer-name))
            (id 0)
            (last-line 1)
            (last-pos 1))
        (mapcar
         (lambda (report)
           (list
            (format "%s-%s" name (cl-incf id))
            (apply #'vector
                   (cons
                    (progn
                      (goto-char (car report))
                      (forward-line 0)
                      (prog1 (propertize
                              (number-to-string
                               (cl-incf last-line
                                        (count-lines last-pos (point))))
                              'org-lint-marker (car report))
                        (setf last-pos (point))))
                    (cdr report)))))
         (sort (cl-mapcan
                (lambda (c)
                  (let ((trust (symbol-name (org-lint-checker-trust c))))
                    (mapcar
                     (lambda (report)
                       (list (copy-marker (car report)) name trust
                             (nth 1 report) c))
                     (save-excursion
                       (funcall (org-lint-checker-function c)
                                ast)))))
                checkers)
               #'car-less-than-car))))))
(defun lint-org-find-report-buffer ()
  "Find and return the buffer in `lint-org-report-mode'."
  (seq-find
   (lambda (it)
     (eq 'lint-org-report-mode
         (buffer-local-value 'major-mode it)))
   (buffer-list)))

(defmacro lint-org-with-report-buffer (&rest body)
  "Check and edit lint report in a dedicated buffer.

Remaining arguments BODY are forms that are evaluated with the current buffer
set to the report buffer found by `lint-org-find-report-buffer'."
  (let ((buffer (make-symbol "buffer")))
    `(when-let ((,buffer (lint-org-find-report-buffer)))
      (with-current-buffer ,buffer
       (progn ,@body)))))

(defun lint-org--refresh-buffer (buffer-or-name)
  "Refresh lint reports for a given buffer.

Argument BUFFER-OR-NAME is either a buffer or the name of an existing buffer to
be refreshed."
  (when-let ((buff (if (bufferp buffer-or-name)
                       buffer-or-name
                     (get-buffer buffer-or-name))))
    (when (buffer-live-p buff)
      (let ((report
             (lint-org-generate-reports buff
                                              org-lint--checkers)))
        (lint-org-with-report-buffer
         (setq tabulated-list-entries
               (seq-remove (lambda (it)
                             (equal buffer-or-name
                                    (aref (cadr it) 1)))
                           tabulated-list-entries))
         (setq tabulated-list-entries
               (append report tabulated-list-entries))
         (tabulated-list-print t))))))

(defun lint-org--refresh-current-buffer ()
  "Refresh lint reports for the current buffer."
  (lint-org--refresh-buffer (buffer-name (current-buffer))))

(defun lint-org--render (reports)
  "Render lint REPORTS in associated Org buffers.

Argument REPORTS is a list of report entries to be rendered."
  (let* ((entries (seq-filter (lambda (it)
                                (buffer-live-p
                                 (get-buffer (aref (cadr it) 1))))
                              reports))
         (buffers (seq-uniq (mapcar (lambda (it)
                                      (get-buffer (aref (cadr it) 1)))
                                    entries)
                            'eq)))
    (dolist (buff buffers)
      (with-current-buffer buff
        (add-hook 'after-save-hook
                  #'lint-org--refresh-current-buffer nil t)))
    (lint-org-with-report-buffer
     (setq tabulated-list-entries
           entries)
     (tabulated-list-print t))))

(defun lint-org--refresh-reports ()
  "Refresh lint reports for Org buffers."
  (let ((buffs (seq-uniq
                (mapcar (lambda (it)
                          (aref (cadr it) 1))
                        tabulated-list-entries)))
        (reports))
    (dolist-with-progress-reporter (buff-name buffs)
        (format "Processing %s" (length buffs))
      (when-let ((buff (get-buffer buff-name)))
        (when (buffer-live-p buff)
          (when-let ((report
                      (lint-org-generate-reports buff
                                                       org-lint--checkers)))
            (setq reports (append reports report))))))
    (lint-org--render reports)))

;;;###autoload
(defun lint-org (&optional recursively)
  "Check and report issues in Org files in DIRECTORY RECURSIVELY or not.

Optional argument RECURSIVELY is a prefix argument; if non-nil, `lint-org' will
operate on all .org files in `org-directory' and its subdirectories recursively."
  (interactive "P")
  (sit-for 0.5)
  (require 'org)
  (let ((reports)
        (files (if recursively
                   (directory-files-recursively org-directory
                                                "\\.org\\'")
                 (directory-files org-directory t "\\.org\\'" t))))
    (dolist-with-progress-reporter (file files)
        (format "Linting %s files" (length files))
      (when (file-exists-p file)
        (delay-mode-hooks
          (let ((buff (or (get-file-buffer file)
                          (let ((inhibit-message t))
                            (find-file-noselect file t)))))
            (when-let ((report
                        (lint-org-generate-reports buff
                                                   org-lint--checkers)))
              (setq reports (append reports report)))))))
    (when reports
      (let ((buffer (get-buffer-create
                     "*Org Lint Batch*")))
        (with-current-buffer buffer
          (lint-org-report-mode)
          (lint-org--render reports))
        (pop-to-buffer buffer)))))

(define-derived-mode lint-org-report-mode tabulated-list-mode "Org+Lint"
  "Display Org lint reports in a tabulated list.

Define a major mode for viewing lint reports of Org files in a tabulated list
format. Display line numbers, file names, trust levels, and warning messages.
Sort entries by line number and allow refreshing of lint reports."
  (setf tabulated-list-format
        `[("Line" 6
           (lambda (a b)
             (< (string-to-number (aref (cadr a) 0))
              (string-to-number (aref (cadr b) 0))))
           :right-align t)
          ("File" 10 t)
          ("Trust" 5 t)
          ("Warning" 0 t)])
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook #'lint-org--refresh-reports nil t))

(provide 'lint-org)
;;; lint-org.el ends here