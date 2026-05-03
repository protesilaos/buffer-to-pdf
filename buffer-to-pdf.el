;;; buffer-to-pdf.el --- Create a PDF out of your current buffer, exactly as you see it -*- lexical-binding:t -*-

;; Copyright (C) 2026  Protesilaos

;; Author: Protesilaos <info@protesilaos.com>
;; Maintainer: Protesilaos <info@protesilaos.com>
;; URL: https://github.com/protesilaos/buffer-to-pdf
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Create a PDF out of your current buffer, exactly as you see it.
;; This feature is only available to Emacs builds that have support
;; for Cairo, per the function `x-export-frames'.
;;
;; To produce a document, go to a buffer and invoke the command
;; `buffer-to-pdf'.  It will prompt you for an orientation among
;; `buffer-to-pdf-orientations'.  Then it will generate the document
;; relative to the `buffer-to-pdf-directory'.
;;
;; Advanced users can modify the `buffer-to-pdf-local-variables',
;; `buffer-to-pdf-common-frame-parameters'.  I am not exposing them as
;; user options because I think it is too early to do so.
;;
;; The documentation of `buffer-to-pdf' describes the available export
;; methods in their order of precedence.
;;
;; I learnt about the existence of `x-export-frames' from Amin
;; Bandali, to whom I am thankful: <https://kelar.org/~bandali/>.
;;
;; And, as is the norm with my packages, there is a backronym for
;; `buffer-to-pdf': Bewitched Users Find Files Effortlessly Rendered
;; To PDF.  Enjoy!

;;; Code:

(defgroup buffer-to-pdf nil
  "Create a PDF out of your current buffer, exactly as you see it."
  :group 'convenience)

(defcustom buffer-to-pdf-directory (expand-file-name "~/")
  "The directory where PDF files are saved at."
  :type 'directory
  :package-version '(buffer-to-pdf . "0.1.0"))

(defvar buffer-to-pdf-common-frame-parameters
  '((no-focus-on-map . t)
    (no-accept-focus . t)
    (undecorated . t)
    (internal-border-width . 60)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (tab-bar-lines . 0))
  "Common frame parameters to affect the resulting PDF.")

(defconst buffer-to-pdf-irriducible-frame-parameters
  '((visibility . t))
  "Like `buffer-to-pdf-common-frame-parameters' but essential.")

(defvar buffer-to-pdf-local-variables
  '((mode-line-format . nil)
    (cursor-type . nil))
  "Buffer-local variables and their values to affect the resulting PDF.
Each element is a cons cell of the form (SYMBOL . VALUE) where SYMBOL is
the name of the variable and VALUE is what it should be bound to.")

(defvar buffer-to-pdf-local-modes
  '((display-line-numbers-mode . -1)
    (flyspell-mode . -1)
    (flymake-mode . -1)
    (show-paren-mode . -1)
    (hl-line-mode . -1)
    (tab-line-mode . -1))
  "Local minor modes and their values to affect the resulting PDF.
Each element is a cons cell of the form (SYMBOL . VALUE) where SYMBOL is
the name of the minor mode function and VALUE is what it should be
called with (e.g. 1 and -1).")

(defun buffer-to-pdf--get-window-orientation ()
  "Return frame parameters based on the width and height of the current window."
  `((width . (text-pixels . ,(window-pixel-width)))
    (height . (text-pixels . ,(window-pixel-height)))))

(defvar buffer-to-pdf-orientations
  '((landscape . ((width . (text-pixels . 960))
                  (height . (text-pixels . 720))))
    (portrait . ((width . (text-pixels . 720))
                 (height . (text-pixels . 960))))
    (current-window . buffer-to-pdf--get-window-orientation))
  "Frame layouts to affect the dimensions of the resulting PDF.
Each element is a cons cell of the form (SYMBOL . PARAMETERS) where
SYMBOL describes the element and PARAMETERS is an alist of frame
parameters (such as those returned by `frame-parameters').  PARAMETERS
may also be a function that returns frame parameters.

These are merged with `buffer-to-pdf-common-frame-parameters' and
`buffer-to-pdf-irriducible-frame-parameters'.")

(defun buffer-to-pdf--get-orientation-parameters (orientation)
  "Resolve ORIENTATION to an alist of frame parameters.
ORIENTATION is a symbol from `buffer-to-pdf-orientations' or an alist."
  (let ((parameters (alist-get orientation buffer-to-pdf-orientations)))
    (if (functionp parameters)
        (funcall parameters)
      parameters)))

(defun buffer-to-pdf--make-frame (orientation)
  "Make a frame with the ORIENTATION parameters.
Merge ORIENTATION parameters with `buffer-to-pdf-common-frame-parameters'."
  (if-let* ((parameters (if (symbolp orientation)
                            (buffer-to-pdf--get-orientation-parameters orientation)
                          orientation))
            (all-parameters (append
                             parameters
                             buffer-to-pdf-common-frame-parameters
                             buffer-to-pdf-irriducible-frame-parameters)))
      (let ((default-frame-alist nil)
            (frame-resize-pixelwise t))
        (make-frame all-parameters))
    (error "Cannot determine frame parameters")))

(defvar buffer-to-pdf--frames nil
  "List of frames created during export.")

(defvar buffer-to-pdf--indirect-buffers nil
  "List of indirect buffers created during export.")

(defun buffer-to-pdf--setup-buffer-state ()
  "Apply `buffer-to-pdf-local-variables' and `buffer-to-pdf-local-modes'."
  (pcase-dolist (`(,variable . ,value) buffer-to-pdf-local-variables)
    ;; NOTE 2026-05-01: Emacs 31 has `set-local', which does
    ;; exactly what I have here, but I do not want to depend on
    ;; such a new version just for this tiny thing.
    (set (make-local-variable variable) value))
  (pcase-dolist (`(,mode . ,value) buffer-to-pdf-local-modes)
    (when (fboundp mode)
      (funcall mode value))))

(defun buffer-to-pdf--create-page (orientation &optional beg end)
  "Create a frame and indirect buffer for a PDF page.
Use ORIENTATION for the frame dimensions.
If BEG and END are provided, narrow the buffer to that region.
If only BEG is provided, move point to BEG and set window start."
  (let ((frame (buffer-to-pdf--make-frame orientation))
        (buffer (clone-indirect-buffer nil nil)))
    (push frame buffer-to-pdf--frames)
    (push buffer buffer-to-pdf--indirect-buffers)
    (with-selected-frame frame
      (switch-to-buffer buffer :no-record)
      (buffer-to-pdf--setup-buffer-state)
      (cond
       ((and beg end)
        (narrow-to-region beg end)
        (goto-char beg))
       (beg
        (goto-char beg)
        (set-window-start nil beg))))
    frame))

(defun buffer-to-pdf--has-outline-p ()
  "Return non-nil if buffer has `outline-regexp' and concomitant heading."
  (and (bound-and-true-p outline-regexp)
       (save-excursion
         (goto-char (point-min))
         (outline-next-heading))))

(defun buffer-to-pdf--has-pages-p ()
  "Return non-nil if the current buffer has page delimiters."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward page-delimiter nil t))))

(declare-function org-map-entries "org" (func &optional match scope &rest skip))
(declare-function org-entry-end-position "org")

(defun buffer-to-pdf--export-for-org (orientation)
  "Export Org buffer, one page per heading, given ORIENTATION."
  (goto-char (point-min))
  (buffer-to-pdf--create-page orientation (point) (org-entry-end-position))
  (org-map-entries
   (lambda ()
     (buffer-to-pdf--create-page orientation (point) (org-entry-end-position)))))

;; FIXME 2026-05-02: The page delimiter should not appear in the PDF.
(defun buffer-to-pdf--export-for-page-delimiter (orientation)
  "Export buffer, one page per `page-delimiter', given ORIENTATION."
  (save-excursion
    (goto-char (point-min))
    (let ((page-start (point)))
      (while (re-search-forward page-delimiter nil t)
        (let ((page-end (match-beginning 0)))
          (buffer-to-pdf--create-page orientation page-start page-end)
          (setq page-start (match-end 0))))
      (buffer-to-pdf--create-page orientation page-start (point-max)))))

(declare-function outline-next-heading "outline")

(defun buffer-to-pdf--export-for-outline (orientation)
  "Export buffer, one page per `outline-regexp', given ORIENTATION."
  (save-excursion
    (goto-char (point-min))
    (let ((start (point)))
      (while (outline-next-heading)
        (let ((end (point)))
          (buffer-to-pdf--create-page orientation start end)
          (setq start end)))
      (buffer-to-pdf--create-page orientation start (point-max)))))

;; FIXME 2026-05-02: The final line is clipped at the bottom if the
;; text naturally reaches the end of the page.  This happens
;; regardless of what `mode-line-format' is set to.  I also tried with
;; different `internal-border-width' values.
(defun buffer-to-pdf--export-for-window-boundaries (orientation)
  "Export buffer, one page per window boundaries, given ORIENTATION."
  (let ((start (point-min)))
    (if (eobp)
        (buffer-to-pdf--create-page orientation)
      (while (< start (point-max))
        (let* ((frame (buffer-to-pdf--create-page orientation start))
               (end (with-selected-frame frame
                      (redisplay t)
                      (window-end nil t))))
          (if (<= end start)
              (setq start (point-max))
            (with-selected-frame frame
              (narrow-to-region start end))
            (setq start end)))))))

(defun buffer-to-pdf--export (orientation)
  "Produce pages for the current buffer based on its state and ORIENTATION."
  (cond
   ((derived-mode-p 'org-mode)
    (buffer-to-pdf--export-for-org orientation))
   ((buffer-to-pdf--has-outline-p)
    (buffer-to-pdf--export-for-outline orientation))
   ((buffer-to-pdf--has-pages-p)
    (buffer-to-pdf--export-for-page-delimiter orientation))
   (t
    (buffer-to-pdf--export-for-window-boundaries orientation)))
  (reverse buffer-to-pdf--frames))

(defun buffer-to-pdf--get-name (buffer)
  "Return the variable `buffer-file-name' or `buffer-name' of BUFFER."
  (or (and (buffer-file-name buffer)
           (file-name-sans-extension (file-name-nondirectory (buffer-file-name buffer))))
      (buffer-name buffer)))

(defun buffer-to-pdf--get-directory ()
  "Return the `buffer-to-pdf-directory'."
  (if (file-exists-p buffer-to-pdf-directory)
      (file-name-directory buffer-to-pdf-directory)
    (user-error "The `buffer-to-pdf-directory' must exist")))

(defun buffer-to-pdf--get-document-path (name)
  "Return a PDF document path with the given NAME."
  (expand-file-name (format "%s.pdf" name) (buffer-to-pdf--get-directory)))

(defun buffer-to-pdf--clear-state ()
  "Delete intermediate frames and buffers."
  (mapc #'delete-frame buffer-to-pdf--frames)
  (setq buffer-to-pdf--frames nil)
  (mapc #'kill-buffer buffer-to-pdf--indirect-buffers)
  (setq buffer-to-pdf--indirect-buffers nil))

(defun buffer-to-pdf--make-document (pdf-path buffer orientation)
  "Make PDF-PATH file for BUFFER using frame ORIENTATION."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (unwind-protect
            (when-let* ((frames (buffer-to-pdf--export orientation)))
              (let ((pdf-data (x-export-frames frames 'pdf))
                    (coding-system-for-write 'binary))
                (write-region pdf-data nil pdf-path)
                (message "Exported %d pages to %s" (length frames) pdf-path)))
          (buffer-to-pdf--clear-state))))))

(defvar buffer-to-pdf-orientation-prompt-history nil
  "Minibuffer history for `buffer-to-pdf-orientation-prompt'.")

(defun buffer-to-pdf-orientation-prompt ()
  "Prompt for an orientation among `buffer-to-pdf-orientations'."
  (let* ((default (car buffer-to-pdf-orientation-prompt-history))
         (choice (completing-read
                  (format-prompt "Select orientation" default)
                  (mapcar #'car buffer-to-pdf-orientations)
                  nil t nil 'buffer-to-pdf-orientation-prompt-history default)))
    (intern choice)))

;;;###autoload
(defun buffer-to-pdf (buffer orientation)
  "Make a PDF document out of the current BUFFER.
Produce the PDF based on the given ORIENTATION.  In interactive use,
prompt for ORIENTATION.  Otherwise ORIENTATION is the `car' of an
element in `buffer-to-pdf-orientations' or an alist of frame parameters.

BUFFER is a buffer object.  In interactive use, always consider the
current buffer.

Supported export methods are as follows, in order of precedence:

- Org documents have one page per heading.  The text before the first
  heading is its own page.

- Documents with an `outline-regexp' and concomitant outline headings
  behave the same as Org.

- Buffers with the `page-delimiter' get one page per delimiter.  The
  text before the first delimiter and after the last delimiter is put in
  its own page.

- As a fallback, the buffer is split into pages based on the window
  boundaries, which depend on the ORIENTATION and the font size."
  (interactive (list (current-buffer) (buffer-to-pdf-orientation-prompt)))
  (unless (string-match-p "cairo" system-configuration-features)
    (user-error "Build Emacs with support for Cairo"))
  (let* ((name (buffer-to-pdf--get-name buffer))
         (pdf-path (buffer-to-pdf--get-document-path name)))
    (buffer-to-pdf--make-document pdf-path buffer orientation)))

(provide 'buffer-to-pdf)
;;; buffer-to-pdf.el ends here
