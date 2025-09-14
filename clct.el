;;; clct.el --- Highlight coverage data for Common Lisp files -*- lexical-binding: t; -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp, tools, coverage
;; URL: https://github.com/diasbruno/clct
;; License: Unlicense

;;; Commentary:

;; clct highlights Common Lisp source files based on coverage
;; information produced by cl-coverage-tools.
;;
;; Usage:
;; 1. Load a Lisp file in Emacs.
;; 2. Ensure a corresponding `.cov` file exists in the same directory.
;; 3. Run `M-x clct-apply-coverage` to highlight the buffer.

;;; Code:

(defgroup clct nil
  "Highlight Common Lisp coverage data."
  :group 'tools
  :prefix "clct-")

(defface clct-not-instrumented
  '((t :underline (:style line :color "gray40")))
  "Face for not instrumented code.")

(defface clct-conditionalized-out
  '((t :underline (:style line :color "gray60")))
  "Face for conditionalized out code.")

(defface clct-executed
  '((t :underline (:style line :color "green")))
  "Face for executed code.")

(defface clct-not-executed
  '((t :underline (:style line :color "red")))
  "Face for not executed code.")

(defface clct-both-branches
  '((t :underline (:style line :color "goldenrod")))
  "Face for both branches taken.")

(defface clct-one-branch
  '((t :underline (:style line :color "orange")))
  "Face for one branch taken.")

(defface clct-neither-branch
  '((t :underline (:style line :color "purple")))
  "Face for neither branch taken.")

;; States and labels (from your earlier mapping)
(defconst clct--state-labels
  '((0  . "Not instrumented")
    (15 . "Conditionalized out")
    (1  . "Executed")
    (2  . "Not executed")
    (5  . "Both branches taken")
    (6  . "One branch taken")
    (10 . "Neither branch taken"))
  "Alist mapping numeric state -> human-friendly label.")

(defun clct--face-for-state (state)
  "Return the face symbol associated with numeric STATE."
  (pcase state
    (0  'clct-not-instrumented)
    (15 'clct-conditionalized-out)
    (1  'clct-executed)
    (2  'clct-not-executed)
    (5  'clct-both-branches)
    (6  'clct-one-branch)
    (10 'clct-neither-branch)
    (_  'clct-not-instrumented)))

(defun clct--label-for-state (state)
  "Return descriptive string for STATE if known, else string of STATE."
  (or (cdr (assoc state clct--state-labels))
      (format "State %s" state)))

;;;###autoload
(defun cl-coverage-tools--cov-file-for-buffer (&optional buffer)
  "Return the `.cov` filename associated with BUFFER (or current buffer).
If the buffer is not visiting a file, return nil."
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((source (buffer-file-name)))
      (when source
        (concat source ".cov")))))

(defun clct-clear (&optional buffer)
  "Remove cl-coverage-tools overlays in BUFFER (or current buffer)."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (delete-all-overlays)))

;;;###autoload
(defun clct-apply (&optional buffer)
  "Read the `.cov` file for BUFFER (default current buffer) and apply overlays.
This reads `file.cov` (where `file` is the visiting filename) and uses
character offsets (0-based) as positions in the buffer."
  (interactive)
  (let* ((source-buffer (or buffer (current-buffer)))
         (cov-file (concat (buffer-file-name source-buffer) ".cov")))
    (if (not (file-exists-p cov-file))
        (warn "[clct] coverage file not available.")
      ;; Remove old overlays first
      (progn
        (delete-all-overlays source-buffer)
        (with-temp-buffer
          (insert-file-contents cov-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (when (string-match
                     "\\([A-Z]+\\):\\(T\\|NIL\\),\\([0-9]+\\),\\([0-9]+\\)"
                     line)
                (let* ((type (match-string 1 line))
                       (state (match-string 2 line))
                       (start (string-to-number (match-string 3 line)))
                       (len (string-to-number (match-string 4 line)))
                       (ov (make-overlay (1+ start) (+ (1+ start) len) source-buffer)))
                  (let ((text (with-current-buffer source-buffer
                                         (buffer-substring-no-properties
                                          (1+ start)
                                          (+ 1 start len)))))
                    (with-current-buffer (get-buffer-create "*example*")
                      (insert text)
                      (insert "\n"))
                  (overlay-put ov 'face (clct--face-for-state (if (string-equal state "T") 1 2))))
                                        ; (overlay-put ov 'clct t)
                                        ; (overlay-put ov 'help-echo (clct--label-for-state
                (forward-char (1+ (pos-eol))))))))))))

(provide 'clct)
;;; clct.el ends here
