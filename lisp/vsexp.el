;;; vsexp.el --- Marking, Killing and Kill-Saving contents of a sexp -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Stanislav Arnaudov

;; Author: Stanislav Arnaudov
;; Version: 1.1
;; Package-Requires: ((emacs "24.3") (smartparens "1.8.0"))
;; Keywords: convenience
;; URL: https://github.com/palikar/vsexp

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Several very simple functions for easier manipulation
;;; of /sexp/s.  I know that this functionality more or less exists
;;; with the existing package smartparens but I found myself needing
;;; the implemented here behavior more and more in my daily
;;; programming life.  Suggested (and the prefered by me) keybinding
;;; can be found in the git repository

;;; Code:
(require 'smartparens)


(defun vsexp-mark-sexp ()
  "Mark the content inside of the current sexp."
  (interactive)
  (if (use-region-p)
      (progn
	(sp-backward-up-sexp)
	(sp-backward-up-sexp)
	(forward-char 1)
	(set-mark-command nil)
	(sp-up-sexp)
	(backward-char 1))
    (progn
      (sp-backward-up-sexp)
      (forward-char 1)
      (set-mark-command nil)
      (sp-up-sexp)
      (backward-char 1))))

(defun vsexp-mark-sexp-whole ()
  "Mark the content outside of the current sexp."
  (interactive)
  (sp-backward-up-sexp)
  (set-mark-command nil)
  (forward-sexp))

(defun vsexp-kill-save-sexp ()
  "Kill the content inside of the current sexp."
  (interactive)
  (sp-backward-up-sexp)
  (forward-char 1)
  (set-mark-command nil)
  (sp-up-sexp)
  (backward-char 1)
  (kill-ring-save (region-beginning) (region-end))
  (deactivate-mark))

(defun vsexp-kill-save-sexp-whole ()
  "Kill the content outside of the current sexp."
  (interactive)
  (sp-backward-up-sexp)
  (set-mark-command nil)
  (sp-up-sexp)
  (kill-ring-save (region-beginning) (region-end))
  (deactivate-mark))

(defun vsexp-kill-sexp ()
  "Kill the content inside of the current sexp."
  (interactive)
  (sp-backward-up-sexp)
  (forward-char 1)
  (set-mark-command nil)
  (sp-up-sexp)
  (backward-char 1)
  (kill-region (region-beginning) (region-end)))

(defun vsexp-kill-sexp-whole ()
  "Mark the content inside of the current sexp."
  (interactive)
  (sp-backward-up-sexp)
  (set-mark-command nil)
  (forward-sexp)
  (kill-region (region-beginning) (region-end)))


(defvar vsexp-bracket-map
  '(("<" . ">")
    ("(" . ")")
    ("[" . "]")
    ("{" . "}"))
  "Pair of symbols that are considered a bracket-pair.
Will fallback to this map if teh sexp is not in the current symbol map")


(defun vsexp--get-matching-bracket (bracket-char-string)
  "Find the other bracket of BRACKET-CHAR-STRING."
  (interactive)
  (let (($syntableValue (aref (syntax-table) (string-to-char bracket-char-string))))
    (if (or (eq (car $syntableValue ) 4)
	    (eq (car $syntableValue ) 5))
	(char-to-string (cdr $syntableValue))
      (progn
	(dolist (braces vsexp-bracket-map)
	  (cond
	   ((string= (car braces) bracket-char-string) (return (cdr braces)))
	   ((string= (cdr braces) bracket-char-string) (return (car braces)))))))))

(defun vsexp--back-to-brace (char &optional forward)
  "Go back to CHAR.
If FORWARD is not nil, the direction is reversed"
  (let* ((other-char (vsexp--get-matching-bracket char))
	 (count 0)
	 (current (char-to-string  (char-after))))
    (while (or (> count 0) (not (string= current char)))
      (when (string= current other-char)
	(setq count (+ count 1)))
      (when (string= current char)
	(setq count (- count 1)))
      (if forward (forward-char 1) (backward-char 1))
      (while (nth 3 (syntax-ppss))
	(if forward (forward-char 1) (backward-char 1)))
      (setq current (char-to-string  (char-after))))))

(defun vsexp--mark (&optional inside)
  "Mark inside the thing.
If INSIDE is not nil, the active region will be around the thing"
  (let* ((char (char-to-string (read-key "Mark:")))
	 (other-char (vsexp--get-matching-bracket char)))
    (if other-char
	(progn
	  (backward-char 1)
	  (vsexp--back-to-brace char nil)
	  (when inside (forward-char 1))
	  (set-mark-command nil)
	  (forward-char 1)
	  (vsexp--back-to-brace other-char t)
	  (unless inside (forward-char 1)))
      (progn
	(search-backward char nil nil 1)
	(when inside (forward-char 1))
	(set-mark-command nil)
	(forward-char 1)
	(search-forward char nil nil 1)
	(when inside (backward-char 1))))))

(defun vsexp-mark-around ()
  "Mark around prompted char."
  (interactive)
  (vsexp--mark nil))

(defun vsexp-mark-inside ()
  "Mark inside prompted char."
  (interactive)
  (vsexp--mark t))

(provide 'vsexp)
;;; vsexp.el ends here
