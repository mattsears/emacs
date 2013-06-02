;;; string-tools.el --- (WORK IN PROGRESS) Collection of handy functions for strings

;; Copyright (C) 2012 Matt Sears

;; Author: Matt Sears <matt@mattsears.com>
;; Maintainer: Matt Sears <matt@mattsears.com>
;; Version: 0.1.0
;; Keywords: speed, convenience
;; URL: http://github.com/mattsears/string-tools

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; string-tools is a minor mode for Emacs that can be used with
;; ruby-mode. It includes various handy functions that will improve
;; your productivity.
;;
;; To use string-tools-mode, make sure that this file is in Emacs load-path:
;;   (add-to-list 'load-path "/path/to/directory/or/file")
;;
;; Then require string-tools:
;;   (require 'string-tools)
;;
;; string-tools-mode is automatically started in ruby-mode.


;;; Code:

(defvar string-tools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-'") 'string-tools-to-single-quote-string)
    (define-key map (kbd "C-\"") 'string-tools-to-double-quote-string)
    (define-key map (kbd "C-:") 'string-tools-to-symbol)
    (define-key map (kbd "C-;") 'string-tools-clear-string)
    map)
  "Keymap for `string-tools-mode'.")

(defun string-tools-toggle-string ()
  "Toggle quoted, single-quoted, symbol, or no quotes")

(defun string-tools-looking-around (back at)
  "Check if looking backwards at BACK and forward at AT."
  (and (looking-at-p at) (looking-back back)))

(defun string-tools-symbol-at-point-p ()
  "Check if cursor is at a symbol or not."
  (string-tools-looking-around ":[A-Za-z0-9_]*" "[A-Za-z0-9_]*"))

(defun string-tools-string-at-point-p ()
  "Check if cursor is at a string or not."
  (string-tools-string-region))

(defun string-tools-symbol-region ()
  "Return region for symbol at point."
  (list
   (save-excursion
     (search-backward ":" (line-beginning-position) t))
   (save-excursion
     (if (re-search-forward "[^A-Za-z0-9_]" (line-end-position) t)
         (1- (point))
       (line-end-position)))))

(defun string-tools-string-region ()
  "Return region for string at point."
  (let ((orig-point (point)) (regex "\\([\"']\\)\\(?:[^\\1]\\|\\\\.\\)*?\\(\\1\\)") beg end)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (re-search-forward regex (line-end-position) t) (not (and beg end)))
        (let ((match-beg (match-beginning 0)) (match-end (match-end 0)))
          (when (and
                 (> orig-point match-beg)
                 (< orig-point match-end))
            (setq beg match-beg)
            (setq end match-end))))
      (and beg end (list beg end)))))


(defun string-tools-to-symbol ()
  "Turn string at point to symbol."
  (interactive)
  (if (string-tools-string-at-point-p)
      (let* ((region (string-tools-string-region))
             (min (nth 0 region))
             (max (nth 1 region))
             (content (buffer-substring-no-properties (1+ min) (1- max))))
        (when (string-match-p "^\\([a-ZA-Z_][a-ZA-Z0-9_]+\\)?$" content)
          (let ((orig-point (point)))
            (delete-region min max)
            (insert (concat ":" content))
            (goto-char orig-point))))))

(defun string-tools-to-single-quote-string ()
  (interactive)
  (string-tools-to-string "'"))

(defun string-tools-to-double-quote-string ()
  (interactive)
  (string-tools-to-string "\""))

(defun string-tools-to-string (string-quote)
  "Convert symbol or string at point to string."
  (let* ((at-string
          (string-tools-string-at-point-p))
         (at-symbol
          (and (not at-string) (string-tools-symbol-at-point-p))))
    (when (or at-string at-symbol)
      (let* ((region
              (or
               (and at-symbol (string-tools-symbol-region))
               (and at-string (string-tools-string-region))))
             (min (nth 0 region))
             (max (nth 1 region))
             (content
              (buffer-substring-no-properties (1+ min) (if at-symbol max (1- max)))))
        (setq content
              (if (equal string-quote "'")
                  (replace-regexp-in-string "\\\\\"" "\"" (replace-regexp-in-string "\\([^\\\\]\\)'" "\\1\\\\'" content))
                (replace-regexp-in-string "\\\\\'" "'" (replace-regexp-in-string "\\([^\\\\]\\)\"" "\\1\\\\\"" content))))
        (let ((orig-point (point)))
          (delete-region min max)
          (insert
           (format "%s%s%s" string-quote content string-quote))
          (goto-char orig-point))))))

(defun string-tools-clear-string ()
  "Clear string at point."
  (interactive)
  (when (string-tools-string-at-point-p)
    (let* ((region (string-tools-string-region))
           (min (nth 0 region))
           (max (nth 1 region)))
      (delete-region (+ min 1) (- max 1)))))

;;;###autoload
(define-globalized-minor-mode global-string-tools-mode
  string-tools-mode turn-on-string-tools-mode-if-desired)

(defun turn-on-string-tools-mode-if-desired ()
  (when (apply 'derived-mode-p hl-todo-activate-in-modes)
    (string-tools-mode 1)))

(defcustom string-tools-activate-in-modes '(emacs-lisp-mode)
  "Major modes in which `hl-todo-mode' should be activated.
This is used by `global-hl-todo-mode'."
  :group 'orglink
  :type '(repeat function))

;;;###autoload
(define-minor-mode string-tools-mode
  "Collection of handy functions for ruby-mode."
  :init-value nil
  :lighter " st"
  :keymap string-tools-mode-map)

;; (add-hook 'text-mode-hook 'string-tools-mode)

(provide 'string-tools)

;;; string-tools.el ends here
