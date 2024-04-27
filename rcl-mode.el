;;; rcl-mode.el --- major mode for editing RCL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2024, Sergei Mishin

;; Author: Sergei Mishin
;; Version: 0.1.0
;; Created: 04 Feb 2024
;; Keywords: languages
;; Homepage: http://github.com/qezz/rcl-mode

;;; License:

;;; MIT

;;; Commentary:

;; Major mode for editing RCL Configuration Language

;;; Code:

(defvar rcl-mode-hook nil)

(setq rcl-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("let" "for" "in" "not" "if" "and" "or" "assert" "trace" "type"))
             (x-types '("Bool" "Int" "String" "Null" "Dict" "List" "Set" "Dynamic"))
             (x-constants '("true" "false" "null" "f\".*\""))
             (x-comments '("//.*"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-comments-regexp (regexp-opt x-comments 'words))
             )

        `(
          (,x-comments-regexp . 'font-lock-comment-face)
          (,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
          (,x-keywords-regexp . 'font-lock-keyword-face))))

(defvar rcl-default-indent 2)

(defun rcl-indent-line ()
  "Indent current line as RCL code.

Attempts to perform the meaningful indentation of the current
line, following some rules:

If the cursor is between the beginning of the line and the actual
code, it will be moved to the indentation.

If the cursor is in the middle if the code line, it should stay
at the same character, relative to indentation."
  (interactive)

  (let (indent
        was-between-beginning-and-code
        (orig-point (point))            ; save the original point
        (point (point)))
    (save-excursion
      (back-to-indentation)
      (setq indent (car (syntax-ppss))
            was-between-beginning-and-code (and (<= orig-point (point))
                                                (>= orig-point (line-beginning-position))))

      ;; If the next character is a closing paren, decrease the indentation
      (when (memq (char-after) '(?\) ?\} ?\]))
        (setq indent (1- indent)))

      ;; Recalculate the indentation

      ;; Remove the current indentation
      (delete-region (line-beginning-position)
                     (point))

      ;; Insert the new indentation
      (indent-to (* rcl-default-indent indent)))

    ;; If the cursor originally was between the beginning and the
    ;; actual code, move it to indentation
    (when was-between-beginning-and-code
      (move-to-column (current-indentation) t))))

(defvar rcl-mode-syntax-table
  (let ((rcl-mode-syntax-table (make-syntax-table)))
    ;; C-style single-line comments (//)
    (modify-syntax-entry ?/ ". 124b" rcl-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" rcl-mode-syntax-table)

    ;; Strings with double quotes
    (modify-syntax-entry ?\" "\"" rcl-mode-syntax-table)

    rcl-mode-syntax-table)
  "Syntax table for rcl-mode")

;;;###autoload
(defun rcl-mode ()
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table rcl-mode-syntax-table)

  ;; Comment hint to `comment-dwim' emacs function, by default mapped to `M-;'
  (set (make-local-variable 'comment-start) "//")

  ;; font-lock
  (set (make-local-variable 'font-lock-defaults) '(rcl-font-lock-keywords))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'rcl-indent-line)
  (setq major-mode 'rcl-mode)
  (setq mode-name "RCL"))

(add-to-list 'auto-mode-alist '("\\.rcl\\'" . rcl-mode))

(provide 'rcl-mode)

;;; rcl-mode.el ends here
