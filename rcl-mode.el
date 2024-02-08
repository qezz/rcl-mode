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

(defvar rcl-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)

    map)
  "Keymap for RCL major mode")

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
  "Indent current line as RCL code"
  (interactive)
  (beginning-of-line)

  ;; beginning of buffer - always indent to 0
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*[]})]") ; dedent on closing parens
          (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) rcl-default-indent)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))

        (save-excursion
	  (while not-indented
	    (forward-line -1)
	    (if (looking-at "^[ \t]*[]})]") ; dedent on closing parens
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at "^.*[[{(]$") ; indent on open parens
		  (progn
		    (setq cur-indent (+ (current-indentation) rcl-default-indent)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))

      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0)))))

(defvar rcl-mode-syntax-table
  (let ((rcl-mode-syntax-table (make-syntax-table)))
    ;; C-style single-line comments (//)
    (modify-syntax-entry ?/ ". 124b" rcl-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" rcl-mode-syntax-table)

    rcl-mode-syntax-table)
  "Syntax table for rcl-mode")

;;;###autoload
(defun rcl-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map rcl-mode-map)
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
