;;; whippet-mode.el --- <enter description here>  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chris Barrett

;; Version: 0.1

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst whippet-mode-keywords
  '("import" "signature" "module" "type" "trait" "impl" "record"
    "let" "law" "match" "as" "fun"
    "if" "then" "else"))

(defvar whippet-mode-map
  (make-sparse-keymap))

(defconst whippet-ty-or-ctor-regexp
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst whippet-mode-font-lock-keywords
  `((,(regexp-opt whippet-mode-keywords 'symbols) . font-lock-keyword-face)
    (,whippet-ty-or-ctor-regexp 1 font-lock-type-face)))

(defconst whippet-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(with-eval-after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'whippet-mode))

;;;###autoload
(define-derived-mode whippet-mode prog-mode "whippet"
  "Major mode for prototyping.

    \\{whippet-mode-map}"
  :syntax-table whippet-mode-syntax-table

  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local indent-tabs-mode nil)
  (setq-local font-lock-defaults '(whippet-mode-font-lock-keywords nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.whippet\\'" . whippet-mode))

(provide 'whippet-mode)

;;; whippet-mode.el ends here
