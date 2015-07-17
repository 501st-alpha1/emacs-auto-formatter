;; Emacs extension to automatically format code
;; Copyright (C) 2015 Scott Weldon

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun auto-formatter-at-indentation()
  (save-excursion
    (and (not (= (skip-chars-backward " \t") 0))
         (auto-formatter-previous-char-is "\n"))))

(defun auto-formatter-previous-char-is(char)
  (char-equal (char-before) (aref char 0)))

(defun auto-formatter-previous-non-whitespace-char-is(char)
  (save-excursion
    (skip-chars-backward " \t\n")
    (char-equal (char-before) (aref char 0))))

(defun auto-formatter-fix-argument-spacing(min max)
  (save-excursion
    (goto-char min)
    (while (search-forward "(" max t)
      (let ((local-min (point))
            (local-max (search-forward ")" max t)))
        (goto-char local-min)
        (while (search-forward "," local-max t)
          (unless (char-equal (char-after) (aref " " 0))
            (insert " ")))))
    ;; FIXME: (almost) duplicate code
    (while (search-forward "[" max t)
      (let ((local-min (point))
            (local-max (search-forward "]" max t)))
        (goto-char local-min)
        (while (search-forward "," local-max t)
          (unless (char-equal (char-after) (aref " " 0))
            (insert " ")))))))

(defun auto-formatter-fix-curly-braces(min max)
  (save-excursion
    (save-restriction
      (goto-char min)
      (while (search-forward "{" max t)
        (backward-char)
        (unless (auto-formatter-previous-char-is " ")
          (insert " "))
        (when (auto-formatter-at-indentation)
          (delete-indentation))
        (end-of-line)))))

(defun auto-formatter-fix-spacing(min max)
  (save-excursion
    (dolist (keyword auto-formatter-keyword-list t)
      (goto-char min)
      (while (search-forward keyword max t)
        (when (char-equal (char-after) (aref "(" 0))
          (insert " "))))))

;;
;; The main function. Calls various other functions to do formatting.
;;
(defun auto-formatter-format-buffer()
  (interactive)
  (untabify (point-min) (point-max))
  (auto-formatter-fix-curly-braces (point-min) (point-max))
  (auto-formatter-fix-spacing (point-min) (point-max))
  (auto-formatter-fix-argument-spacing (point-min) (point-max))
  (indent-region (point-min) (point-max)))
