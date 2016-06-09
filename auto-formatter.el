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

(defvar auto-formatter-keyword-list '("if" "else" "foreach" "while" "for"
                                      "switch" "try" "catch"))
(defvar auto-formatter-attachable-keyword-list '("else" "catch"))
(defvar auto-formatter-code-tag-list '("script" "style"))

(defun auto-formatter-at-indentation()
  (save-excursion
    (and (not (= (skip-chars-backward " \t") 0))
         (auto-formatter-previous-char-is "\n"))))

(defun auto-formatter-previous-char-is(char)
  (char-equal (char-before) (aref char 0)))

(defun auto-formatter-previous-non-whitespace-char-is(char)
  (save-excursion
    (skip-chars-backward " \t\n")
    (and (char-before)
         (char-equal (char-before) (aref char 0)))))

(defun auto-formatter-next-non-whitespace-char-is(char)
  (save-excursion
    (skip-chars-forward " \t\n")
    (and (char-after)
         (char-equal (char-after) (aref char 0)))))

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

(defun auto-formatter-fix-attachable(min max)
  (save-excursion
    (dolist (keyword auto-formatter-attachable-keyword-list t)
      (goto-char min)
      (while (search-forward keyword max t)
        (backward-word)
        (unless (auto-formatter-at-indentation)
          (insert "\n"))
        (forward-word)))))

(defun auto-formatter-fix-curly-braces(min max)
  (save-excursion
    (save-restriction
      (goto-char min)
      (while (search-forward "{" max t)
        (backward-char)
        (unless (or (auto-formatter-previous-char-is " ")
                    (auto-formatter-previous-char-is "(")
                    (auto-formatter-previous-char-is "[")
                    (string-suffix-p ".cshtml" buffer-file-name))
          (insert " "))
        (when (and (auto-formatter-at-indentation)
                   (not (auto-formatter-previous-non-whitespace-char-is ",")))
          (delete-indentation))
        (end-of-line)))))

(defun auto-formatter-fix-empty-lines(min max)
  (save-excursion
    (goto-char min)
    (while (search-forward "\n\n" max t)
      (backward-char)
      (when (or (auto-formatter-previous-non-whitespace-char-is "{")
                (auto-formatter-next-non-whitespace-char-is "}"))
        (kill-line)))))

(defun auto-formatter-fix-spacing(min max)
  (save-excursion
    (dolist (keyword auto-formatter-keyword-list t)
      (goto-char min)
      (while (search-forward keyword max t)
        (when (char-equal (char-after) (aref "(" 0))
          (insert " "))))))

;; Function to split lines longer than 80 characters by commas.
;; Enhancement: run again on the new line?
(defun auto-formatter-split-long-line-by-comma()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (search-forward "," (line-end-position) t)
      (user-error "There are no commas on this line"))
    (if (< (- (point) (line-beginning-position)) 80)
        (progn
          (insert "\n")
          (indent-for-tab-command))
      (user-error (concat "The comma is past the line limit, so splitting "
                          "wouldn't help.")))))

(defun auto-formatter-format-specified(min max)
  (if indent-tabs-mode
      (tabify min max)
    (untabify min max))
  (unless (string-suffix-p ".blade.php" buffer-file-name)
    (auto-formatter-fix-curly-braces min max))
  (unless (string-suffix-p ".cshtml" buffer-file-name)
    (auto-formatter-fix-spacing min max))
  (auto-formatter-fix-argument-spacing min max)
  (auto-formatter-fix-empty-lines min max)
  (unless (string-suffix-p ".blade.php" buffer-file-name)
    (auto-formatter-fix-attachable min max))
  (indent-region min max))

;;
;; The main function. Calls various other functions to do formatting.
;;
(defun auto-formatter-format-buffer()
  (interactive)
  (auto-formatter-format-specified (point-min) (point-max)))

(provide 'auto-formatter)
