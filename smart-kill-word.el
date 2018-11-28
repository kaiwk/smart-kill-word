;;; smart-kill-word.el --- delete word in smart way

;; Copyright (C) 2018  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Default `backward-kill-word' has several problems:
;;;     - when cursor at beginning of line, it kills line above
;;;     - for word like 'getThingFromList', it kills the whole word
;;;
;;; 1. Don't kill too much
;;; 2. For word 'getThingFromList', smart-kill-word will kill 'List', then 'From',
;;; 'Thing' and 'get'
;;

;;; Code:


;;;###autoload
(defun smart-kill-word (arg)
  (interactive "p")
  (when (looking-back "\\ca\\s-*")
    (let* ((word-end (save-excursion (backward-word) (forward-word) (point)))
           (word-beg (save-excursion
                       (backward-word)
                       (save-match-data
                         (let ((case-fold-search nil))
                           (if (re-search-forward "\\([a-z].*[A-Z]\\)" word-end t)
                               (- (match-end 1) 1)
                             nil)))))
           (delimiter-end (point))
           (delimiter-beg (save-excursion
                            (backward-word) (forward-word)
                            (save-match-data
                              (if (re-search-forward "\\([-_,.; ]\\)" delimiter-end t)
                                  (match-beginning 1)
                                delimiter-end)))))
      ;; clear spaces
      (ignore-errors (kill-region delimiter-beg delimiter-end))
      (goto-char word-end)
      ;; delete word
      (if word-beg
          (progn
            (kill-region word-beg word-end))
        (kill-word (- arg))))))


(provide 'smart-kill-word)
;;; smart-kill-word.el ends here
