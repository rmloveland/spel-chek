;; Copyright (C) 2012 Rich Loveland

;; Author: Rich Loveland <loveland.richard@gmail.com>
;; Keywords: spellcheck, spelling
;; Version: 0.1

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
;;
;; 
;; 
;; 
;; 

(defgroup spel nil
  "A spelling checker implemented in Emacs Lisp."
  :group 'spel)

(defvar *spel-words (make-hash-table :test 'equal)
  "A hash that holds the count of how many times a word from our
  training file has been seen.")

(defvar *spel-alphabet "abcdefghijklmnopqrstuvwxyz")

(defun spel-add-word (word)
  "Add a new word (with a count of 1) to our word hash table, or
increment its existing count if it's been seen already."
  (let ((tally (gethash word *spel-words)))
    (if tally
        (setq tally (1+ tally))
      (setq tally 1))
    (puthash word tally *spel-words)))

(defun spel-train (file)
  "Create a hash table that keeps a count of each word's appearances."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((words (split-string (buffer-string))))
      (mapc (lambda (word) (spel-add-word word)) words))))

(defun spel-edits1 (word)
  "Dummy function, for now."
  (let* ((splits '())
         (deletes '())
         (transposes '())
         (replaces '())
         (inserts '()))))

(defun spel-split-words (words)
  "Given a list of words, return a list of the possible substrings
that can be made from those words."
  (let ((splits nil))
    (mapc (lambda (word)
            (dotimes (i (length word))
              (push (cons (substring word 0 i)
                          (substring word i (length word))) splits)))
          words)
    splits))

(spel-train "c:/Users/rml/Downloads/big.txt")
