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

(defun spel-flatten-list (lst)
  (cond ((listp lst)
	 (apply #'append (mapcar 'spel-flatten-list lst)))
	(t (list lst))))

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

(spel-train "c:/Users/rml/Downloads/big.txt")

(defun spel-edit-distance-1 (word)
  (let ((result (spel-flatten-list
                 (append
                  (spel-splits word)
                  (spel-deletes word)
                  (spel-transposes word)
                  (spel-replaces word)
                  (spel-inserts word)))))
    result))

(defun spel-splits (word)
  "String -> List
Given a word, return a list of the possible substrings
that can be made from that word."
  (let ((splits nil))
    (dotimes (i (length word))
      (push (list (substring word 0 i)
                  (substring word i (length word))) splits))
    splits))

(defun spel-deletes (word)
  (mapcar (lambda (pair)
            (concat (car pair)
                    (substring (cadr pair) 1 (length (cadr pair)))))
          (spel-splits word)))

(defun spel-transposes (word)
  (remove-if-not #'stringp
                 (mapcar (lambda (pair)
                           (when (> (length (cadr pair)) 1)
                             (concat (car pair) 
                                     (substring (cadr pair) 1 2)
                                     (substring (cadr pair) 0 1)
                                     (substring (cadr pair) 2 (length (cadr pair))))))
                         (spel-splits word))))

(defun spel-replaces (word)
  (spel-flatten-list
   (mapcar (lambda (pair)
             (when (cadr pair)
               (mapcar (lambda (c)
                         (concat (car pair)
                                 (char-to-string c)
                                 (substring (cadr pair) 1 (length (cadr pair)))))
                       *spel-alphabet)))
           (spel-splits word))))

(defun spel-inserts (word)
  (spel-flatten-list
   (mapcar (lambda (pair)
             (mapcar (lambda (c)
                       (concat (car pair)
                               (char-to-string c)
                               (cadr pair)))
                     *spel-alphabet))
           (spel-splits word))))

(defun spel-known (words)
  (let ((known nil))
    (mapc (lambda (w)
            (when (gethash w *spel-words) (push w known)))
          words)
    known))

(defun spel-correct (word)
  (let ((candidates (or (spel-known (spel-edits-1 word)) (spel-known word))))
    candidates))
