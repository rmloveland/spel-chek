;; SOUNDEX

(eval-when-compile (require 'cl))

(defvar +soundex-hash+ (make-hash-table))

(defun soundex-insert-into-table (string value table)
  "Associate each of the characters from STRING with VALUE in HASH-TABLE.
String Int Hash -> (IO)"
  (mapc (lambda (c) (puthash c value table)) (string-to-list string)))

(defun soundex-build-values-table ()
  "Build the table that maps from characters to numeric values.
Null -> (IO)"
  (let ((soundex-values
	 '(("bfpv" 1)
	   ("cgjkqsxz" 2)
	   ("dt" 3)
	   ("l" 4)
	   ("mn" 5)
	   ("r" 6))))
    (mapc (lambda (lst)
	    (soundex-insert-into-table (car lst) (cadr lst)
				       +soundex-hash+))
	  soundex-values)))

(defun soundex-lookup-char (char)
  "Look up the value of CHAR in the Soundex table.
Char -> Int"
  (gethash char +soundex-hash+))

(defun soundex-drop-vowel-chars (char-list)
  "Return CHAR-LIST with all of the vowel characters filtered out.
List -> List"
  (let ((vowel-chars
	 (string-to-list "aeiouyhw")))
    (remove-if #'null
	       (mapcar (lambda (c)
			 (unless (member c vowel-chars)
			   c))
		       char-list))))

(defun soundex-drop-vowels (string)
  "Given a STRING, return a string with the vowels removed.
String -> String"
  (mapconcat (lambda (c)
	       (char-to-string c))
	     (soundex-drop-vowel-chars (string-to-list string)) nil))

(defun soundex-generate-code (string)
  "Generate the appropriate Soundex code for STRING."
  (let ((answer
	 (mapconcat (lambda (n)
		      (number-to-string n))
		    (mapcar (lambda (c)
			      (soundex-lookup-char c))
			    (cdr (string-to-list
				  (soundex-drop-vowels string)))) "")))
    (concat
     (char-to-string (car (string-to-list string)))
     answer)))

(defun soundex-list-to-string (list)
  "Given the elements of LIST, a list of character codes, produces a string."
  (mapconcat (lambda (c) (char-to-string c)) list nil))

(defun soundex-string-truncate (str w)
  "Truncate STRING to WIDTH characters."
  (let ((chars (nreverse (string-to-list str))))
    (while (not (= (length chars) 4))
      (setq chars (cdr chars)))
    (soundex-list-to-string (nreverse chars))))

(defun soundex-string-pad (string n)
  "Pad STRING with N zeroes."
  (let* ((chars (nreverse (string-to-list string)))
	 (len (length chars)))
    (while (not (= (length chars) (+ len n)))
      (setq chars (cons 48 chars)))
    (soundex-list-to-string (nreverse chars))))

; take the string
; convert it to list
; pop the first char, and push it onto the results list
; for each of the remaining chars:
;   for each of the 5 lookup tables:
;     if char in table, then replace char with value from table and push onto results list and exit the procedure
;   if we get here without the character being in any of the lookup tables, push char onto results list as-is
