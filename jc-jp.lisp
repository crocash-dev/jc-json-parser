(defpackage #:jc-jp
  (:use #:common-lisp
	#:iterate
	#:parse-number)
  (:export #:main))

(in-package #:jc-jp)

;; Program to parse JSON
;; Reports to Standard Output a suitable message
;; Exit with Code 0 for success and Code 1 for invalid

;; Testing Parameters

;; key validity testing params
(defparameter *test-key1* "key1")
(defparameter *test-key1-false* #\,)
(defparameter *test-key2-false* 5)
(defparameter *test-key3-false* 'false)

;; value validity testing params
(defparameter *test-value1* "value1")
(defparameter *test-value2* 2)
(defparameter *test-value3* 'null)
(defparameter *test-value4* 'true)
(defparameter *test-value5* 'false)
(defparameter *test-value1-false* #\})

;; testing file
(defparameter *input-file* "./tests/step1/valid.json")


;; pairs testing params
(defparameter *test-pair* "{\"key\": \"value\"}")
(defparameter *test-pair-comma* "{\"key\": \"value\",}")
(defparameter *false-test-pair-comma* "{\"key\": \value\",}")
(defparameter *false-test-pair1* "\"key\": \"value\"}")
(defparameter *false-test-pair2* "{\"key\" \"value\"}")
(defparameter *false-test-pair3* "{\"key\": value\"}")
(defparameter *false-test-pair4* "\"key\": value\"}")
(defparameter *test-multiple-pairs* "{
  \"key\": \"value\",
  \"key2\": \"value\"
}")
(defparameter *false-multiple-pairs* "{
  \"key\": \"value\",
  key2: \"value\"
}")

;; lexer testing
(defparameter *lexer-test1* "{\"key1\": \"value1\", \"key2\": -112.5, \"key3\": 112.5, \"key4\": 56, \"key5\": null, \"key6\": true, \"key7\": false}")
(defparameter *lexer-test1-false* "{\"key1\": \"value1\", \"key2\": -112.5, \"key3\": 11S2.5, \"key4\": 5G6, \"key5\": noggin, \"key6\": tree, \"key7\": false")

(defparameter *lexer-test2-false* "{\"key1\": \"value1, \"key2\": -112.5}") ; missing closing quote
(defparameter *lexer-test3-false* "{\"key1\": \"value1\", \"key2\: -112.5, \"key3\": 112.5, \key4\": 56, \"key5\"\"\"\"\": null, \"key6\": true, \"key7\": false}") ;even number of messed up closing quotes


;; Program Parameters
(defparameter *lexical-tokens* '(#\{ #\} #\[ #\] #\: #\,))

(defparameter *lexical-reserved-symbols* '(true false null))

(defparameter *number-signs* '("+" "-"))

(defparameter *token-types* '("colon" "comma" "string" "boolean" "null" "number")) 


(defun main ()
  ;; Executable name is first argv, take REST to discard
  (let* ((args (rest sb-ext:*posix-argv*)))
    (cond ((uiop:file-exists-p (first args))
	   (let ((data (with-open-file (stream (first args) :direction :input)
			(let ((contents (make-string (file-length stream))))
			  (read-sequence contents stream)
			  contents))))
	     (format t "~A" data)))
	  ('t (format t "Invalid CLI argument. Please provide a file to parse.")))))


(defun lexer (input-string)
  "Returns either valid tokens or NIL where there is an unclosed string, invalid number, or invalid boolean or invalid null"
  (let ((lex-list '()) (end-index 'nil))
    (iter (for i from 0 to (- (length input-string) 1))
      ;; if a lexical symbol, push to list
      (cond ((member (char input-string i) *lexical-tokens* :test #'string=)
	     (push (char input-string i) lex-list))
	    ;; if a string, extract it as a string, and set i to index of last quote
	    ((string= (char input-string i) "\"")
	     (setf end-index (get-closing-quote-index input-string i))
	     (if end-index
		 (progn
		   (push (extract-valid-string input-string i end-index) lex-list)
		   (setf i end-index))
		 (push 'nil lex-list)))
	    ;; if a number, extract it as a string, and set i to closing-index -1 to capture any symbols following
	    ((or (member (char input-string i) *number-signs* :test #'string=)
		 (digit-char-p (char input-string i)))
	     (setf end-index (get-closing-index input-string i))
	     (if end-index 
		 (progn
		   (push (extract-valid-number input-string i end-index) lex-list)
		   (setf i (- end-index 1)))
		 (push 'nil lex-list)))
	    ;; Same for booleans as numbers
	    ((or (string= (char input-string i) "t")
		 (string= (char input-string i) "f")
		 (string= (char input-string i) "n"))
	     (setf end-index (get-closing-index input-string i))
	     (if end-index
		 (progn
		   (push (extract-boolean-or-null input-string i end-index) lex-list)
		   (setf i (- end-index 1)))
		 (push 'nil lex-list)))))
    (reverse lex-list)))


(defun valid-key-p (input)
  (and (not (member input *lexical-tokens* :test #'eql)) (stringp input)))
	 

(defun valid-value-p (input)
  ;; Needs testing TODO 30/10/2026
  (cond ((member input *lexical-reserved-symbols*)
	 't)
	 ((and
	   (not (member input *lexical-tokens* :test #'eql))
	   (stringp input))
	  't)
	((numberp input)
	 't)
	('t 'nil)))

(defun extract-valid-string (input-string starting-index ending-index)
  "Extracts the substring at starting-index + 1 (to cut off initial quote mark)
   and ends at ending-index - 1, as end is not included by subseq, cutting off
   the ending quote mark of the string."
   (subseq input-string (+ starting-index 1) ending-index))


(defun extract-valid-number (input-string starting-index ending-index)
  "Returns string as number or NIL if invalid number"
  (let ((compare-string ""))
    (setf compare-string (subseq input-string starting-index ending-index))
    (handler-case
	(parse-number:parse-number compare-string)
      (error ()
	'nil))))

(defun extract-boolean-or-null (input-string starting-index ending-index)
  "Extracts booleans true and false and null as symbols, or returns nil if no match found"
  (let ((compare-string ""))
    (setf compare-string (subseq input-string starting-index ending-index))
    (cond ((string= compare-string "true")
	   'true)
	  ((string= compare-string "false")
	   'false)
	  ((string= compare-string "null")
	   'null)
	  ('t 'nil))))

(defun get-closing-quote-index (input-string starting-index)
  "Gets index of closing quote given input-string and index of opening quote as starting-index, or Nil if none"
  (iter (for i from (+ starting-index 1) to (- (length input-string) 1))
    (when (string= (char input-string i) "\"")
      (leave i))))

(defun get-closing-index (input-string starting-index)
  "Gets the end-index of the character which terminates the number, boolean, or null, and returns Nil when there is no end-index found"
  (iter (for i from starting-index to (- (length input-string) 1))
    (cond ((string= (char input-string i) " ")
	   (leave i))
	  ((member (char input-string i) *lexical-tokens* :test #'string=)
	   (leave i)))))
	 

(defun valid-curly-brackets-p (input-string)
  (let ((curly-nesting-level 0) (token-found-p 'nil))
    (iter (for char in-string input-string)
      (cond ((string= char "{")
	     (incf curly-nesting-level)
	     (setf token-found-p 't))
	    ((string= char "}")
	     (decf curly-nesting-level)
	     (setf token-found-p 't))))
    (and token-found-p (= curly-nesting-level 0))))


;; (defun valid-multiple-pairs-p (input-string)
;;   ;; Totally broken
;;   (let ((temp-string "") (previous-char "") (comma-present 'nil) (num-commas 0) (num-valid-pairs 0))
;;     (iter (for char in-string input-string)
;;       (setf temp-string (concatenate 'string temp-string (string char)))
;;       (cond ((and (string= previous-char ",") (valid-pair-plus-comma-p temp-string))
;; 	     (format t "plus-comma: ~D" temp-string)
;; 	     (incf num-commas)
;; 	     (setf comma-present 't)
;; 	     (incf num-valid-pairs)
;; 	     (setf temp-string ""))
;; 	    ((valid-key-value-pair-p temp-string)
;; 	     (format t "no-comma: ~D" temp-string)
;; 	     (incf num-valid-pairs)
;; 	     (setf temp-string "")))
;;       (setf previous-char char))
;;     (format t "commas=~D valid-pairs=~D comma-present=~S~%"
;;         num-commas num-valid-pairs comma-present)
;;     (or (and (not comma-present) (= num-valid-pairs 1)) (= (- num-valid-pairs 1) num-commas))))


;; (defun valid-pair-plus-comma-p (input-string)
;;   (let ((comma-present 'nil) (valid-pair 'nil) (temp-string ""))
;;     (iter (for char in-string input-string)
;;       (setf temp-string (concatenate 'string temp-string (string char)))
;;       (if (valid-key-value-pair-p temp-string) (setf valid-pair 't))
;;       (cond ((string= char ",")
;; 	     (setf comma-present 't))))
;;     (and valid-pair comma-present)))

;; (defun valid-key-value-pair-p (input-string)
;;   (let ((temp-string "")
;; 	(brackets-open 'nil)
;; 	(num-strings 0)
;; 	(valid-colon 'nil)
;; 	(valid-brackets (valid-brackets-p input-string)))
;;     (iter (for char in-string input-string)
;;       (setf temp-string (concatenate 'string temp-string (string char)))
;;       (cond ((string= char "{")
;; 	     (setf brackets-open 't))
;; 	    ((and brackets-open (valid-string-p temp-string) (< num-strings 2))
;; 	     (incf num-strings)
;; 	     (setf temp-string ""))
;; 	    ((and brackets-open (= num-strings 1) (string= char ":"))
;; 	     (setf valid-colon 't)
;; 	     (setf temp-string ""))
;; 	    ((and brackets-open (= num-strings 1) (valid-string-p temp-string))
;; 	     (incf num-strings)
;; 	     (setf temp-string ""))))
;;     (and (= num-strings 2) valid-colon valid-brackets)))


;; (defun valid-string-p (input-string)
;;   (let ((string-opened-p 'nil) (string-found-p 'nil))
;;     (iter (for char in-string input-string)
;;       (cond ((and (not string-opened-p) (string= char "\""))
;; 	     (setf string-found-p 't)
;; 	     (setf string-opened-p 't))
;; 	    ((and string-opened-p (string= char "\""))
;; 	     (setf string-found-p 't)
;; 	     (setf string-opened-p 'nil))))
;;     (and string-found-p (not string-opened-p))))
	    

