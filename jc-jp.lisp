(defpackage #:jc-jp
  (:use #:common-lisp
	#:iterate)
  (:export #:main))

(in-package #:jc-jp)

;; Program to parse JSON
;; Reports to Standard Output a suitable message
;; Exit with Code 0 for success and Code 1 for invalid

;; Testing
(defparameter *input-file* "./tests/step1/valid.json")
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


(defun valid-multiple-pairs-p (input-string)
  ;; Totally broken
  (let ((temp-string "") (previous-char "") (comma-present 'nil) (num-commas 0) (num-valid-pairs 0))
    (iter (for char in-string input-string)
      (setf temp-string (concatenate 'string temp-string (string char)))
      (cond ((and (string= previous-char ",") (valid-pair-plus-comma-p temp-string))
	     (format t "plus-comma: ~D" temp-string)
	     (incf num-commas)
	     (setf comma-present 't)
	     (incf num-valid-pairs)
	     (setf temp-string ""))
	    ((valid-key-value-pair-p temp-string)
	     (format t "no-comma: ~D" temp-string)
	     (incf num-valid-pairs)
	     (setf temp-string "")))
      (setf previous-char char))
    (format t "commas=~D valid-pairs=~D comma-present=~S~%"
        num-commas num-valid-pairs comma-present)
    (or (and (not comma-present) (= num-valid-pairs 1)) (= (- num-valid-pairs 1) num-commas))))


(defun valid-pair-plus-comma-p (input-string)
  (let ((comma-present 'nil) (valid-pair 'nil) (temp-string ""))
    (iter (for char in-string input-string)
      (setf temp-string (concatenate 'string temp-string (string char)))
      (if (valid-key-value-pair-p temp-string) (setf valid-pair 't))
      (cond ((string= char ",")
	     (setf comma-present 't))))
    (and valid-pair comma-present)))

(defun valid-key-value-pair-p (input-string)
  (let ((temp-string "")
	(brackets-open 'nil)
	(num-strings 0)
	(valid-colon 'nil)
	(valid-brackets (valid-brackets-p input-string)))
    (iter (for char in-string input-string)
      (setf temp-string (concatenate 'string temp-string (string char)))
      (cond ((string= char "{")
	     (setf brackets-open 't))
	    ((and brackets-open (valid-string-p temp-string) (< num-strings 2))
	     (incf num-strings)
	     (setf temp-string ""))
	    ((and brackets-open (= num-strings 1) (string= char ":"))
	     (setf valid-colon 't)
	     (setf temp-string ""))
	    ((and brackets-open (= num-strings 1) (valid-string-p temp-string))
	     (incf num-strings)
	     (setf temp-string ""))))
    (and (= num-strings 2) valid-colon valid-brackets)))


(defun valid-string-p (input-string)
  (let ((string-opened-p 'nil) (string-found-p 'nil))
    (iter (for char in-string input-string)
      (cond ((and (not string-opened-p) (string= char "\""))
	     (setf string-found-p 't)
	     (setf string-opened-p 't))
	    ((and string-opened-p (string= char "\""))
	     (setf string-found-p 't)
	     (setf string-opened-p 'nil))))
    (and string-found-p (not string-opened-p))))
	    

(defun valid-brackets-p (input-string)
  (let ((curly-nesting-level 0) (token-found-p 'nil))
    (iter (for char in-string input-string)
      (cond ((string= char "{")
	     (incf curly-nesting-level)
	     (setf token-found-p 't))
	    ((string= char "}")
	     (decf curly-nesting-level)
	     (setf token-found-p 't))))
    (and token-found-p (= curly-nesting-level 0))))
