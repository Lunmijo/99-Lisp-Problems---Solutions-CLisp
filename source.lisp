;; 1 Find the last box of a list.
(setq new_list '(a b c k)) ;creating a list
(print "Problem 1: solving")
(print new_list)
(print "The last one is:")
(print (last new_list)) ;last - last element of list instead of cdr - cdr return all elements after the first one

;; 2 Find the last but one box of a list.
(print "Problem 2: solving")
(print new_list)
(setq reversed_list (reverse new_list))
(print "Reversed list: ")
(print reversed_list)
(print (nth 0 reversed_list))
(print (nth 1 reversed_list))

;; 3 Find the K'th element of a list.
(print "Problem 3: solving:")
(print "2'th element:")
(print (nth 2 new_list))

;; 4 Find the number of elements of a list.
(print "Problem 4: solving")
(defun len (new_list)
	(if (null new_list)
		0
	(1+(len (cdr new_list))) ; recursion
	)
)
(print(len new_list))


;; 5 Reverse a list
(print "Problem 5: solving")
(print new_list)
(setq reversed_list_task_5 (reverse new_list))
(print "Reverse list: ")
(print reversed_list_task_5)

;; 6 Find out whether a list is a palindrome.
(print "Problem 6: solving")
(setq value "NEPALINDROM")
(defun palindrome (value)
	(equal value (reverse value)) )
(print (palindrome value))

;; 7  Flatten a nested list structure.
(print "Problem 7: solving")
(setq list_to_flat '(a (b (c d) e)))
(print list_to_flat)
(defun flatten (list_to_flat)
    (if (null list_to_flat)
        nil
        (if (atom (car list_to_flat))
            (cons (car list_to_flat) (flatten (cdr list_to_flat)))
            (append (flatten (car list_to_flat)) (flatten (cdr list_to_flat))))))
(setq flatten_list (flatten list_to_flat))
(print flatten_list)

;; 8 Eliminate consecutive duplicates of list elements.
(print "Problem 8: solving")
(setq list_with_dups '(a a b b c c k k)) ;creating a list
(defun comptemps (list_with_dups)
  (cond
   ((null list_with_dups) nil)
   ((null (cdr list_with_dups)) list_with_dups)
   ((eql (first list_with_dups) (car (cdr list_with_dups)))
    (comptemps (cdr list_with_dups)))
   (t (cons (car list_with_dups) (comptemps (cdr list_with_dups))))
   )
  )
(setq list_without_dups (comptemps list_with_dups))
(print list_with_dups)
(print list_without_dups) 

;; 9 Pack consecutive duplicates of list elements into sublists.
(print "Problem 9: solving")
(setq l '(8 i i s K K K p p p p hh hh hh))
(print l)

(defun pack (list)
  (if (cdr list)
      (let ((temp (pack (cdr list))))
      (if (equal (car list) (car (cdr list)))
        (cons (cons (car list) (car temp)) (cdr temp))
        (cons (list (car list)) temp)))
      (when list (list list))))
(print (pack l))

;; 10 Run-length encoding of a list.
(print "Problem 10: solving")
(setq l '(8 i i s K K K kkk p p p p hh hh hh))
(print l)

(defun encode (list)
  (mapcar (lambda (sublist) (list (len sublist) ; len - task 4 --- mapcar - выполняет некую функцию ко всем эл-там списка, 
        (car sublist)))
          (pack list))) ; pack - task 9
(print (encode l))

; 11 Modified run-length encoding.
(print "Problem 11: solving")
(setq l '(8 i i s K K K kkk p p p p hh hh hh))
(print l)
(defun encode_without_dups (list)
  (mapcar (lambda (sublist)
    (if (equal (car sublist) 1)
      (car (cdr sublist))
        sublist))
      (encode list)))
(setq encoded_list (encode_without_dups l))
(print encoded_list)

; 12 Decode a run-length encoded list.
(print "Problem 11: solving")
(print encoded_list)



