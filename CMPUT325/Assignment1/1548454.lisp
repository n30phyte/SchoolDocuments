; QUESTION 1
; Returns the number of atoms appearing in a possibly nested list L.
(defun xcount (L)
  (if (null L)
      0
      (if (atom (car L))
          (if (null (car L))
              (xcount (cdr L))
              (+ 1 (xcount (cdr L))))
          (+ (xcount (car L)) (xcount (cdr L))))))


; QUESTION 2
; Returns a flattened form of x with atoms on a single level
(defun flatten (x)
  (if (null x)
      nil
      (if (atom (car x))
          (cons (car x) (flatten (cdr x)))
          (append (flatten (car x)) (flatten (cdr x))))))


; QUESTION 3
; Removes repeated atoms in list. Copy removed doesn't mater, order preserved.
(defun remove-duplicate (x)
  (if (null x)
      nil
      (if (<= 1 (count-atoms (cdr x) (car x)))
          (remove-duplicate (cdr x))
          (cons (car x) (remove-duplicate (cdr x))))))
; Helper for question 3
; Count the number of times atom x appears in list L
(defun count-atoms (L x)
  "Helper function for question 3, counts the number of times atom x shows up in list L"
  (if (null L)
      0
      (if (eq x (car L))
          (+ 1 (count-atoms (cdr L) x))
          (count-atoms (cdr L) x))))


; QUESTION 4 a
; Mix elements of L1 and L2 into a single new list, alternating between lists.
; If one list is horter than the other, append the remaining elements from the longer list.
(defun mix (L1 L2)
  (if (not (or (null L1) (null L2)))
      (cons (car L1) (cons (car L2) (mix (cdr L1) (cdr L2))))
      (if (null L1)
          L2
          L1)))


; QUESTION 4 b
; Returns two lists, the first is elements of input L at odd positions
; and the second is elements of input L at even positions
(defun split (L)
  (if (null L)
      '(nil nil)
      (list (get-odd L) (get-even L))))

; Helper functions for Question 4 b
; Returns the odd items in the list
(defun get-odd (L)
  (if (and (not (null L)) (not (null (car L))))
      (cons (car L) (get-odd (cddr L)))))
; Returns the even items in the list
(defun get-even (L)
  (if (and (not (null L)) (not (null (cadr L))))
      (cons (cadr L) (get-even (cddr L)))))

(defun split (L)
  (if (null L)
      '(nil nil)
      (list (get-odd L) (get-even L))))

; QUESTION 5
(defun find-subset (L)
  (declare (optimize (debug 3)))
  (if (not (null L))
      (cons (cons (cdr L) (find-subset (cdr L))) (find-subset (cdr L)))))

(defun allsubsets (L)
  (find-subset (remove-duplicate L)))