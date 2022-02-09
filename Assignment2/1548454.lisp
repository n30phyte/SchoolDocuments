(defun fl-interp (E P)
  (cond
   ((atom E) E)
   (t
    (let ((funcname (car E))
          (arg (cdr E)))
      (cond
       ; handle built-in functions
       ((eq funcname 'first) (car (fl-interp (car arg) P)))
       ((eq funcname 'rest) (cdr (fl-interp (car arg) P)))
       ((eq funcname 'null) (null (fl-interp (car arg) P)))
       ((eq funcname 'atom) (atom (fl-interp (car arg) P)))
       ((eq funcname 'not) (not (fl-interp (car arg) P)))
       ((eq funcname 'number) (numberp (fl-interp (car arg) P)))

       ((eq funcname 'eq) (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq funcname 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ; if f is a user-defined function,
       ;    then evaluate the arguments 
       ;         and apply f to the evaluated arguments 
       ;             (applicative order reduction) 
       ((mapcar #'fl-interp arg))
       ; f is undefined
       (t E))))))

(fl-interp '(rest (1 2 3 4)) nil)
(fl-interp '(null ()) nil)
(fl-interp '(null (a)) nil)
(fl-interp '(atom a) nil)
(fl-interp '(atom (a)) nil)
(fl-interp '(number 2) nil)
(fl-interp '(not ()) nil)
(fl-interp '(not nil) nil)
(fl-interp '(not T) nil)
(fl-interp '(eq 2 2) nil)
(fl-interp '(eq (2) (2)) nil)
(fl-interp '(equal (2) (2)) nil)
(fl-interp '(cons 2 2) nil)
(fl-interp '(+ 2 (* 2 4)) nil)
(fl-interp '(- 2 2) nil)
(fl-interp '(* 2 2) nil)
(fl-interp '(> 2 2) nil)
(fl-interp '(< 2 2) nil)
(fl-interp '(= 2 2) nil)
(fl-interp '(and T nil) nil)
(fl-interp '(or T nil) nil)
(fl-interp '(count (1 2 3 4)) ((count (L) = (if (null L)
                                                0
                                                (+ 1 (count (rest L)))))))
