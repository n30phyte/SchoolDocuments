;;;; Assignment 2
;;;; Allowed functions:
;;;; atom, null, eq, equal, numberp, append, car, first, cdr, rest, cons, if, cond, let, let*
;;;; defun, quote, mapcar, reduce, lambda, funccall, apply, list, sort, progn, print, abs, eval

(defun contains-atom (L x)
  "Returns T if atom is in list, nil otherwise"
  (if (null L)
      nil
      (if (eq x (car L))
          T
          (contains-atom (cdr L) x))))

(defun func-map-aor (E P ctx)
  "map each argument of the function to perform Applicative Order Reduction"
  (mapcar #'(lambda (f) (fl-interp-ctx f P ctx)) E))

(defun make-kvp (Lk Lv)
  "Make KVP List with Lk as keys and Lv as values"
  (mapcar #'(lambda (k v) (list k v)) Lk Lv))

(defun get-value (kvp key)
  "Return value stored in KVP list defined by key. Can also be used to find function definitions"
  (if (null kvp)
      nil
      (if (eq key (caar kvp))
          (cdar kvp)
          (get-value (cdr kvp) key))))

(defun get-func-args (func-def)
  "Build a list of argument names"
  (if (eq (car func-def) '=)
      ; Hit the end of the arg list
      ()
      (append (car func-def) (get-func-args (cdr func-def)))))

(defun get-func-body (func-def)
  "Return function body from function definition"
  (if (eq (car func-def) '=)
      ; Hit the end of the arg list
      (cdr func-def)
      (get-func-body (cdr func-def))))

(defun fl-interp (E P)
  "Main interpreter function without context"
  (fl-interp-ctx E P nil))

(defun fl-interp-ctx (E P C)
  "E is the expression to interpret, P is any stored program and C is a list of variables and their associated value (context)"

  (cond
   ; Check if atom in context, if it is: return value stored in C
   ((not (null (get-value C E))) (car (get-value C E)))
   ((atom E) E)
   (t
    (let ((funcname (car E))
          (arg (cdr E)))

      (cond
       ;; handle built-in functions
       ((eq funcname 'first) (car (fl-interp-ctx (car arg) P C)))
       ((eq funcname 'rest) (cdr (fl-interp-ctx (car arg) P C)))
       ((eq funcname 'number) (apply 'numberp (func-map-aor arg P C)))

       ((contains-atom '(null atom not eq equal cons + - * < > =) funcname) (apply funcname (func-map-aor arg P C)))

       ((eq funcname 'and) (if (fl-interp-ctx (car arg) P C)
                               (if (fl-interp-ctx (cadr arg) P C)
                                   t
                                   nil)
                               nil))

       ((eq funcname 'or) (if (fl-interp-ctx (car arg) P C)
                              t
                              (if (fl-interp-ctx (cadr arg) P C)
                                  t
                                  nil)))

       ((eq funcname 'if) (if (fl-interp-ctx (car arg) P C)
                              (fl-interp-ctx (cadr arg) P C)
                              (fl-interp-ctx (caddr arg) P C)))

       ; if f is a user-defined function,
       ((not (null (get-value P funcname)))
        (fl-interp-ctx
         (car (get-func-body (get-value P funcname))) P
         (append (make-kvp (get-func-args (get-value P funcname)) (func-map-aor arg P C)) C))) ; add variables

       ; f is undefined
       (t E))))))


; ;;; Tests

; ;; Primitives

; (fl-interp '(rest (1 2 (3))) nil)
; (fl-interp '(rest (p 1 2 (3))) nil)
; (fl-interp '(first (rest (1 (2 3)))) nil)
; (fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil)
; (fl-interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil)
; (fl-interp '(if (> 1 0) (if (eq 1 2) 3 4) 5) nil)
; (fl-interp '(cons (first (1 2 3))  (cons a nil)) nil)
; (fl-interp '(and (or T  nil) (> 3 4)) nil)
; (fl-interp '(eq (1 2 3) (1 2 3)) nil)
; (fl-interp '(equal (1 2 3) (1 2 3)) nil)
; (fl-interp '(+ 1 2) nil)

; ;; Programmed

; (fl-interp '(f (f 2))
;   '( (f (X) =  (* X X)) ))
; (fl-interp '(a (+ 1 2))
;   '( (a (X) = (+ X 1)) ))
; (fl-interp '(b (+ 1 2))
;   '( (b (X) = (+ X 1)) ))

; (set 'E '(reverse (1 2 3)))
; (set 'P
;   '((reverse (X) = (if (null X)
;                       nil
;                       (append (reverse (rest X))
;                               (cons (first X) nil))))
;    (append (X Y) = (if (null X)
;                        Y
;                        (cons (first X) (append (rest X) Y))))
;    (count (L) = (if (null L)
;                     0
;                     (+ 1 (count (rest L)))))))
; (fl-interp E P)

; (fl-interp '(count (append (a b c) (1 2 3))) P)
; ;; Provided test cases
; ;; Primitives

; (fl-interp '(+ 10 5) nil) ; > 15
; (fl-interp '(- 12 8) nil) ; > 4
; (fl-interp '(* 5 9) nil) ; > 45
; (fl-interp '(> 2 3) nil) ; > nil
; (fl-interp '(< 1 131) nil) ; > t
; (fl-interp '(= 88 88) nil) ; > t 
; (fl-interp '(and nil t) nil) ; > nil
; (fl-interp '(or t nil) nil) ; > t
; (fl-interp '(not t) nil) ; > 'nil
; (fl-interp '(number 354) nil) ; > t
; (fl-interp '(equal (3 4 1) (3 4 1)) nil) ; > t
; (fl-interp '(if nil 2 3) nil) ; > 3
; (fl-interp '(null ()) nil) ; > t
; (fl-interp '(atom (3)) nil) ; > nil
; (fl-interp '(eq x x) nil) ; > t
; (fl-interp '(first (8 5 16)) nil) ; > 8
; (fl-interp '(rest (8 5 16)) nil) ; > (5 16)
; (fl-interp '(cons 6 3) nil) ; > (6 . 3)

; ;; Complex primitives
; (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) ; > 12
; (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2)) (not (= 3 2)))) nil) ; > t
; (fl-interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil) ; > nil
; (fl-interp '(if (not (null (first (a c e)))) (if (number (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) ; > ((a c e) . d)

(fl-interp '(count (append (a b c) (1 2 3)) ) P)
