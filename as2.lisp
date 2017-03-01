
(defun fl-interp (E P)
  (cond 
	((atom E) E)   ;this includes the case where expr is nil  
        (t
           (let ( (f (car E))  (arg (cdr E)) )
	      (cond 
                ; handle built-in functions
                ((eq f 'first) (car (fl-interp (car arg) P)))
                ((eq f 'rest)  (cdr (fl-interp (car arg) P)))
                ((eq f '+)  (fl-interp (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P) ) P))
                ((eq f '-)  (fl-interp (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f '*)  (fl-interp (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f '/)  (fl-interp (/ (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f '<)  (fl-interp (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f '>)  (fl-interp (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f '=)  (fl-interp (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f 'or) (fl-interp (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f 'and)  (fl-interp (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f 'not)  (fl-interp (not (fl-interp (car arg) P)) P))
                ((eq f 'isnumber)  (fl-interp (numberp (fl-interp (car arg) P)) P))
                ((eq f 'equal)  (fl-interp (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                ((eq f 'if)  (if (fl-interp (car arg) P) (fl-interp (cadr arg) P)  (fl-interp (caddr arg) P)))
                ((eq f 'null)  (fl-interp (null (fl-interp (car arg) P)) P))
                ((eq f 'atom)  (fl-interp (atom (fl-interp (car arg) P)) P))
                ((eq f 'eq)  (fl-interp (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P) ) P))
                ((eq f 'cons)  (fl-interp (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)) P))
                (t E)
        )

           )
        )
  )
)



	        ; if f is a user-defined function,
                ;    then evaluate the arguments 
                ;         and apply f to the evaluated arguments 
                ;             (applicative order reduction) 

                ; otherwise f is undefined; in this case,
                ; E is returned as if it is quoted in lisp