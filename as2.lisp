(defun fl-interp(E P)
  (fl-interp-help E P nil nil)
)


(defun fl-interp-help (E P N V)
  (cond 
  ((not (null (getVal E N V))) (getVal E N V))
	((atom E) E)   ;this includes the case where expr is nil  
        (t
           (let ( (f (car E))  (arg (cdr E)) )
	      (cond 
                ; handle built-in functions
                ((eq f 'first) (car (fl-interp-help (car arg) P N V)))
                ((eq f 'rest)  (cdr (fl-interp-help (car arg) P N V)))
                ((eq f '+)  (fl-interp-help (+ (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V) ) P N V))
                ((eq f '-)  (fl-interp-help (- (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f '*)  (fl-interp-help (* (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f '/)  (fl-interp-help (/ (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f '<)  (fl-interp-help (< (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f '>)  (fl-interp-help (> (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f '=)  (fl-interp-help (= (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f 'or) (fl-interp-help (or (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f 'and)  (fl-interp-help (and (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f 'not)  (fl-interp-help (not (fl-interp-help (car arg) P N V)) P N V))
                ((eq f 'isnumber)  (fl-interp-help (numberp (fl-interp-help (car arg) P N V)) P N V))
                ((eq f 'equal)  (fl-interp-help (equal (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ((eq f 'if)  (if (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)  (fl-interp-help (caddr arg) P N V)))
                ((eq f 'null)  (fl-interp-help (null (fl-interp-help (car arg) P N V)) P N V))
                ((eq f 'atom)  (fl-interp-help (atom (fl-interp-help (car arg) P N V)) P N V))
                ((eq f 'eq)  (fl-interp-help (eq (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V) ) P N V))
                ((eq f 'cons)  (fl-interp-help (cons (fl-interp-help (car arg) P N V) (fl-interp-help (cadr arg) P N V)) P N V))
                ; made it this far without a match, check for the presense of input function
                ((eq f (caar P)) 
                  ; check if the function in in P 
                  (let ( (fargs (getArgs (cdar P) nil)) (fdef (getFunction (cdar P))))
                    ;; (if (eq f userf)
                        ; make namelist and call fl-interp with it
                        (fl-interp-help fdef P (addNames N fargs) (addValues V arg))
                      ;; )
                    )
                )
                ;check if the name is in the name list
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

; get the value associated with the name
(defun getVal(name Nlist Vlist)
  (cond
  ((null name) nil)
  ((null Nlist) nil)
  ((eq name (car Nlist)) (car Vlist))
  (t (getVal name (cdr Nlist) (cdr Vlist)))
  )
)


(defun addNames(ExistingNames names)
  (append ExistingNames names) 
)

(defun addValues(ExistingVals vals)
  (append ExistingVals vals)
)

(defun getArgs(H argL)
  (if (eq (car H) '=)
    argL
    (getArgs (cdr H) (append argL (list (car H))))
  )
)

(defun getFunction(H)
  (if (eq (car H) '= )
  (cadr H)
    (getFunction (cdr H))
  )
)
