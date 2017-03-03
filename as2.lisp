(defun fl-interp(E P)
  (fl-interp-help E P nil nil)
)


(defun fl-interp-help (E P N V)
  (cond 
  ((not (null (hasName E N))) (getVal E N V))
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
                ((hasFuncDef P f) 
                  ; check if the function in in P 
                  (let ( (fargs (getArgs (cdr (getFuncDef P f)) nil)) (fdef (getFunction (cdr (getFuncDef P f)))))
                    ;; (if (eq f userf)
                        ; make namelist and call fl-interp with it
                        (fl-interp-help fdef P (addNames nil fargs) (addValues nil arg P N V))
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





; get the value associated with the name
(defun getVal(name Nlist Vlist)
  (cond
  ((null name) nil)
  ((null Nlist) nil)
  ((eq name (car Nlist)) (car Vlist))
  (t (getVal name (cdr Nlist) (cdr Vlist)))
  )
)

; check if a name is in the name list
(defun hasName (name nameList)
  (cond
  ((null name) nil)
  ((null nameList) nil)
  ((eq name (car nameList)) (car nameList))
  (t (hasName name (cdr nameList) ))
  )
)



(defun addNames(ExistingNames names)
  (append ExistingNames names) 
)

;evaluate values then add to values list
(defun addValues(ExistingVals vals P N V)
  (if (null vals)
    ExistingVals
    (addValues (append ExistingVals (list (fl-interp-help (car vals) P N V))) (cdr vals) P N V)
   ) 
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

(defun hasFuncDef(P fname)
  (cond
    ((null P) nil)
    ((eq (caar P) fname) t)
    (t (hasFuncDef (cdr P) fname))
  )
)

(defun getFuncDef(P fname)
  (cond
    ((null P) nil)
    ((eq (caar P) fname) (car P))
    (t (getFuncDef (cdr P) fname))
  )
)
