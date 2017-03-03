; Brian Ofrim (1374053) 
; CMPUT 325 Assignment 2 

; An iterpreter for the fl functional language
; E is a funciton application
; P is a list or user defined funcitions

;; test cases:
;;   (fl-interp '(* 5 9) nil) ; > '45
;;   (fl-interp '(or t nil) nil) ; > 't
;;   (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) ; > '12
;;   (fl-interp '(greater 3 5) '((greater x y = (if (> x y) x (if (< x y) y nil))))) 
;;   (fl-interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1))))))) ; > '24
;;   (fl-interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) ; > '6

(defun fl-interp(E P)
  (fl-interp-help E P nil nil)
)

; A helper file for fl-interp, the same as fl-interp but maintains name (N) and value (V) lists
; E is a funciton application
; P is a list or user defined funcitions

;; test cases:
;;   (fl-interp '(* 5 9) nil nil nil) ; > '45
;;   (fl-interp '(or t nil) nil nil nil) ; > 't
;;   (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil nil nil) ; > '12
;;   (fl-interp '(greater 3 5) '((greater x y = (if (> x y) x (if (< x y) y nil)))) nil nil) 
;;   (fl-interp '(factorial 4) '((factorial x = (if (= x 1) 1 (* x (factorial (- x 1)))))) nil nil) ; > '24
;;   (fl-interp '(divide 24 4) '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1))))) nil nil) ; > '6


(defun fl-interp-help (E P N V)
  (cond 
  ;check if the name is in the name list
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
                ; made it this far without a match, check for the presense of user defined function definition
                ; check if the function is in P 
                ((hasFuncDef P f) 
                  ; retive and parse the user defined function for it's arguments and body
                  (let ( (fargs (getArgs (cdr (getFuncDef P f)) nil)) (fdef (getFunction (cdr (getFuncDef P f)))))
                        ; make a name list and a value list and call fl-interp with it and the user defined funtion
                        (fl-interp-help fdef P (addNames nil fargs) (addValues nil arg P N V))
                    )
                )
                ; no match found, probably a list or malformed input
                (t E)
        )
           )
        )
)
)


; get the value associated with the given name from the value list
; test case:
;   (getVal 'd '(f d e) '(1 2 3)) => 2
(defun getVal(name Nlist Vlist)
  (cond
  ((null name) nil)
  ((null Nlist) nil)
  ((eq name (car Nlist)) (car Vlist))
  (t (getVal name (cdr Nlist) (cdr Vlist)))
  )
)

; check if a name is in the nameList
; test case: 
;  (hasName 'd '(g r d)) => T
(defun hasName (name nameList)
  (cond
  ((null name) nil)
  ((null nameList) nil)
  ((eq name (car nameList)) (car nameList))
  (t (hasName name (cdr nameList) ))
  )
)


; add names to the name list
;test case:
; (addNames '(a b c) '(e f)) => (a b c e f)
(defun addNames(ExistingNames names)
  (append ExistingNames names) 
)

;evaluate values then add them to the ExistingVals list
(defun addValues(ExistingVals vals P N V)
  (if (null vals)
    ExistingVals
    (addValues (append ExistingVals (list (fl-interp-help (car vals) P N V))) (cdr vals) P N V)
   ) 
)

;get the argument names from a user defined function definition
;; test cases:
;;    (getArgs '((simpleinterest x y z = (* x (* y z))))) => (x y z)
(defun getArgs(H argL)
  (if (eq (car H) '=)
    argL
    (getArgs (cdr H) (append argL (list (car H))))
  )
)

; get the body of a function from a function definition
;test cases:
;  (getFunction  '((square x = (* x x))))) => (* x x)
(defun getFunction(H)
  (if (eq (car H) '= )
  (cadr H)
    (getFunction (cdr H))
  )
)

;check to see if the user defined functions list (P) has a given funtion name
;test cases:
;   (hasFuncDef '((square x = (* x x))) 'square) => T
(defun hasFuncDef(P fname)
  (cond
    ((null P) nil)
    ((eq (caar P) fname) t)
    (t (hasFuncDef (cdr P) fname))
  )
)

;get function with function name fname from functio list P
;test cases: 
;   (getFuncDef '((divide x y = (div x y 0)) (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1))))) 'div)
;       => (div x y z = (if (> (* y z) x) (- z 1) (div x y (+ z 1))))
(defun getFuncDef(P fname)
  (cond
    ((null P) nil)
    ((eq (caar P) fname) (car P))
    (t (getFuncDef (cdr P) fname))
  )
)
