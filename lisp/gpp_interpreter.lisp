(load "gpp_lexer.lisp")
(defparameter stack (list))
(defparameter final-elements (list "CP" "KW_TRUE" "KW_FALSE" "VALUEF" "ID" "FCALL" "ASG"))
(defparameter rule-flag 0)
(defparameter current-rule (list))
(defparameter eval-flag 0)
(defparameter up (list))
(defparameter down (list))
(defparameter idname (list))
(defparameter idvalue (list))
(defparameter current-id (list))
(defparameter gcd 1)
(defparameter functions (list))
(defparameter function-t (list))
(defparameter paranthesis 0)

; Reset all global variables
(defun reset-interpreter-var()
    (setq rule-flag 0)
    (setq eval-flag 0)
    (setq current-rule nil)
    (setq stack nil)
    (setq up nil)
    (setq down nil)
    (setq current-id nil)
    (setq paranthesis 0)
    (setq function-t nil)
)

; Read token list coming from lexer one by one
(defun read-tokens(index)
    (if (not (eq (length ls) index))
        (progn
            (check-final 0 (nth 1 (nth index ls)))
            (if (string= (nth 0 (nth index ls)) "(")
                (incf paranthesis)
            )
            (if (eq eval-flag 0)
                (push (nth 1 (nth index ls)) stack)
                (final-elements-eval index)
            )
            (read-tokens (1+ index))
        )
        (progn
            (if (eq (length stack) 1)
                (progn
                    (terpri)
                    (princ "SYNTAX OK!")
                    (terpri)
                    (format T "Result = ~D / ~D" (car up) (car down))
                )
                (error-func)
            )
            (terpri)
            (reset-interpreter-var)
        )
    )
)

; Check if the current token is a final element
; Final elements needs to be evaluated and pushed to stack according to shift/reduce result
(defun check-final(i token)
    (if (not (eq (length final-elements) i))
        (progn
            (if (string= (nth i final-elements) token)
                (setq eval-flag 1)
                (check-final (1+ i) token)
            )  
        )
        (setq eval-flag 0)
    )
)

; If token is final element, then evaluate it
(defun final-elements-eval(index)
    ; Push fractions as expression and extract the value of it
    (if (string= (nth 1 (nth index ls)) "VALUEF")
        (progn
            (push "EXP" stack)
            (extract index)
        )
    )
    ; ID has some exceptions
    ; If it is only ID, then push as expression by finding its value from the table
    ; If it is a part of fcall/set/defvar rules then push as ID
    ; Id it is a part of deff rules, then count the paranthesis to push as either EXP or ID
    ; For the rest of ID's, push as expression
    (if (string= (nth 1 (nth index ls)) "ID")
        (progn
            (if (= 1 (length ls))
                (progn
                    (push "EXP" stack)
                    (find-id (nth 0 (nth index ls)) 0)
                )
                (progn
                    (if (or (string= (nth 1 (nth (1- index) ls)) "OP")
                            (string= (nth 1 (nth 1 ls)) "OP_SET")
                            (string= (nth 1 (nth 1 ls)) "DEFV")
                            (string= (nth 1 (nth 1 ls)) "DEFF"))
                        (progn
                            (if (not (string= (nth 1 (nth 1 ls)) "DEFF"))
                                (progn
                                    (if (and (string= (nth 1 (nth (1- index) ls)) "OP") (eq (check-func 0 (nth 0 (nth index ls))) -1))
                                        (progn
                                            (push "EXP" stack)
                                            (push (nth 0 (nth index ls)) current-id)
                                            (find-id (nth 0 (nth index ls)) 0)
                                        )
                                        (progn
                                            (push (nth 0 (nth index ls)) current-id)
                                            (push "ID" stack)
                                        )
                                    )
                                )
                                (progn
                                    (if (< 2 paranthesis)
                                        (push "EXP" stack)
                                        (push "ID" stack)
                                    )
                                )
                            )
                        )
                        (progn
                            (push "EXP" stack)
                            (push (nth 0 (nth index ls)) current-id)
                            (find-id (nth 0 (nth index ls)) 0)
                        )
                    )
                )
            )
        )
    )
    ; FCALL and ASG are pushed as EXP because of reduction
    (if (or (string= (nth 1 (nth index ls)) "FCALL") (string= (nth 1 (nth index ls)) "ASG"))
        (push "EXP" stack)
    )
    ; KW_TRUE and KW_FALSE are reduced to EXPB
    (if (or (string= (nth 1 (nth index ls)) "KW_TRUE") (string= (nth 1 (nth index ls)) "KW_FALSE"))
        (progn
            (if (string= (nth 1 (nth index ls)) "KW_TRUE")
                (progn 
                    (push 1 up)
                    (push 1 down)
                )
                (progn
                    (push 0 up)
                    (push 1 down)
                )
            )
            (push "EXPB" stack)
        )
    )
    ; CP generally indicates the end of the rule (deff has an exception)
    ; Therefore, it starts evaluation to find the rule
    (if (string= (nth 1 (nth index ls)) "CP")
        (progn
            (if (and (string= (nth 1 (nth 1 ls)) "DEFF") (< paranthesis 3))
                    (push "CP" stack) 
                (progn
                    (push "CP" stack)
                    (rule-check)            
                )
            )
        )
    )

)

; Check all rules to find the correct one
(defun rule-check()
    (if (eq rule-flag 0)
        (FUNCTION-RULE )
    )
    (if (eq rule-flag 0)
        (EXPLIST-RULE )
    )
    (if (eq rule-flag 0)
        (EXP-RULE )
    )
    (if (eq rule-flag 0)
        (EXPB-RULE )
    )
    (if (eq rule-flag 0)
        (ASG-RULE )
    )
    (if (eq rule-flag 0)
        (FCALL-RULE )
    )

    (if (eq rule-flag 1)
        (if (= (check-func 0 (nth 0 (nth 2 ls))) -1)
            (progn
                (setq function-t ls)
                (push function-t functions)
                (setq function-t (list)) 
                (push 0 up)
                (push 1 down)
            )
        )
    )

    (if (not (eq rule-flag 0))
        (update-stack)
        (progn
            (if (not (string= "DEFF" (nth (- (length stack) 2) stack)))
                (error-func)
            )
        )
    )
)

; If the correct rule is found, then reduce the rule
(defun update-stack()
    (case rule-flag
        (1(update-ext 0 "FUNCTION"))
        (2(update-ext 0 "EXPLIST"))
        (3(update-ext 0 "EXP"))
        (4(update-ext 0 "EXPB"))
        (5(update-ext 0 "ASG"))
        (6(update-ext 0 "FCALL"))
    )

    (if (eq rule-flag 2)
        (progn
            (setf temp-up (car up))
            (setf temp-down (car down))
            (setf up (cdr (cdr up)))
            (setf down (cdr (cdr down)))
            (push temp-up up)
            (push temp-down down)
        )
    )

    (setq rule-flag 0)
    (setq current-rule nil)

)

; Remove the tokens which belong to the rule and push the token of the rule
(defun update-ext(index token)
    (if (not (eq (length current-rule) index))
        (progn 
            (setf stack (cdr stack))
            (update-ext (1+ index) token)
        )
        (push token stack)
    )
)

; Rule comparison part 
(defun FUNCTION-RULE()
    (setq rule1 (list "OP" "DEFF" "ID" "OP" "CP" "EXPLIST" "CP"))
    (setq rule2 (list "OP" "DEFF" "ID" "OP" "ID" "CP" "EXPLIST" "CP"))
    (setq rule3 (list "OP" "DEFF" "ID" "OP" "ID" "ID" "CP" "EXPLIST" "CP"))
    (setq rule4 (list "OP" "DEFF" "ID" "OP" "ID" "ID" "ID" "CP" "EXPLIST" "CP"))
    (rule-compare rule1 0 1 ) 
    (rule-compare rule2 0 1 ) 
    (rule-compare rule3 0 1 ) 
    (rule-compare rule4 0 1 ) 
)

(defun EXPLIST-RULE()
    (setq rule1 (list "OP" "EXP" "EXP" "CP"))
    (setq rule2 (list "OP" "EXP" "EXP" "EXP" "CP"))
    (setq rule3 (list "OP" "EXP" "EXP" "EXP" "EXP" "CP"))
    (rule-compare rule1 0 2 ) 
    (rule-compare rule2 0 2 ) 
    (rule-compare rule3 0 2 ) 
)

(defun EXP-RULE()
    (setq rule1 (list "OP" "OP_PLUS" "EXP" "EXP" "CP"))
    (setq rule2 (list "OP" "OP_MINUS" "EXP" "EXP" "CP"))
    (setq rule3 (list "OP" "OP_MULT" "EXP" "EXP" "CP"))
    (setq rule4 (list "OP" "OP_DIV" "EXP" "EXP" "CP"))
    (setq rule5 (list "OP" "KW_IF" "EXPB" "EXPLIST" "EXPLIST" "CP"))
    (setq rule6 (list "OP" "KW_WHILE" "EXPB" "EXPLIST" "CP"))
    (setq rule7 (list "OP" "DEFV" "ID" "EXP" "CP"))
    (rule-compare rule1 0 3 ) 
    (rule-compare rule2 0 3 ) 
    (rule-compare rule3 0 3 ) 
    (rule-compare rule4 0 3 ) 
    (rule-compare rule5 0 3 ) 
    (rule-compare rule6 0 3 ) 
    (rule-compare rule7 0 3 ) 
)

(defun EXPB-RULE()
    (setq rule1 (list "OP" "OP_EQ" "EXP" "EXP" "CP"))
    (setq rule2 (list "OP" "OP_GT" "EXP" "EXP" "CP"))
    (setq rule3 (list "OP" "OP_AND" "EXPB" "EXPB" "CP"))
    (setq rule4 (list "OP" "OP_OR" "EXPB" "EXPB" "CP"))
    (setq rule5 (list "OP" "OP_NOT" "EXPB" "CP"))
    (rule-compare rule1 0 4 ) 
    (rule-compare rule2 0 4 ) 
    (rule-compare rule3 0 4 ) 
    (rule-compare rule4 0 4 ) 
    (rule-compare rule5 0 4 ) 
)

(defun ASG-RULE()
    (setq rule1 (list "OP" "OP_SET" "ID" "EXP" "CP"))
    (rule-compare rule1 0 5 ) 
)

(defun FCALL-RULE()
    (setq rule1 (list "OP" "ID" "CP"))
    (setq rule2 (list "OP" "ID" "EXP" "CP"))
    (setq rule3 (list "OP" "ID" "EXP" "EXP" "CP"))
    (setq rule4 (list "OP" "ID" "EXP" "EXP" "EXP" "CP"))
    (rule-compare rule1 0 6 ) 
    (rule-compare rule2 0 6 ) 
    (rule-compare rule3 0 6 ) 
    (rule-compare rule4 0 6 ) 
)


; Compare all elements in the rule to ones in the stack
; If there is a match, then set the rule-flag to the rule number
(defun rule-compare(rule index flag)
    (if (not (eq (length rule) index))
        (progn
            (if (string= (nth (- (1- (length rule)) index) rule) (nth index stack))
                (rule-compare rule (1+ index) flag)    
            )
        )
    )
    
    (if (eq (length rule) index) 
        (progn
            (setq rule-flag flag)
            (setq current-rule rule)
            (if (not (string= (nth 1 (nth 1 ls)) "DEFF"))
                (operation (nth 1 rule))
            )
        )
    )
)

; Greatest common divisor to simplify
(defun gcd-calculater(n m)
    (if (not (= m 0))
        (gcd-calculater m (mod n m))
        (setq gcd n)
    )
)

;Simplify the fraction
(defun simplify-up(num1 num2)
    (gcd-calculater num1 num2)
    (/ num1 gcd)
)

(defun simplify-down(num1 num2)
    (gcd-calculater num1 num2)
    (/ num2 gcd)
)

; Perform the operation according to operator in the rule
(defun operation(op)
    (if (string= "OP_PLUS" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (setq r-up (+ (* up1 down2) (* up2 down1)))
            (setq r-down (* down1 down2))

            (setq result-up (simplify-up r-up r-down))
            (setq result-down (simplify-down r-up r-down))

            (push result-up up)
            (push result-down down)
        )
    )
    (if (string= "OP_MINUS" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (setq r-up (- (* up1 down2) (* up2 down1)))
            (setq r-down (* down1 down2))

            (setq result-up (simplify-up r-up r-down))
            (setq result-down (simplify-down r-up r-down))

            (push result-up up)
            (push result-down down)
        )
    )
    (if (string= "OP_MULT" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            
            (setq r-up (* up1 up2))
            (setq r-down (* down1 down2))

            (setq result-up (simplify-up r-up r-down))
            (setq result-down (simplify-down r-up r-down))

            (push result-up up)
            (push result-down down)
        )
    )
    (if (string= "OP_DIV" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (setq r-up (* up1 down2))
            (setq r-down (* down1 up2))

            (setq result-up (simplify-up r-up r-down))
            (setq result-down (simplify-down r-up r-down))

            (push result-up up)
            (push result-down down)
        )
    )
    (if (string= "DEFV" op)
        (progn 
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (check-id (car current-id) up1 down1 1 0 0)
            (setq current-id (cdr current-id))

            (push up1 up)
            (push down1 down)
        )
    )
    (if (string= "OP_SET" op)
        (progn 
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (check-id (car current-id) up1 down1 0 0 0)
            (setq current-id (cdr current-id))

            (push up1 up)
            (push down1 down)
        )
    )
    (if (string= "KW_WHILE" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            
            (if (eq up1 1)
                (progn
                    (push up2 up)
                    (push down2 down)
                )
            )
        )
    )
    (if (string= "KW_IF" op)
        (progn 
            (setq up3 (car up))
            (setq down3 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            
            (if (eq up1 1)
                (progn
                    (push up2 up)
                    (push down2 down)
                )
                (progn
                    (push up3 up)
                    (push down3 down)
                )
            )
        )
    )
    (if (string= "OP_AND" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (if (and (eq up1 1) (eq up2 1))
                (progn
                    (push 1 up)
                    (push 1 down)
                )
                (progn
                    (push 0 up)
                    (push 1 down)
                )
            )
        )
    )
    (if (string= "OP_OR" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (if (or (eq up1 1) (eq up2 1))
                (progn
                    (push 1 up)
                    (push 1 down)
                )
                (progn
                    (push 0 up)
                    (push 1 down)
                )
            )
        )
    )
    (if (string= "OP_NOT" op)
        (progn 
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (if (eq up1 1)
                (progn
                    (push 0 up)
                    (push 1 down)
                )
                (progn
                    (push 1 up)
                    (push 1 down)
                )
            )
        )
    )
    ; Check if the first number is equal to the second one
    ; Use subtraction to find out
    (if (string= "OP_EQ" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (setq r-up (- (* up1 down2) (* up2 down1)))
            (setq r-down (* down1 down2))
            (setq result-up (simplify-up r-up r-down))
            
            (if (eq result-up 0)
                (progn
                    (push 1 up)
                    (push 1 down)
                )
                (progn
                    (push 0 up)
                    (push 1 down)
                )
            )
        )
    )
    ; Check if the first number is greater than the second one
    ; Use subtraction to find out
    (if (string= "OP_GT" op)
        (progn 
            (setq up2 (car up))
            (setq down2 (car down))
            (setq up (cdr up))
            (setq down (cdr down))
            (setq up1 (car up))
            (setq down1 (car down))
            (setq up (cdr up))
            (setq down (cdr down))

            (setq r-up (- (* up1 down2) (* up2 down1)))
            (setq r-down (* down1 down2))
            (setq result-up (simplify-up r-up r-down))
            
            (if (> result-up 0)
                (progn
                    (push 1 up)
                    (push 1 down)
                )
                (progn
                    (push 0 up)
                    (push 1 down)
                )
            )
        )
    )
    ; FCALL operations
    ; Fetch the function first
    ; Change the parameters with their values
    ; Extract the explist part
    ; Recall the parser for the explist part
    (if (string= "ID" op)
        (progn 
            (if (not (= (check-func 0 (nth 0 (nth 1 ls))) -1))
                (progn
                    (setq function-t (copy-tree (nth (check-func 0 (nth 0 (nth 1 ls))) functions)))
                    (setq param1 (find-param1))
                    (setq param2 (find-param2))
                    (setq param3 (find-param3))
                    (change-param 0 param1 param2 param3)
                    (setq ls (copy-tree function-t))
                    (reset-interpreter-var)
                    (extract-explist 0)
                    (setq ls (reverse ls))
                    (setq ls (cdr ls))
                    (setq ls (reverse ls))
                    (read-tokens 0)
                    (terpri)
                    (setq ls nil)
                    (get-input)
                )
                (error-func)
            )
        )
    )
    
)

; Get only the explist part of the function for evaluation
(defun extract-explist(op)
    (if (and (string= "(" (nth 0 (nth 0 ls))) (< op 2))
        (progn
            (setq ls (cdr ls))
            (extract-explist (1+ op))
        )
        (progn 
            (if (and (not (string= "(" (nth 0 (nth 0 ls)))) (< op 3))
                (progn
                    (setq ls (cdr ls))
                    (extract-explist op)
                )
            )
        )
    )
)

; Find the parameter's name of the function
(defun find-param1()
    (if (not (string= ")" (nth 0 (nth 4 function-t))))
        (nth 0 (nth 4 function-t))
        nil
    )
)
(defun find-param2()
    (if (and (not (string= ")" (nth 0 (nth 4 function-t)))) (not (string= ")" (nth 0 (nth 5 function-t)))))
        (nth 0 (nth 5 function-t))
        nil
    )
)
(defun find-param3()
    (if (and (not (string= ")" (nth 0 (nth 4 function-t)))) (not (string= ")" (nth 0 (nth 5 function-t))))
             (not (string= ")" (nth 0 (nth 6 function-t)))))
        (nth 0 (nth 6 function-t))
        nil
    )
)

; Change the parameters with their corresponding values
(defun change-param(i param1 param2 param3)
    (if (not (eq i (length function-t)))
        (progn
            (if (string= param1 (nth 0 (nth i function-t)))
                (progn
                    (setf val (concatenate 'string (write-to-string (nth (1- (length up)) up)) "f" (write-to-string (nth (1- (length down)) down))))
                    (setf (nth 0 (nth i function-t)) val)
                    (setf (nth 1 (nth i function-t)) "VALUEF")
                )
            )
            (if (string= param2 (nth 0 (nth i function-t)))
                (progn
                    (setf val (concatenate 'string (write-to-string (nth (- (length up) 2) up)) "f" (write-to-string (nth (- (length down) 2) down))))
                    (setf (nth 0 (nth i function-t)) val)
                    (setf (nth 1 (nth i function-t)) "VALUEF")
                )
            )
            (if (string= param3 (nth 0 (nth i function-t)))
                (progn
                    (setf val (concatenate 'string (write-to-string (nth (- (length up) 3) up)) "f" (write-to-string (nth (- (length down) 3) down))))
                    (setf (nth 0 (nth i function-t)) val)
                    (setf (nth 1 (nth i function-t)) "VALUEF")
                )
            )
            (change-param (1+ i) param1 param2 param3)
        )
    )
)

; Check if the given function is defined already
(defun check-func(i name)
    (if (not (eq i (length functions)))
        (progn
            (if (string= name (nth 0 (nth 2 (nth i functions))))
                i
                (check-func (1+ i) name)
            )
        )
        -1
    )
)

; Add the new ID to the table
(defun assign(name up down)
    (push name idname)
    (push (list up down) idvalue)
)

; Check if the given ID is already in the table
; If so, change its value
; If not, add it to the table
(defun check-id(name up down op i flag)
    (if (not (eq i (length idname)))
        (progn
            (if (string= name (nth i idname))
                (progn
                    (if (= op 0)
                        (progn
                            (setf (nth i idvalue) (list up down))
                            (setq flag 1)
                        )
                    )
                )
                (check-id name up down op (1+ i) flag)
            )
        )
        (progn
            (if (= flag 0)
                (assign name up down)
            )
        )
    )
)

; Find the given ID from the table
(defun find-id(name i)
    (if (not (eq i (length idname)))
        (progn
            (if (string= name (nth i idname))
                (progn
                    (push (nth 0 (nth i idvalue)) up)
                    (push (nth 1 (nth i idvalue)) down)
                    (setq current-id (cdr current-id))
                )
                (find-id name (1+ i))
            )
        )
        (error-func)
    )
)

(defun error-func()
    (terpri)
    (write-line "SYNTAX ERROR!")
    (terpri)
    (exit-prog)
)

; Exctract the value of fraction
(defun extract(index) 
    (setq f 0)
    (setq len (length (nth 0 (nth index ls))))
    
    (extract-rec 0 len (nth 0 (nth index ls)) 0 0 f)
)

(defun extract-rec(i len content u d f) 
    (if (< i len)
        (progn
            (if (and (eq f 0) (not (eq (char content i) #\f)))
                (progn
                    (setq u (* u 10))
                    (setq u (+ (digit-char-p (char content i)) u))
                )
            )
            (if (and (eq f 0) (eq (char content i) #\f))
                (setq f 1)
            )
            (if (and (eq f 1) (not (eq (char content i) #\f)))
                (progn
                    (setq d (* d 10))
                    (setq d (+ (digit-char-p (char content i)) d))
                )
            )
            (extract-rec (1+ i) len content u d f)
        )
        (progn
            (push u up)
            (push d down)
        )
    )
)

; Start the program
(defun gppinterpreter()
    (get-input)
    (gppinterpreter)
)

(defun exit-prog()
    (exit)
)

(gppinterpreter)
