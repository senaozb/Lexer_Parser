(defparameter tokens (list "OP_AND" "OP_OR" "OP_NOT" "OP_EQ" "OP_GT" 
              "OP_SET" "DEFF" "DEFV" 
              "KW_WHILE" "KW_IF" "KW_EXIT" "KW_TRUE" "KW_FALSE"
              "OP_PLUS" "OP_MINUS" "OP_DIV" "OP" "OP_MULT"
              "CP" "OP_COMMA"
              "VALUEF" "ID"))
(defparameter symbols (list "and" "or" "not" "eq" "gt" 
              "set" "deffun" "defvar"
              "while" "if" "exit" "true" "false"
              "+" "-" "/" "(" "*"
              ")" ","))

;Global variables for the program
(defparameter subinput "")
(defparameter sublist nil)
(defparameter int-process 0)
(defparameter id-process 0)
(defparameter fraction-process 0)
(defparameter checkskip 0)
(defparameter exceptions 0)
(defparameter ls (list))
(defparameter op 0)
(defparameter cp 0)


;;;;;;;;        --------  RULES FOR DFA'S  --------    ;;;;;;;;;;
; DFA FOR INTEGERS -> [numeric-ch-state]+
; DFA FOR FRACTIONS -> [numeric-ch-state]+ [alpha-ch-state] [numeric-ch-state]+
; DFA FOR IDENTIFIERS -> [alpha-ch-state] ([alpha-ch-state]|[numeric-ch-state])*




(defun alpha-ch-state()
    ; ------------     IDENTIFIER DFA   ------------ ;
    ; If it is an alpha character in identifier DFA 
    ; Then continue
    (if (and (= int-process 0) (= fraction-process 1))
        (progn
            (push subinput sublist)
            (setq checkskip 1)
        )
    )
    (if (and (= int-process 0) (= fraction-process 0))
        (progn
            (setq id-process 1)
            (push subinput sublist)
        )
    )
    ; ------------------------------------------------ ;

    ; ------------     FRACTION DFA     -------------- ;
    ; Check if the character 'f' is valid for fraction number DFA
    (if (and (= int-process 1) (char= subinput #\f) (= fraction-process 0))
        (progn 
            (push subinput sublist)
            (incf fraction-process)
            ; Start a new integer DFA
            (setq int-process 0)
            (setq exceptions 0)
        )
    )
    ; ------------------------------------------------ ;

    ; -------------     INTEGER DFA     -------------- ;
    ; If it is an alpha character (except f) in integer DFA 
    ; Then terminate the DFA
    (if (and (= int-process 1) (not (char= subinput #\f)))
        (progn 
            (push subinput sublist)
            (setq checkskip 1)
        )
    )
    ; ------------------------------------------------ ;
)


(defun numeric-ch-state()
    ; If it is a numeric character in both identifier and integer DFA's
    ; Then continue

    ; ------------     IDENTIFIER DFA   -------------- ;
    (if (= id-process 1)
        (push subinput sublist)
    )
    ; ------------------------------------------------ ;
    
    ; ---------     INTEGER/FRACTION DFA    ---------- ;
    (if (= id-process 0)
        (progn
            ; Integer cannot be started with 0, it should be a digit (0)
            ; To see, set "exceptions" 3
            (if (and (= int-process 0) (= (digit-char-p subinput) 0))
                (setq exceptions 3)
            )
            (setq int-process 1)
            (push subinput sublist)
        )
    )
    ; ------------------------------------------------ ;
)

(defun not-alphanumeric-state-eval()
    ; Start the symbol DFA if it is not space or newline characters
    (if (and (not (char= subinput #\Space)) (not (char= subinput #\_)))
        (symbol-DFA-eval 0)
    )

    ; Underscore is an exception for identifiers
    (if (char= #\_ subinput)
        (progn
            (setq id-process 1)
            (push subinput sublist)
        )
    )

)

(defun alphanumeric-state-eval()
    ; Go to the alpha/numeric state for identifier/integer/fraction DFA
    (if (alpha-char-p subinput)
        (alpha-ch-state)
        (numeric-ch-state)
    )
)

(defun identifier-DFA-eval()
    ; Start the keyword DFA for the subinput
    ; If it is not a keyword then return "identifier" keyword
    (if (= id-process 1)
        (progn
            (keyword-DFA-eval 0)
            (if (= id-process 1)
            (progn 
                (setq a (list (list (format nil "~{~A~}" (reverse sublist)) "ID")))
                (setq ls (append ls a))
                (reset-var)
            ))
        )
    )
)

(defun integer-DFA-eval()
    ; If it is an integer DFA, then return "valuei"
    (if (and (= int-process 1) (= fraction-process 0))
        (progn
            (terpri)
            (format T "LEXICAL ERROR!: ~A cannot be tokenized!" (format nil "~{~A~}" (reverse sublist)))
            (terpri)
            (exit-prog)
        )
    )
)

(defun fraction-DFA-eval()
    (setq flag 0)
    (if (and (= fraction-process 1))
        (progn
            ; {integer}f is not valid for fractions
            (if (char= (nth 0 sublist) #\f)
                (progn 
                    (print "hi")
                    (terpri)
                    (format T "LEXICAL ERROR!: ~A cannot be tokenized!" (format nil "~{~A~}" (reverse sublist)))
                    (terpri)
                    (exit-prog)
                )
            )
            ; If it is valid, then return "valuef"
            (if (= flag 0)
                (progn
                    (setq a (list (list (format nil "~{~A~}" (reverse sublist)) "VALUEF")))
                    (setq ls (append ls a))
                )
            )
            (reset-var)
        )
    )
)

(defun symbol-DFA-eval(i)
    ; Check if the given symbol is in the token list
    ; If so, then return its token
    (setq symbol (nth i symbols))
    (setq flag 0)
   
    (if (char= (char symbol 0) subinput)
        (progn
            (setq a (list (list (nth i symbols) (nth i tokens))))
            (setq ls (append ls a))
            (setq flag 1)
        )
    )   

    ; Set "exceptions" 2 to determine if it is ";;"
    ; Check if the previous character is ";"
    ; If so, then return "COMMENT"
    (if (char= #\; subinput)
        (progn
            (if (= exceptions 2)
                (progn
                    (setq exceptions 0)
                    (setq checkskip 2)
                    (setq a (list (list ";;" "COMMENT")))
                    (setq ls (append ls a))
                    (setq flag 1)
                )
                (progn
                    (setq exceptions 2)
                    (setq flag 1)
                )
            )  
        )
    )  

    ; Recursively calls the function to determine if the given symbol is valid
    (if (< i (1- (length symbols)))
        (progn
            (if (= flag 0)
                (symbol-DFA-eval (1+ i))
            )
        )
        (progn    
            (if (= flag 0)
                (progn
                    (terpri)
                    (format T "LEXICAL ERROR!: ~A cannot be tokenized!" subinput)
                    (terpri)
                    (exit-prog)
                )
            )
        )
    )
         
)

(defun keyword-DFA-eval(i)
    ;Check if the sublist is a keyword
    ;If so, then return its token
    (if (string= (nth i symbols) (format nil "~{~A~}" (reverse sublist)))
        (progn
            (setq a (list (list (nth i symbols) (nth i tokens))))
            (setq ls (append ls a))
            (reset-var)
        )
        (progn 
            (if (< i (1- (length symbols)))
                (keyword-DFA-eval (1+ i))
            )
        )
    )  
)      

(defun exception-check()
    ; Exception handler for the operator ";;"
    (if (and (= exceptions 2) (not (char= subinput #\;)))
        (progn
            (terpri)
            (format T "LEXICAL ERROR!: ; cannot be tokenized!")
            (terpri)
            (exit-prog)
        )
    )
    ; Exception handler for the leading zero
    (if (and (= exceptions 3) (numberp (digit-char-p subinput)))
        (progn 
            (push subinput sublist)
            (setq checkskip 1)
        )
    )
)

(defun reset-var()
    ; Reset all global variables
    (setq checkskip 0)
    (setq fraction-process 0)
    (setq int-process 0)
    (setq id-process 0)
    (setq sublist nil)
    (setq exceptions 0)
)

(defun evaluate-char()
    ; Directs to the right evaluation function
    (if (not (alphanumericp subinput))
        (progn
            ; Before evaluating the symbol, checks the DFA's
            (integer-DFA-eval)
            (fraction-DFA-eval)
            (identifier-DFA-eval)
                
            (not-alphanumeric-state-eval)
        )
    ) 
    (if (alphanumericp subinput)
        (alphanumeric-state-eval)
    )   
)

(defun skip-invalid()
    ; If the current DFA is not valid, then skip the rest until a non alphanumeric char except "_"
    (if (and (not (alphanumericp subinput)) (not (char= subinput #\_)))
        (progn
            (terpri)
            (format T "LEXICAL ERROR!: ~A cannot be tokenized!" (format nil "~{~A~}" (reverse sublist)))
            (terpri)
            (exit-prog)
        )
    )
    (if (or (alphanumericp subinput) (char= subinput #\_))
        (push subinput sublist)
    )
)

(defun tokenize() 
    ; If it is not newline, then evaluate the char when checkskip is 0
    (if (not (char= subinput #\Newline))
        (progn 
            (if (= checkskip 1)
                (skip-invalid)
            )
            (if (= checkskip 0)
                (progn
                    (exception-check)
                    (if (= checkskip 0)
                        (evaluate-char)
                    )
                )
            )
        )
    )
    ; If it is newline, then check all DFA's when checkskip is 0
    ; If checkskip is 1, then print error message for invalid DFA and terminate the program
    ; If checkskip is 2, then reset variables since it is the end of the comment
    (if (char= subinput #\Newline)
        (progn
            (if (= checkskip 0)
                (progn
                    
                    (exception-check)
                    (integer-DFA-eval)
                    (fraction-DFA-eval)
                    (identifier-DFA-eval)
                    (reset-var)
                )
            )
            (if (= checkskip 1)
                (progn
                    (terpri)
                    (format T "LEXICAL ERROR!: ~A cannot be tokenized!" (format nil "~{~A~}" (reverse sublist)))
                    (terpri)
                    (exit-prog)
                )
            )
            (if (= checkskip 2)
                (reset-var)
            )
        )
    )
)

(defun get-char-rec(input i)
    ; Get char from the input recursively
    (setq subinput (char input i))
    (if (string= (string subinput) "(")
        (incf op)
    )
    (if (string= (string subinput) ")")
        (incf cp)
    )
    (tokenize)
    (if (< (1+ i) (length input))
        (get-char-rec input (1+ i))
        (progn 
            (setq subinput #\Newline)
            (tokenize)
        )
    )
)


(defun get-input()
    ; Get input from the user
    (setq op 0)
    (setq cp 0)
    (princ "Type anything or type (exit) to terminate:")
    (loop
        (setq input (read-line))
        (setq input (string input))
        (if (string= input "(exit)")
            (exit)
            (if (not (= 0 (length input)))
                (get-char-rec input 0)
            )
        )
        (when (eq op cp) (return 1)) 
    )

    (read-tokens 0)

    (terpri)
    (setq ls (list))
    (reset-interpreter-var)

    (get-input)
)


