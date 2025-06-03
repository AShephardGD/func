(defun l () (write "here"))

(defun set1        ; Name
 (l arg)           ; Args
 (cons arg (cdr l)); Set the first element of the list
)

(defun set2                        ; Name
 (l arg)                           ; Args
 (cons (car l) (cons arg (cddr l))); Set the second element of the list
)

(defun set3                                         ; Name
 (l arg)                                            ; Args
 (cons (car l) (cons (cadr l) (cons arg (cdddr l)))); Set the third element of the list
)

(defun set4                                         ; Name
 (l arg)                                            ; Args
 (cons (car l) (cons (cadr l) (cons (caddr l) arg))); Set the forth element of the list
)



(defun errorIndex; Name
 (index)         ; Args
 (cond           ; Correct the index for correct error output
  (
   (eq index 0)
   index
  )
  (
   T
   (1- index)
  )
 )
)



(defun factorial           ; Name
 (n)                       ; Args
 (cond                     ; Counts factorial
  (
   (< n 2)
   1
  )
  (
   T
   (* n (factorial (1- n)))
  )
 )
)



(defun digitChar      ; Name
 (char)               ; Args
 (cond                ; digit-char-p with char error handling
  (
   char
   (digit-char-p char)
  )
  (
   T
   nil
  )
 )
)

(defun charWOlen                ; Name
 (input index)                  ; Args
 (cond                          ; char with index error handling
  (
   (< (length input) (1+ index))
   nil
  )
  (
   T
   (char input index)
  )
 )
)

(defun subseqWOlen            ; Name
 (input start end)            ; Args
 (cond                        ; subseq with index error handling
  (
   (< (length input) (1+ end))
   nil
  )
  (
   T
   (subseq input start end)
  )
 )
)



(defun derivative
 (inputList functions)
 nil
)

(defun checkFunction
 (inputList functions)
 nil
)

(defun executeFunction
 (inputList functions)
 nil
)



(defun NNegDegreeSF1      ; Name
 (negDegree)              ; Args
 (cond                    ; Is number an Integer or double
  ((minusp negDegree) 0)
  (T negDegree)
 )
)

(defun NNegDegreeSF2            ; Name
 (negDegree)                    ; Args
 (cond                          ; Increment degree if double
  ((minusp negDegree) negDegree)
  (T (1+ negDegree))
 )
)

(defun NChange; Name
 (lf)         ; Args
 (cond        ; Change arg if there is a number
  (lf lf)
  (T 0d0)
 )
)

(defun N                                                                 ; Name, returns (input, index, error, res)
 (inputList negDegree)                                                   ; Args
 (let
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
   (lf (cdddr inputList))
  )                                                                      ; Number grammar rule
  (cond
   (
    error                                                                ; If error
    inputList                                                            ; Return error, nothing done
   )
   (
    (digitChar (charWOlen input index))                                  ; If digit
    (N                                                                   ; Read next char 
     (set4 (set2 inputList (1+ index))
     (+ (* (NChange lf) 10) (digitChar (char input index))))
     (NNegDegreeSF2 negDegree)
    )
   )
   (
    (eq #\. (charWOlen input index))                                     ; If dot
    (cond
     (
      (minusp negDegree)                                                 ; If there is no degree
      (N (set2 inputList (1+ index)) 0)                                  ; Make degree
     )
     (
      T                                                                  ; If there is a degree
      (set3 inputList "Too many dots...")                                ; Return error
     )
    )
   )
   (
    (eq #\e (charWOlen input index))                                     ; If e
    (set4 (set2 inputList (1+ index)) (exp 1))                           ; Return Euler constant
   )
   (
    (string= "pi" (subseqWOlen input index (+ 2 index)))                 ; If pi
    (set4 (set2 inputList (+ index 2)) pi)                               ; Return pi
   )
   (
    lf                                                                   ; Other char and there is a number
    (set4 inputList (/ (NChange lf) (expt 10 (NNegDegreeSF1 negDegree)))); Return number
   )
   (
    T                                                                    ; Other char and there is no number
    (set3 inputList "There is no number.")                               ; Return error
   )
  )
 )
)



(defun T3                                                                      ; Name
 (inputList functions)                                                         ; Args
 (let
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
  )
  (cond
   (
    error                                                                      ; If error
    inputList                                                                  ; Return error
   )
   (
    (eq #\( (charWOlen input index))                                           ; If openning bracket
    (let*
     (
      (ERes (E (set2 inputList (1+ index)) functions))
      (EResIndex (cadr ERes))
     )
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (string= ")log(" (subseqWOlen input EResIndex (+ 5 EResIndex)))         ; Logarithm
       (let ((E2Res (E (set2 ERes (+ 5 EResIndex)) functions)))
        (cond
         (
          (caddr ERes)                                                         ; If error
          E2Res                                                                ; Return error
         )
         (
          (= 1 (cdddr E2Res))                                                  ; If base equals one
          (set3 E2Res "Base can not be 1.")                                    ; Return error
         )
         (
          (>= 0 (cdddr ERes))                                                  ; If number below or equals zero
          (set3 ERes "Number must be positive.")                               ; Return error
         )
         (
          (>= 0 (cdddr E2Res))                                                 ; If base below or equals zero
          (set3 E2Res "Base must be positive.")                                ; Return error
         )
         (
          (eq #\) (charWOlen input (cadr E2Res)))                              ; If closing bracket
          (set2 (set4 ERes (log (cdddr ERes) (cdddr E2Res))) (1+ (cadr E2Res))); Return res
         )
         (
          T                                                                    ; Other char
          (set3 E2Res "There is no closing brackets.")                         ; Return error
         )
        )
       )
      )
      (
       (eq #\) (charWOlen input EResIndex))                                    ; If closing bracket
       (set2 ERes (1+ EResIndex))                                              ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "lg(" (subseqWOlen input index (+ 3 index)))                      ; Logarithm with base 10
    (let ((ERes (E (set2 inputList (+ 3 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (>= 0 (cdddr ERes))                                                     ; If number below or equals zero
       (set3 ERes "Number must be positive.")                                  ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing brackets
       (set2 (set4 ERes (log (cdddr ERes) 10)) (1+ (cadr ERes)))               ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
   (string= "ln(" (subseqWOlen input index (+ 3 index)))                       ; Logarithm with base e
    (let ((ERes (E (set2 inputList (+ 3 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (>= 0 (cdddr ERes))                                                     ; If number below or equals zero
       (set3 ERes "Number must be positive.")                                  ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (log (cdddr ERes))) (1+ (cadr ERes)))                  ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "sin(" (subseqWOlen input index (+ 4 index)))                     ; Sine
    (let ((ERes (E (set2 inputList (+ 4 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (sin (cdddr ERes))) (1+ (cadr ERes)))                  ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return res
      )
     )
    )
   )
   (
    (string= "cos(" (subseqWOlen input index (+ 4 index)))                     ; Cosine
    (let ((ERes (E (set2 inputList (+ 4 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (cos (cdddr ERes))) (1+ (cadr ERes)))                  ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "tg(" (subseqWOlen input index (+ 3 index)))                      ; Tangent
    (let ((ERes (E (set2 inputList (+ 3 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (= (/ pi 2) (mod (cdddr ERes) pi))                                      ; If arg equals pi/2 + pik
       (set3 ERes "tan(+-pi/2) is undefined.")                                 ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (tan (cdddr ERes))) (1+ (cadr ERes)))                  ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "arcsin(" (subseqWOlen input index (+ 7 index)))                  ; Arcsine
    (let ((ERes (E (set2 inputList (+ 7 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (< 1 (cdddr ERes))                                                      ; If arg over one
       (set3 ERes "The number should be in [-1, 1].")                          ; Return error
      )
      (
       (> -1 (cdddr ERes))                                                     ; If arg below minus one
       (set3 ERes "The number should be in [-1, 1].")                          ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (asin (cdddr ERes))) (1+ (cadr ERes)))                 ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "arccos(" (subseqWOlen input index (+ 7 index)))                  ; Arccosine
    (let ((ERes (E (set2 inputList (+ 7 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (< 1 (cdddr ERes))                                                      ; If arg over one
       (set3 ERes "The number should be in [-1, 1].")                          ; Return error
      )
      (
       (> -1 (cdddr ERes))                                                     ; If arg below minus one
       (set3 ERes "The number should be in [-1, 1].")                          ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (acos (cdddr ERes))) (1+ (cadr ERes)))                 ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "arctg(" (subseqWOlen input index (+ 6 index)))                   ; Arctangent
    (let ((ERes (E (set2 inputList (+ 6 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (atan (cdddr ERes))) (1+ (cadr ERes)))                 ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (string= "d/dx(" (subseqWOlen input index (+ 5 index)))                    ; Derivative
    (let ((ERes (E (set2 inputList (+ 5 index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (eq #\) (charWOlen input (cadr ERes)))                                  ; If closing bracket
       (set2 (set4 ERes (derivative (cdddr ERes))) (1+ (cadr ERes)))           ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (eq #\| (charWOlen input index))                                           ; Absolute value
    (let ((ERes (E (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr ERes)                                                            ; If error
       ERes                                                                    ; Return error
      )
      (
       (eq #\| (charWolen input (cadr ERes)))                                  ; If closing line
       (set2 (set4 ERes (abs (cdddr ERes))) (1+ (cadr ERes)))                  ; Return res
      )
      (
       T                                                                       ; Other char
       (set3 ERes "There is no closing brackets.")                             ; Return error
      )
     )
    )
   )
   (
    (checkFunction inputList functions)                                        ; If custom function
    (executeFunction inputList functions)                                      ; Execute custom function
   )
   (
    T                                                                          ; Number?
    (N (set4 inputList nil) -1)
   )
  )
 )
)



(defun T22                                                   ; Name
 (inputList functions)                                       ; Args
 (let                                                        ; Grammar rule T22 body
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
   (lf (cdddr inputList))
  )
  (cond
   (
    error                                                    ; If error
    inputList                                                ; Return error
   )
   (
    (eq #\! (charWOlen input index))
    (cond
     (
      (minusp lf)                                            ; If number is negative
      (set3 inputList "Factor number can not be negative.")  ; Return error
     )
     (
      (= 0 (mod lf 1))                                       ; Is number an integer
      (set2 (set4 inputList (factorial lf)) (1+ index))      ; Return factorial of number
     )
     (
      T                                                      ; Else number is non integer
      (set3 inputList "Factor number can not be non integer"); Return error
     )
    )
   )
   (
    T                                                        ; Other char
    inputList                                                ; Epsilon, nothing done
   )
  )
 )
)



(defun T21                                                   ; Name
 (inputList functions)                                       ; Args
 (let                                                        ; Grammar rule T21 body
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
   (lf (cdddr inputList))
  )
  (cond
   (
    error                                                    ; If error
    inputList                                                ; Return error
   )
   (
    (eq #\- (charWOlen input index))                         ; If there is a minus
    (T21 (set2 (set4 inputList (- lf)) (1+ index)) functions); Change sign
   )
   (
    T                                                        ; Other char
    inputList                                                ; Epsilon, nothing done
   )
  )
 )
)



(defun T2                                                          ; Name
 (inputList functions)                                             ; Args
 (let                                                              ; Grammar rule T2 body
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
  )
  (cond
   (
    error                                                          ; If error
    inputList                                                      ; Return error
   )
   (
    T
    (let* 
     (
      (T21Res (T21 (set4 inputList 1) functions))                  ; Sign check
      (T3Res (T3 T21Res functions))                                ; T3 check
     )
     (cond
      (
       (caddr T3Res)
       T3Res
      )
      (
       T
       (T22 (set4 T3Res (* (cdddr T21Res) (cdddr T3Res))) functions); Factorial check
      )
     )
    )
   )
  )
 )
)



(defun T1Apos                                                 ; Name, returns (input, index, error, res)
 (inputList functions)                                        ; Args
 (let
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
   (lf (cdddr inputList))
  )                                                           ; Grammar rule T1' body
  (cond
   (
    error                                                     ; If error
    inputList                                                 ; Return error, nothing done
   )
   (
    (eq #\* (charWOlen input index))                          ; If multiply
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
      )
      (
       T                                                      ; Else
       (T1Apos (set4 T2Res (* lf (cdddr T2Res))) functions)   ; Input left * T2 and push to T1'
      )
     )
    )
   )
   (
    (eq #\/ (charWOlen input index))                          ; If divide
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
      )
      (
       (= 0 (cdddr T2Res))                                   ; If denumerator equals zero
       (set3 T2Res "Division by zero.")                       ; Return error
      )
      (
       T                                                      ; Else
       (T1Apos (set4 T2Res (/ lf (cdddr T2Res))) functions)   ; Input left / T2 and push to T1'
      )
     )
    )
   )
   (
    (eq #\^ (charWOlen input index))                          ; If power
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
      )
      (
       (and (= lf 0) (= (cdddr T2Res) 0))                   ; If there is 0^0
       (set3 T2Res "The result of 0^0 is undefined.")         ; Return error
      )
      (
       T                                                      ; Else
       (T1Apos (set4 T2Res (expt lf (cdddr T2Res))) functions); Input left ^ T2 and push to T1'
      )
     )
    )
   )
   (
    (eq #\% (charWOlen input index))                          ; If modulo
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
      )
      (
       (= 0 (cdddr T2Res))
       (set3 T2Res "Division by zero.")
      )
      (
       T                                                      ; Else
       (T1Apos (set4 T2Res (mod lf (cdddr T2Res))) functions) ; Input left % T2 and push to T1'
      )
     )
    )
   )
   (
    T                                                         ; Other char
    inputList                                                 ; Epsilon, nothing done
   )
  )
 )
)



(defun T1                                    ; Name, returns (input, index, error, res)
 (inputList functions)                       ; Args
  (T1Apos (T2 inputList functions) functions); Grammar rule T1 body
)



(defun EApos                                                  ; Name, returns (input, index, error, res)
 (inputList functions)                                        ; Args
 (let
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
   (lf (cdddr inputList))
  )                                                           ; Grammar rule E' body
  (cond
   (
    error                                                     ; If error
    inputList                                                 ; Return error, nothing done
   )
   (
    (eq #\+ (charWOlen input index))                          ; If plus
    (let* ((T1Res (T1 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T1Res)                                          ; If error
       T1Res                                                  ; Return error, nothing done
      )
      (
       T                                                      ; Else
       (EApos (set4 T1Res (+ lf (cdddr T1Res))) functions)    ; Input left + T1 and push to E'
      )
     )
    )
   )
   (
    (eq #\- (charWOlen input index))                          ; If minus
    (let* ((T1Res (T1 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T1Res)                                          ; If error
       T1Res                                                  ; Return error, nothing done
      )
      (
       T                                                      ; Else
       (EApos (set4 T1Res (- lf (cdddr T1Res))) functions)    ; input left - T1 and push to E'
      )
     )
    )
   )
   (
    T                                                         ; Other char
    inputList                                                 ; Epsilon, nothing done
   )
  )
 )
)



(defun E                                   ; Name, returns (input, index, error, res)
 (inputList functions)                     ; Args
 (EApos (T1 inputList functions) functions); Grammar rule E body
)



(defun functionalCalculate                                                    ; Name
 (input functions)                                                            ; Args
 (cond 
  (
   (find #\= input)
   (functionalCalculate (delete #\Space (read-line)) (cons functions input))  ; Add custom function
  )
  (
   T
   (let ((ERes (E (cons input (cons 0 (cons nil 0))) functions)))             ; Calculate input
    (cond
     (
      (caddr ERes)                                                            ; If error show error
      (let ((pointer (make-string (1+ (cadr ERes)) :initial-element #\Space)))
       (setf (char pointer (errorIndex(cadr ERes))) #\^)
       (print input)
       (print pointer)
       (print (caddr ERes))
      )
     )
     (
      T                                                                       ; Else show the result
      (print (cdddr ERes))
     )
    )
   )
   (functionalCalculate (delete #\Space (read-line)) functions)               ; Loop
  )
 )
)



(functionalCalculate (delete #\Space (read-line)) nil)