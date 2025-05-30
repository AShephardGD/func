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



(defun factorial
 (n)
 (cond
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

(defun N                                                                   ; Name, returns (input, index, error, res)
 (inputList negDegree)                                                     ; Args
 (let
  (
   (input (car inputList))
   (index (cadr inputList))
   (error (caddr inputList))
   (lf (cdddr inputList))
  ); Number grammar rule
  (cond
   (
    error                                                                  ; If error
    inputList                                                              ; Return error, nothing done
   )
   (
    (eq index (length input))                                              ; End of input
    (cond
     (
      lf                                                                   ; There is a number
      (set4 inputList (/ (NChange lf) (expt 10 (NNegDegreeSF1 negDegree)))); Return number
     )
     (
      T                                                                    ; No number at the end of input
      (set3 inputList "There is no number.")                               ; Return error
     )
    )
   )
   (
    (digit-char-p (char input index))                                      ; If digit
    (N                                                                     ; Read next char 
     (set4 (set2 inputList (1+ index))
     (+ (* (NChange lf) 10) (digit-char-p (char input index))))
     (NNegDegreeSF2 negDegree)
    )
   )
   (
    (eq #\. (char input index))                                            ; If dot
    (cond
     (
      (minusp negDegree)                                                   ; If there is no degree
      (N (set2 inputList (1+ index)) 0)                                    ; Make degree
     )
     (
      T                                                                    ; If there is a degree
      (set3 inputList "Too many dots...")                                  ; Return error
     )
    )
   )
   (
    (eq #\e (char input index))                                            ; If e
    (set4 (set2 inputList (1+ index)) (exp 1))                             ; Return Euler constant
   )
   (
    (< (+ index 1) (length input))                                         ; If more than 2 chars left and no digits or e or dot
    (cond
     (
      (string= "pi" (subseq input index (+ 2 index)))                      ; If pi
      (set4 (set2 inputList (+ index 2)) pi)                               ; Return pi
     )
     (
      lf                                                                   ; If there is a number
      (set4 inputList (/ (NChange lf) (expt 10 (NNegDegreeSF1 negDegree)))); Return number
     )
     (
      T                                                                    ; If there is no number
      (set3 inputList "There is no number.")                               ; Return error
     )
    )
   )
   (
    lf                                                                     ; Other char and there is a number
    (set4 inputList (/ (NChange lf) (expt 10 (NNegDegreeSF1 negDegree))))  ; Return number
   )
   (
    T                                                                      ; Other char and there is no number
    (set3 inputList "There is no number.")                                 ; Return error
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
    (eq index (length input))                                ; End of input
    inputList                                                ; Epsilon, nothing done
   )
   (
    (eq #\! (char input index))
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
    (eq #\- (char input index))                              ; If there is a minus
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
;      (T3Res (T3 T21 functions))                                  ; T3 check
      (T3Res (N (set2 (set4 inputList nil) (cadr T21Res)) -1))
     )
      (T22 (set4 T3Res (* (cdddr T21Res) (cdddr T3Res))) functions); Factorial check
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
    (eq index (length input))                                 ; End of input
    inputList                                                 ; Epsilon, nothing done
   )
   (
    (eq #\* (char input index))                               ; If multiply
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
    (eq #\/ (char input index))                               ; If divide
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
      )
      (
       (eq 0 (cdddr T2Res))                                   ; If denumerator equals zero
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
    (eq #\^ (char input index))                               ; If power
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
      )
      (
       (and (eq lf 0) (eq (cdddr T2Res) 0))                   ; If there is 0^0
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
    (eq #\% (char input index))                               ; If modulo
    (let* ((T2Res (T2 (set2 inputList (1+ index)) functions)))
     (cond
      (
       (caddr T2Res)                                          ; If error
       T2Res                                                  ; Return error, nothing done
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
    (eq index (length input))
    inputList                                                 ; End of input
   )
   (
    (eq #\+ (char input index))                               ; If plus
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
    (eq #\- (char input index))                               ; If minus
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
       (setf (char pointer (cadr ERes)) #\^)
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