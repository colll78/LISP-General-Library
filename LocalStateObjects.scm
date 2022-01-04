;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Author: Philip DiSarro
;;
;;  Here we introduce the assignment and local state by combining set! with local variables.
;;
;;
;;
;;

; rewrite make-account to use lambda more explicitly
(define make-account-lambda 
  (lambda (balance) 
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
      )
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))balance)
    )
  

    (lambda (message)
      (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            (else (error "Unknown request: Make-Account-Lambda" message))))
   )
)

; inlined version of the previous procedure
(define make-account-inline
  (lambda (balance) 
     (lambda (message)
       (cond ((eq? message 'withdraw) ; eq is guaranteed to work on symbols
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount)) balance) "Insufficient funds")))
      
            ((eq? message 'deposit)
             (lambda (amount)
               (set! balance (+ balance amount))balance))
            (else (error "Unknown request: Make-Account-Inline" message))))  
   )
)

(define (display* first . rest)
    (define port (if (output-port? first) first (current-output-port)))
    (define (display** objs)
        (if (pair? objs) 
	    (begin (display (car objs)) (display** (cdr objs)))))
    (display** (if (output-port? first) rest (cons first rest))))


(define (error . args )
    (newline)
    (display "Error: ")
    (apply display* args)
    (newline)
)

; factor out lambda from make-accout-inline
(define make-account-inline-factored
  (lambda (balance)
    (lambda (message)
      (if (and (not (eq? message 'withdraw)) (not (eq? message 'deposit)))
          (error "Unknown request: Make-Account-Inline-Factored" message)
          (lambda (amount)
            (cond ((eq? message 'withdraw)
                    (if (>= balance amount)
                    (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
                  ((eq? message 'deposit)
                     (set! balance (+ balance amount))balance)
            )
          )
       )
    )
 )
)

; make-monitored
(define (make-monitored f)
  ((lambda (call-count)
    (lambda (message)
      (cond ((eq? message 'how-many-calls?) call-count)
            ((eq? message 'reset-count) (set! call-count 0) call-count)
            (else (set! call-count (+ call-count 1)) (f message)))
    )
  )0)
)

; procedure creating password protected accounts.
(define make-pw-account
  (lambda (balance pw)
    (let ((wrapped-acc (make-account-inline-factored balance)))
      (lambda (user-inp-pw message)
        (if (eq? user-inp-pw pw)
            (wrapped-acc message)
            handle-incp-error
        )
      )
    )
  )
)

; keeps the program from breaking
; if someone enters an incorrect password
(define (handle-incp-error amount)
  (error "Cannot process transaction with amount: " amount)
  (display "Reason: incorrect password"))
