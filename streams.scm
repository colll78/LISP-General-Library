;(define-syntax cons-stream
;  (syntax-rules ()
;    ((cons-stream head tail)
;     (cons head (delay tail)))))

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))


(define (stream-filter pred stream)
  (cond ((null? stream) '())
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;****************************************************************

; procedure that prints the first n elements of
; stream, each on a separate line.
(define (display-n data n)
  (let loop ((index n) (st data))
    (if (null? st)
       (newline)
       (if (> index 0)
           (begin (display (stream-car st))
           (newline)
           (loop (- index 1) (stream-cdr st)))
           (newline)
       )
    )
  )
)


;Generalizes stream-map to allow procedures that take
;multiple arguments.  
(define (stream-map proc . argstreams)
  (cons-stream
   (apply proc (map stream-car argstreams))
   (apply stream-map(cons proc
         (map stream-cdr argstreams))))  
)


(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define notdiv-235
  (stream-filter
   (lambda (x)
     (and (not (equal? 0 (remainder x 2)))
          (not (equal? 0 (remainder x 3)))
          (not (equal? 0 (remainder x 5))))) integers)
)
                                        

; Computing the digits in PI

; convert an integer to a list of its digits
; avoiding all the pitfalls and nastiness
; of convertion to and from strings.
; It uses some neat math. This is the
; same method employed by the C Standard
; library in itoa (integer to array).

(define (digits n . args)
  (let ((b 10 ))
    (let loop ((n n) (d '()))
      (if (zero? n) d
          (loop (quotient n b)
                (cons (modulo n b) d))))))


; mult-stream procedure.
; simple test -
;       (mult-stream 87 (list->stream '(9 8 7 4 3 6 9 1 7)))
(define (mult-stream m stream)
  ;;f(n) -> y normal
  ; power function, except when y <= 0,
  ; in which case the function returns 1
  (define (fix-power lst)
    (if(> (expt 10 (- (length lst) 1)) 0)
      (expt 10 (- (length lst) 1))
      1
    )
  )

  (let loop ((a 0) (a-list '()) (st stream))  
    (if (stream-null? st)
        (if (null? a-list)
            '()
            (cons-stream (car a-list)
            (loop a (cdr a-list) st))
             ; here we are done once we append the rest
             ; of a-list to our output.
        )
        (if (not (equal? a-list '()))             
            (if (< (+ m (modulo a  (fix-power a-list)))
                    (fix-power a-list))
                
                   (cons-stream (car a-list)
                   (loop (modulo a (fix-power a-list))
                   (cdr a-list) st)) ;;produce
                          
                   (loop (+ (* a 10) (* m (stream-car st)))
                   (pad-list (digits (+ (* a 10) (* m (stream-car st))))
                             a-list
                             0
                   )
                   (stream-cdr st)) ;consume
                   
             )   
       
             (loop (+ (* a 10) (* m (stream-car st)))
             (pad-list (digits (+ (* a 10) (* m (stream-car st))))
                             a-list
                             0
             )
             (stream-cdr st));consume
        )
    )
  )
)                                 

; Inputs:
;    to-pad: the list to pad
;    to-comp: the list we want to exceed in length (by 1).
;    pad-data: the data we would like to use as padding,
;    (in mult-stream this is 0)
; Outputs:
;    pad-list with pad-data prepended until it exceeds the length
;    of to-comp by 1.
(define (pad-list to-pad to-comp pad-data)
 (let loop ((tp to-pad))
  (if (<= (length tp) (length to-comp))
      (loop (cons pad-data tp))
      tp
  )
 )
)

; Inputs:
;   M: matrix given as list of rows
;   n: the index of the desired column
; Ouputs:
;   A list containing the elements of column n in matrix M.
(define (matrix-col M n)
  (let loop ((index (length M)) (column-data '()))
    (if (equal? index 0)
        column-data
        (loop (- index 1)
              (cons (matrix-row (matrix-row M (- index 1)) n) column-data)))))

; Inputs:
;   M: matrix given as list of rows
;   n: the index of the desired row
; Ouputs:
;   A list containing the elements of row n in matrix M.
(define (matrix-row list n)
  (let loop ((row-index n) (row list))
    (if (equal? row-index 0)
        (car row)
        (loop (- row-index 1) (cdr row))
    )
  )
)

(define (reduce op lst)
  (let loop ((res (car lst)) (lst (cdr lst)))
    (if (null? lst)
        res
        (loop (op res (car lst)) (cdr lst)))))

; Function to multiply two matricies.
; Inputs:
;   M: matrix given as list of rows.
;   N: matrix given as list of rows.
; Outputs:
;   The dot product - N.M
(define (matrix-composition N M)
  (let row-loop ((row-index (length N)) (result '()))
    (if (equal? row-index 0)
        result
        (row-loop (- row-index 1)
           (cons
             (let col-loop ((col-index (length (car M))) (row '()))
               (if (equal? col-index 0)
                   row
                   (col-loop
                    (- col-index 1)
                    (cons (reduce + (map *
                     (matrix-row N (- row-index 1))
                     (matrix-col M (- col-index 1))))
                            row)
                    )
                   )
               )
               result)
        )
    )
  )
)

; Function to add two matricies.
; Inputs:
;   M: matrix given as list of rows.
;   N: matrix given as list of rows.
; Outputs:
;   The sum of matrix N and matrix M.
(define (matrix-sum N M)
  (let loop ((N N) (M M) (mat-sum '()))
    (if (or (null? N) (null? M))
        (reverse mat-sum)
        (loop (cdr N) 
              (cdr M)
              (cons (map + (car N) (car M)) mat-sum)
        )
    )
  )
)

; Returns element at row i, column j in matrix M
(define (get-ele i j M)
  (matrix-row (matrix-row M i) j)
)

; self explanatory.
(define m1 '((1 6) (0 3)))
(define to-add '((1 4) (0 2)))
(define increase-stream (cons-stream to-add increase-stream))
(define fractional-transformation-stream
  (cons-stream
     m1
    (stream-map matrix-sum increase-stream fractional-transformation-stream)
  )
)

; Uses integer division.
; Returns the floor of:
;    m[0,0]*x + m[0,1]
;       divided by
;    m[1,0]*x + m[1,1]
(define (linear-transform x M)
  (quotient (+ (* (get-ele 0 0 M) x) (get-ele 0 1 M))
            (+ (* (get-ele 1 0 M) x) (get-ele 1 1 M))
  )
)

;Nice and concise generation of the pi stream.
(define pi
  (let loop ((mat m1) (st (stream-cdr fractional-transformation-stream)))
    (let ((lt1 (linear-transform 3 mat)) (lt2 (linear-transform 4 mat)))
      (if (equal? lt1 lt2)
          (cons-stream lt2
           (loop (matrix-composition (list (list 10 (* -10 lt2)) (list 0 1)) mat) st))
          (loop (matrix-composition mat (stream-car st)) (stream-cdr st)) 
      )
    )
  )
)
  
  
  
