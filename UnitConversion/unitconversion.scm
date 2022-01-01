; determine if unit x
; is a base unit or not
(define (is-base? x)
  (if (equal? (assoc x source) #f)
      #t
      #f
  )
)

; check if a unit-list contains specific unit type.
; used as a helper function in determining whether
; or not a given conversion is possible.
(define (contains? unit-list data-unit)
    (cond ((null? unit-list) #f)
          ((equal? (car unit-list) data-unit) #t)
          (else (contains? (cdr unit-list) data-unit))
    )
)

; returns true if when normalized unit list a
; and unit list b consist of the same units.
(define (convertable? ulist-a ulist-b)
  (define (convertable-h? ulist-a ulist-b)
    (let loop ((a (cdar (merge-units(normalize ulist-a))))
               (b (cdar (merge-units (normalize ulist-b)))))
      (cond ((null? b) #t)
            ((contains? a (car b)) (loop a (cdr b)))
            (else #f)))
  )
  (and (convertable-h? ulist-a ulist-b) (convertable-h? ulist-b ulist-a)))
 

 

(define (normalize unitlist)
    (cond ((null? unitlist) unitlist)
        ((is-base? (caar unitlist))
          (cons(list 1 (car unitlist)) (normalize (cdr unitlist))))
        (else
         (cons (expand-derived
                (cadr (assoc (caar unitlist) source))
                     (cadar unitlist)) (normalize (cdr unitlist))))
    )
)


(define (convert data unitlist)
  (if (convertable? (cdr data) unitlist)
      (cons (/ (* (car data)
                 (caar (merge-units (normalize (cdr data)))))
         (caar (merge-units (normalize unitlist))))
          unitlist)
      '()
  )
)

(define (merge-units unitlist)
   (define (sum-exponents unit-a unit-b)
    (if (equal? (car unit-a) (car unit-b))
        (list (car unit-a) (+ (cadr unit-a) (cadr unit-b)))
        (list unit-a unit-b)))
 
     
   (define (merge-units-h ulist-a ulist-b)
       (if (null? ulist-a)
           ulist-b
           (if (null? ulist-b)
               ulist-a
               (if(>= (length ulist-a) (length ulist-b))
                 (if (assoc (caar ulist-a) ulist-b)
                     (cons (sum-exponents (car ulist-a)
                     (assoc (caar ulist-a) ulist-b))
                     (merge-units-h (cdr ulist-a) (cdr ulist-b)))
                     (cons (car ulist-a)
                          (merge-units-h (cdr ulist-a) ulist-b)))
                 (merge-units-h ulist-b ulist-a)
               ))))
  (if  (> (length unitlist) 1)
     (merge-units (append (list (cons
        (* (caar unitlist) (caar (cdr unitlist)))
        (merge-units-h (cdr (car unitlist))
          (cdr (car (cdr unitlist)))))) (cddr unitlist)))
       unitlist
  ) 
)
(define (expand-derived derivable-unit exponent)
  (cond ((null? derivable-unit) derivable-unit)
        ((number? (car derivable-unit))
         (cons (expt (car derivable-unit) exponent)
               (expand-derived (cdr derivable-unit) exponent)))
         ((pair? (car derivable-unit))
          (cons (list (caar derivable-unit)
          (* (cadar derivable-unit)  exponent))
                (expand-derived (cdr derivable-unit) exponent))
          ))
)

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))


(define source (with-input-from-file "units.dat" read-file))