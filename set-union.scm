; is-list? 

; This implementation of is-list?  returns #t iff obj is an acyclic list. 
; This function is guarenteed to terminate when used with cyclic data 
; structures.
; This procedure does not, however, return #t when passed a cyclic list.
(define (is-list? obj)
  ((letrec ((loop (lambda (elementA elementB)
    (if (pair? elementA)
        ((lambda (elementA)
           (and (not (eq? elementA elementB))
           (if (pair? elementA)(loop (cdr elementA) (cdr elementB))
           (null? elementA))))(cdr elementA))
                (null? elementA)))))loop)obj obj))

; When compared to UMB Scheme's built-in function list?
; the above function clocks in an average of 500ms
; faster on this same code:
; (let loop((i 100000))
;      (cond ((>= i 0) (is-list? (cyclic-datalist 1 2 3 4 5)) (loop(- i 1)))))
; The efficiency difference only increases more when you use larger
; values for the loop's starting index. I'd like to submit a request
; for this to take the place of the built-in list? function, 
; however, it looks like there is no active maintainer for UMB Scheme

; This second procedure returns #t for both cyclic and acyclic lists.
; No such function exists in the current implementation of UMB Scheme
; This might be a good addition to prelude.scheme
(define (is-list?2 obj)
  ((letrec ((loop (lambda (elementA elementB)
    (if (pair? elementA)
        ((lambda (elementA)
           (if (eq? elementA elementB) #t
           (if (pair? elementA)(loop (cdr elementA) (cdr elementB))
           (null? elementA))))(cdr elementA))
                (null? elementA)))))loop)obj obj))

; In the pdf for class lecture four, we are told that
; eq? is "pretty much of historical interest only"
; however, I believe that these functions clearly 
; demonstrate the relevance of the ability
; to check for pointer equality.


; Here is an example of a cyclic data structure
; which the above functions handle properly.
(define (cyclic-datalist . objs)
  (if (pair? objs)
      ((letrec ((loop (lambda (element)
                      (if (pair? (cdr element))
                          (loop (cdr element))
                          (set-cdr! element objs)))))loop)objs))
  objs)

; reverse list procedure
; This is quite simple, if the list is empty, then likewise, the
; reverse of the list is empty. Otherwise, we pre-append
; the result of the recursive call the return value. 
(define (my-reverse objlist)
  (if (equal?  '() objlist)
      '()
      (append (reverse-list (cdr objlist)) (cons (car objlist) `()))))


;  same-parity
(define (same-parity x . intlist)   
  (if (equal? '() intlist)
      '()
      (if (equal? (modulo x 2) (modulo (car intlist) 2))
          (cons (car intlist) (apply same-parity x (cdr intlist)))
          (apply same-parity x (cdr intlist)))))

; I opted to rest the arguments in recursion via the use of the
; apply procedure in each recursive call, as even though this
; doesn't compare favorably with the more obvious alternative
; (using a helper function) in regards to efficiency, it
; does, by my perspective, result in a much more concise
; and readable function. This decision coincides with the
; general trend to sacrifice efficiency for readability.
; In-fact, the author of our textbook was one of the
; early adopters of this trend, have said:
; "Programs must be written for people to read, and only
; incidentally for machines to execute."
; This is especially relevant in the context of functional
; programming, where the importance of readability over
; computational efficiency is essentially built into
; the language structure. This does not, however,
; contradict my earlier statements regarding
; the value of the computational efficiency of
; my is-list? procedure in the UMB-Scheme STDL
; as when it comes to compiler / interpreter
; development, efficiency is treated in a vastly
; different manner, then it might be in a normal
; commercial application. 


; Problem 4. square-list
(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items))) ))

(define (square-list-map items) (map (lambda (x) (* x x)) items))

; This was a matter of simply filling in the blanks, which
; if one knows Clojure, or any other functional language,
; and thus is familiar with map, is trivial.

;  for-each procedure.
(define (my-for-each function objlist)
  (if (eq? objlist '()) '()
      (begin (function (car objlist)) (my-for-each function (cdr objlist)))))

; Here, begin is used to establish a left to right evaluation order
; for the expression. Typically, in R5RS Scheme, there is no specified
; evaluation order for expressions; the use of begin, forces
; the expressions to be evaluated from left to right. 

; my-equal?
(define (my-equal? objlist1 objlist2)
  ;if the parameters are not lists, then just compare the values normally.
  (if (not (and (list? objlist1) (list? objlist2)))
      (eqv? objlist1 objlist2)
  (if (and (list? objlist1) (list? objlist2))
  (cond ((null? objlist1) (null? objlist2)) 
        ((null? objlist2) #f)
; reached end of second objlist, while objlist1 has elements remaining.
	((eqv? (car objlist1) (car objlist2))
        (my-equal? (cdr objlist1) (cdr objlist2)))
	(else #f))
(#f); if one parameter is a list and the other is not, then return false.
   )))

;  every?
(define (every? predicate objlist)
  (if (null? objlist)
      #t
      (if (predicate (car objlist))
          (every? predicate (cdr objlist))
          #f)))
; Matters of what should happen, are largely up to the developer.
; From my perspective, which coincides with that of set theory,
; and the properties of the empty set, the procedure should
; always return true on the empty set. In set theory,
; statements of the form "For every x in S..." where S is
; some set, are false IFF (if and only if) there exists
; a counter example. Since the empty set has no elements,
; then every element of the empty set satisfies any
; all possible predicate in existence. Since, the absense
; of elements, by definition, results in the absense of
; counterexamples. (ie the statement every element of the
; empty set is even is true, likewise the statement every element
; of the empty set is odd is true.) Typically, in mathematics,
; truth by the abense of a counter-example is referred to as
; vacuous truth. One of the core exhibited by functional
; programming languages is the ability to prove the
; correctness of an algorithm mathematically, via its equivalence
; with lambda calculuous. However, this is only true if
; the functional language adheres to the same axioms that
; our standard mathematics are premised on. If we decide to return
; false for the empty set, then we can no longer directly apply
; the conclusions of set theory to our scheme code, which would
; be quite counter-productive. This code will return the same
; boolean regardless of whether it is applied to two lists,
; independently, or whether they are appended first, and then
; the function is applied.

; element-of-set? function from the book.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; unordered-union-set
(define (unordered-union-set set1 set2)
     (cond
        ((null? set1) set2)
        ((null? set2) set1)
        ; if A is the empty set, then we know from set theory
        ; that A U B is B. 
        ((element-of-set? (car set1) set2)
         (unordered-union-set (cdr set1) set2))
        ; making to not double add elements.
        (else (cons (car set1) (unordered-union-set
                                (cdr set1) set2)))))

; we recursively add elements to the union set,
; while making sure to avoid adding duplicates.


; we will repeat the same process as before, while assuring that
; elements are added in the proper order.
(define (ordered-union-set set1 set2)
  (cond ((equal? set1 '()) set2)
        ((equal? set2 '()) set1)
        (else
         (let ((_element1 (car set1)) (_element2 (car set2)))
           (cond
            ; assure the proper order of elements
           ((equal? _element1 _element2) (cons _element1
                (ordered-union-set (cdr set1) (cdr set2))))
           ((< _element1 _element2) (cons _element1
                (ordered-union-set (cdr set1) set2)))
           ((> _element1 _element2) (cons _element2
                  (ordered-union-set set1 (cdr set2)))))))))


; remove-val
(define (remove-val val objlist)
  (cond ((null? objlist) '())
        ((equal? val (car objlist)) (remove-val val (cdr objlist)))
        (else (cons (car objlist) (remove-val val (cdr objlist))))))
