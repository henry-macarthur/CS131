#lang racket
;TODO
;FOR LAMBDA check if and quotes
;FOR LAMBDA check improper lengths

(define LAMBDA (string->symbol "\u03BB"))
(define ns (make-base-namespace))
;(eval my-program ns)

(define (is-lambda x)
  (or (equal? LAMBDA x) (equal? 'lambda x))
)
(define (create-if-term-different x y)
  (quasiquote (if % (unquote x) (unquote y)))
 )

(define (check-boolean x y)
  (cond
    [(equal? x y) x]
    [(equal? x #t) '%]
    [else (if x '% '(not %))]
  )
)


(define (get-position item list index)
  (cond
    [(equal? list empty) -1]
    [(equal? item (car list)) index]
    [else (get-position item (cdr list) (+ 1 index))]
  )
)
(define (combine-vars x y)
  (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))

;we know that x and y are equal, basically need to check to see if one is a function in parents 


(define (handle-lambda-arguments x y x-par y-par)
  (cond
    [(and (equal? x empty) (equal? y empty))
     '()
    ]
    [(and (list? x) (list? y))
     ;they are both lists
     (if (equal? (length x) (length y))
         (if (equal? (car x) (car y))
             (cons (car x) (handle-lambda-arguments (cdr x) (cdr y) (cons (car x) x-par) (cons (car y) y-par)))
             (cons (combine-vars (car x) (car y)) (handle-lambda-arguments (cdr x) (cdr y) (cons (car x) x-par) (cons (car y) y-par)))
         )
        (create-if-term-different x y) ; different length, return the if statement of the two 
      )
    ]
  )
)

(define (handle-single-lambda-expr x-arg y-arg x-expr y-expr)
  (cond
    ;if the arguments are equal then we either return the same or do an if statement
    [(and (equal? x-expr y-expr) (equal? x-arg y-arg)) x-expr]
    [(and (equal? x-expr y-expr) (not (equal? x-arg y-arg)))
     (create-if-term-different (combine-vars (car x-arg) (car y-arg)) x-expr)
     ]
    [else
     (cond
       [(not (equal? (get-position x-expr x-arg 0) -1)) (create-if-term-different x-expr (combine-vars x-expr y-expr))]
       [(not (equal? (get-position y-expr y-arg 0) -1)) (create-if-term-different (combine-vars x-expr y-expr) y-expr)]
       [else (combine-vars x-expr y-expr)]
     )
    ]
  )
)

(define (handle-lambda-function-arguments x-arg y-arg x-expr y-expr) ; dealt with the function calls, now its argument order
 
  
  (cond
    [(equal? x-expr empty) '()]
    
    
    ;need to check to see if the item in the function is a list/tuple so we need to recurse on it
    ;will later need to check to see if one is a pair and the other isnt
    [(and (and (pair? (car x-expr)) (pair? (car y-expr))) (not (pair? (car (car x-expr)))))
     ;first check to see if we arent dealing with a lambda function
     ;(print x-expr)
     (cond
       [(and (not (is-lambda(car (car x-expr)))) (not (is-lambda(car (car y-expr))))) ;basic case, not a lambda
        ;check to see if the functions are equal
        (cons (handle-lambda-function-call x-arg y-arg (car x-expr) (car y-expr)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
       ]
       ;check to see if they are both lambda
       [(and (is-lambda (car (car x-expr))) (is-lambda (car (car y-expr))))
        ;(cons (cons ('() (car (car x-expr)) (car (car y-expr))) (handle-lambda-outer (car x-expr)  (car y-expr))) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
        (cons (list (handle-lambda-outer (car x-expr)  (car y-expr) x-arg y-arg)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
       ]
     )
    ]
    [(and (pair? (car x-expr)) (equal? '() (cdr x-expr)))
     (handle-lambda-function-arguments x-arg y-arg (car x-expr) (car y-expr))
    ] ; check to see if its a tuple with a tuple head and a empty tail
    [(equal? x-arg y-arg)
     (if (not (equal? (car x-expr) (car y-expr)))
         (cons (create-if-term-different (car x-expr) (car y-expr)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
         (cons (car x-expr) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
     )
    ]
    [(not (equal? x-arg y-arg))
     ;check to see the index that they match to in each array
     ;if it maps to the same index in each array use its argument
     ;if its present in one and not the other x!y
     (let ([x-index-x (get-position (car x-expr) x-arg 0)] [x-index-y (get-position (car x-expr) y-arg 0)] [y-index-y (get-position (car y-expr) y-arg 0)] [y-index-x (get-position (car y-expr) x-arg 0)])
       (cond
         [(and (equal? (car x-expr) (car y-expr)) (equal? (car x-expr) 'if))
              (cond
                ;they both contain if
                [(and (not (equal? x-index-x -1)) (not (equal? y-index-y)))
                 (cons (car x-expr) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
                ]
                [(not (equal? x-index-x -1))
                 ; x is if
                 (cons (create-if-term-different (combine-vars 'if (car y-expr)) 'if) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
                ]
                [(not (equal? y-index-y -1))
                 ; y is if
                 (cons (create-if-term-different 'if (combine-vars (car x-arg) 'if)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
                ]
              )
              ;(cons (car x-expr) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
          
          
         ]
         [(and (equal? (car x-expr) (car y-expr)) (not (equal? x-index-x y-index-y)))
          (if (equal? -1 y-index-y)
              (cons (create-if-term-different  (combine-vars (list-ref x-arg 0) (list-ref y-arg 0)) (car x-expr)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
              ;(cons (create-if-term-different (car x-expr) (combine-vars (list-ref x-arg 0) (list-ref y-arg 0))) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
              (cons (create-if-term-different (combine-vars (list-ref y-arg y-index-y) (list-ref x-arg y-index-y)) (combine-vars (list-ref x-arg y-index-y) (list-ref y-arg y-index-y))) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
          )
          
         ]
         [(and (equal? (car x-expr) (car y-expr))  (equal? x-index-x y-index-y))
          (cons (car x-expr) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
         ]
         [(equal? x-index-x x-index-y)
          ;if expression with x and make-expr y-index-y of x and y
          (cons (create-if-term-different (car x-expr) (combine-vars (list-ref x-arg y-index-y) (list-ref y-arg y-index-y))) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
         ]
        ; [(equal? x-index-x y-index-y)
          ;if expression with x and make-expr y-index-y of x and y
         ; (cons (create-if-term-different (combine-vars (list-ref x-arg y-index-y) (list-ref y-arg y-index-y)) (combine-vars (list-ref x-arg y-index-y) (list-ref y-arg y-index-y))) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
      ;   ]
         [(equal? x-index-x y-index-y)
          ;if expression with x and make-expr y-index-y of x and y
          (cons (combine-vars (list-ref x-arg x-index-x) (list-ref y-arg x-index-x)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
         ]
         [else
          ;(if (equal? x-index-x x-index-x))
          (cons (create-if-term-different (combine-vars (list-ref x-arg x-index-x) (list-ref y-arg x-index-x)) (car y-expr)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
         ]
       )
     )
    ]
  )
)

(define (check-function-call x y x-par y-par)
  (cond
    [(and (not (equal? (get-position x x-par 0) -1)) (not (equal? (get-position y y-par 0) -1))) x]
    [(and (not (equal? (get-position x x-par 0) -1)) (equal? (get-position y y-par 0) -1)) (create-if-term-different (combine-vars x (list-ref y-par (get-position x x-par 0))) x)]
    [(and (equal? (get-position x x-par 0) -1) (not (equal? (get-position y y-par 0) -1))) (create-if-term-different (combine-vars x x (list-ref x-par (get-position y y-par 0))) x)]
    [else (list x)]
  )
)

(define (handle-lambda-function-call x-arg y-arg x-expr y-expr)
  ;assume we have equal lengths
  (cond
    [(and (equal? x-expr y-expr) (pair? x-expr)) (check-function-call (car x-expr) (car y-expr) x-arg y-arg)]
    [(equal? x-expr y-expr) x-expr] ; if the expressions are equal just return one of them
    ;check to see if they are lambdas
    [(and (pair? (car x-expr)) (pair? (car y-expr)))
     (cons (expr-compare-two (car x-expr) (car y-expr) x-arg y-arg) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
    ]
    [(not (equal? (car x-expr) (car y-expr)))
     (if (and (not (equal? (get-position (car x-expr) x-arg 0) -1)) (not (equal? (get-position (car y-expr) y-arg 0) -1))) ; if the function call is one of the arguments
         (append (handle-lambda-function-arguments x-arg y-arg (list (car x-expr)) (list (car y-expr))) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
         (cons (create-if-term-different (car x-expr) (car y-expr)) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
     )]
    [(equal? (car x-expr) (car y-expr))
     (cons (car x-expr) (handle-lambda-function-arguments x-arg y-arg (cdr x-expr) (cdr y-expr)))
    ]
  )
)

(define (handle-lambda-expr x-arg y-arg x-expr y-expr)
  (cond
    [(not (and (pair? x-expr) (pair? y-expr))) ; just a statement
     (handle-single-lambda-expr x-arg y-arg x-expr y-expr)
    ]
    [(and (list? x-expr) (list? y-expr))
     (handle-lambda-function-call x-arg y-arg x-expr y-expr)
    ]
  )
)

(define (determine-lambda-type x y)
  (cond
    [(equal? x 'lambda)
     (if (equal? y 'lambda)
         'lambda
         'λ
     )
    ]
    [else
     'λ
    ]
  )
)

(define (handle-lambda-outer x y x-par y-par)
  (let ([x-type (car x)] [y-type (car y)] [x-arg (car (cdr x))] [y-arg (car (cdr y))] [x-func (car (cdr (cdr x)))] [y-func (car (cdr (cdr y)))]) ; get each of the three arguments
    
    ;(cons 'lambda (append (list (handle-lambda-expr-arg x-arg y-arg 0) (handle-lambda-expr x-func y-func 0))))
    (if(equal? (length x-arg) (length y-arg))
       (cons (determine-lambda-type x-type y-type) (list (handle-lambda-arguments x-arg y-arg x-par y-par) (handle-lambda-expr  x-arg y-arg x-func y-func)))
       ;(cons (determine-lambda-type x-type y-type) (list (handle-lambda-arguments x-arg y-arg x-par y-par) (handle-lambda-expr  (append x-par x-arg) (append y-par y-arg) x-func y-func)))
       (create-if-term-different x y)
    )
  )
)
(define (handle-list-helper x y xp yp)
  ;basic format iis to compare the head value and compare the tail and compare them
  ;head can either be a pair or a single value
  (cond
    [(and (equal? (car x) 'quote) (equal? (car y) 'quote))
     ;(append (check-boolean (car (cdr x)) (car (cdr y))) (check-boolean (cdr (cdr x)) (cdr (cdr y))))
     (create-if-term-different (car (cdr x)) (car (cdr y)))
     
    ]
    ;this is our condition if we have an if statement
    [(and (not (equal? (car x) (car y))) (or (equal? (car x) 'if) (equal? (car y) 'if)))
     (create-if-term-different x y)
    ]
    [(xor (is-lambda (car x) ) (is-lambda (car y)))
     (create-if-term-different x y)
    ]
    [(and (and (is-lambda (car x) ) (is-lambda (car y))) (not (equal? (length x) 1)))
     ;check lengths
     
     (handle-lambda-outer x y xp yp)
    ]
    [(and (pair? x) (pair? y))
     ;head is a pair so we recursively check both
     (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))
    ]
  )
)
(define (handle-list x y xp yp)
  (if (not (equal? (length x) (length y)))
      (create-if-term-different x y)
      (handle-list-helper x y xp yp)
  )
)
(define (compare-to-single x y)
  (cond
    [(equal? x y) x] ; can return either one since they are equal
    [(and (boolean? x) (boolean? y)) (check-boolean x y)] ; check to see if we have boolean terms
    ;[(and (list? x) (list y)) (handle-list x y)] ;they are both lists
    [else (create-if-term-different x y)] ;this is our base case, if they are completeley different
  )
)

(define (expr-compare-two x y xp yp)
  (cond
    ;first check to see if we a single value
    [(or (not (pair? x)) (not (pair? y)))
     (compare-to-single x y)
    ]
    [(and (list? x) (list? y)) (handle-list x y xp yp)]    
  )
)

(define (expr-compare x y)
  (cond
    ;first check to see if we a single value
    [(or (not (pair? x)) (not (pair? y)))
     (compare-to-single x y)
    ]
    [(and (list? x) (list? y)) (handle-list x y '() '())]    
  )
)

(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-x '(+ 3 ((lambda (z) z) ((lambda (if z) (- if z)) 10 3))));'(+ 3 ((lambda (a b) (list a b)) 1 2)))
(define test-expr-y '(- ((λ (lambda) (+ lambda 1)) 5) ((lambda (a if) (+ a if)) -2 -2)))

;((lamda (z) z) ((lambda (if z) (- if z)) 10 3))