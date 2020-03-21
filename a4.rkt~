;Japinder Sandhu
;101021899


"Question 4."

;append 
(define (append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1)
               (append (cdr L1) L2)))))

;exists
(define (exists x lst)
       (if (null? lst) #f
           
            (if (equal? x (car lst))  #t
                
                                     (exists x (cdr lst)) 
            )
       )
) 



(define (make-graph)

  (define globalList '())

  
   (define (add-node x)
      
        (if (not (exists x globalList))
            
            (begin
              
                (set! globalList (append globalList x) )
                
                #t)
            
           #f) )

   (define (add-edge x y)
      
        (set! balance (+ balance amount))
      
        balance)

   (define (remove-node x y)
      
        (set! balance (+ balance amount))
      
        balance)

   (define (remove-edge x y)
      
        (set! balance (+ balance amount))
      
        balance)
  

    (define (dispatch method)
      
        (cond ((eq? method 'add-node) add-node)
              
              ((eq? method 'add-edge) add-edge)

              ((eq? method 'remove-node) add-edge)

              ((eq? method 'remove-edge) add-edge)
              
			  (else (lambda() (display "Unknown Request: ")
                                  
			                  (display method)(newline)))))

    dispatch)