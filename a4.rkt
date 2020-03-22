;Japinder Sandhu
;101021899


"Question 4."

;append 
(define (append L1 L2)
  (cond ((null? L1) L2)
        (else (cons (car L1)
               (append (cdr L1) L2)))))

;exists-node
(define (exists-node x list)
  
    (cond
      
        ((null? list) #f)
        ;check if the first element of the first element = x 
        ( (eq? (caar list) x) #t)
        ;else tail recursion
        (else (exists-node x (cdr list)))

        )

  )

;exists-element
;used for single dimensional lists
(define (exists-element x list)
    (cond
      
        ((null? list) #f)
        ;check if the first element of the first element = x 
        ( (eq? (car list) x) #t)
        ;else tail recursion
        (else (exists-element x (cdr list)))

        )

  )

;exists-edge
(define (exists-edge x y list)


  (newline)
  (display list)
  
  (cond ( (not (and (exists-node x list) (exists-node y list)) ) #f )

        ;base case
        ((null? list) #f)

        ;check if the first element of the first element = x
        ;and if y is in that list
        ( (and (eq? (caar list) x) (exists-element y  (car list) ) ) #t)

        ;else tail recursion
        (else (exists-edge x (cdr list)))

        )
  

  )


(define (make-graph)

  (define globalList '() )

   (define (add-node x)
      
        (if (not (exists-node x globalList))
            
            (begin 
              
                (set! globalList (cons (cons x '()) globalList) )
                
                #t)
            
           #f) )

  (define (add-edge x y)

     
     ;if edge exist, return false
     (cond ( (exists-edge x y globalList) #f)


           ;if nodes do not exist, return false
           ( (not (and (exists-node x globalList) (exists-node y globalList) )) #f)

           
           ;create edge if it doesnt exist
           (else (begin
              
             (set! globalList (cons x globalList) )
                
             #t) ) )
    )

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


(define (l) (cons 'a '( (b c) ) ) )