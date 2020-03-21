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

  (newline)

  (display list)
  
    (cond
      
        ((null? list) #f)

  
        ( (and (not(list? list)) (eq? list x) ) #t)
        
        ( (eq? (car list) x) #t)

        ( (eq? (car (car list)) x) #t)
        
        (else (exists-node x (cdr list)))

        )

  )


       
;exists-edge
(define (exists-edge x y lst)

      ;find node x
      ;if edges dont exist return false
      (cond ((and (exists-node x lst) (exists-node y lst)) #f)
            
            ;if list is one item and found node, no edge connected 
            ( (equal? x lst )  #f )
           
            ;if list is more than 1 item
            (else (if (equal? x (car lst) )

                      ;if found node x check edge list for node y
                      (exists-node y (cdr lst))

                      (exists-node x (cdr lst)) ) )
            
            )

  )

(define (make-graph)

  (define globalList '() )

   (define (add-node x)
      
        (if (not (exists-node x globalList))
            
            (begin
              
                (set! globalList (cons x globalList) )
                
                #t)
            
           #f) )

   (define (add-edge x y)

     ;if two nodes exist
     ;create edge if it doesnt exist
     (if (exists-edge x y globalList)

         ;if edge already exists
         #f

         ;add edge
         (begin
              
           (set! globalList (cons x globalList) )
                
           #t))
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