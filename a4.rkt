;Japinder Sandhu
;101021899


"Question 4."

;exists-node
(define (exists-node x lst)


    (cond
      
        ((null? lst) #f)
        ;check if the first element of the first element = x 
        ( (eq? (caar lst) x) #t)
        ;else tail recursion
        (else (exists-node x (cdr lst)))

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

  
  (cond ( (not (exists-node x list)  ) #f )

        ;base case
        ((null? list) #f)
        
        ;check if the first element of the first element = x
        ;and if y is in that list
        ( (and (eq? (caar list) x) (exists-element y  (cdr(car list)) ) ) #t)

        ;else tail recursion
        (else (exists-edge x y (cdr list)))

        )

  )

;add-edge-recursive


(define (add-edge-recursive x y lst)

 
  (cond
      
    ( (null? lst) #f)
        
        ;check if the first element of the first element = x 
        ( (eq? (caar lst) x) (cons (append (list (caar lst) y) (cdr (car lst) )) (cdr lst)) )
        
        ;else tail recursion
        (else (add-edge-recursive x y (append (cdr lst) (cons (car lst) '() )) ) )

        )

  )
;remove-edge-recursive
(define (remove-edge-recursive x y lst)


  (cond
      
    ( (null? lst) #f)
        
        ;check if the first element of the first element = x
        ;search edge list of node, 
        ( (eq? (caar lst) x) (cons (remove-element y (car lst) ) (cdr lst)  ) )
        
        ;else tail recursion
        (else (remove-edge-recursive x y (append (cdr lst) (cons (car lst) '() )) ) )

        )

  )
;remove-element

(define (remove-element x lst)
 
    (cond
      
        ((null? lst) #f)
        
        ;check if the first element of the first element = x  
        ( (eq? (car lst) x)  (reverse (cdr lst)) )
        
        ;else tail recursion
        (else (remove-element x  (append (cdr lst) (cons (car lst) '() )) ) )

        )

  )



;remove-node-helper

(define (remove-node-helper x lst)

    (cond
      
        ((null? lst) #f)
        
        ;check if the first element of the first element = x  
        ( (eq? (caar lst) x)  (cdr lst) )
        
        ;else tail recursion
        (else (remove-node-helper x  (append (cdr lst) (cons (car lst) '() )) ) )

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

     
    
    (cond

      ;if node does not exist, return false
      ( (not (and (exists-node x globalList) (exists-node y globalList) )) #f)
           
      ;if edge exist, return false
      ( (exists-edge x y globalList)  #f) 

      ;create edge if it doesnt exist
      (else (begin
              
              (set! globalList (add-edge-recursive x y globalList) )
           
              #t)) 
        
      )
    )

    (define (remove-node x)

      ;if node exists, remove
        (if (exists-node x globalList)
            
            (begin 
              ;find node and remove 
                (set! globalList (remove-node-helper x globalList) )
                
                #t)
            
           #f) )

   (define (remove-edge x y)
      
     (cond

       ;if node does not exist, return false
       ( (not (exists-node x globalList)) #f)
           
       ;if edge does not exist, return false
       ( (not (exists-edge x y globalList) )  #f)

       ;create edge if it doesnt exist
       (else (begin
              
               (set! globalList (remove-edge-recursive x y globalList) )
           
               #t)) 
      
        )
     )

  (define (display-list-helper lst)
    
    (newline)
      (display (caar lst))
      (display ": ")
      (display (cdr (car lst) ) )
      (newline)

      (cond ( (null? (cdr lst) ) (display " ") )

           
            (else (display-list-helper (cdr lst) ) )


            )


    )

    (define (display-list)
      
      (newline)
      (display globalList)
      (newline)

      (display-list-helper globalList)
     
      )
    

  

    (define (dispatch method)
      
        (cond ((eq? method 'add-node) add-node)
              
              ((eq? method 'add-edge) add-edge)

              ((eq? method 'remove-node) remove-node)

              ((eq? method 'display) display-list )

              ((eq? method 'remove-edge) remove-edge)
              
			  (else (lambda() (display "Unknown Request: ")
                                  
			                  (display method)(newline)))))

    dispatch)


(define G (make-graph))
((G 'add-node) 'a)       ;=> #t
((G 'add-node) 'b)       ;=> #t
((G 'add-node) 'c)       ;=> #t
((G 'add-node) 'a)       ;=> #f
	
((G 'add-edge) 'a 'b)    ;=> #t
((G 'add-edge) 'a 'c)    ;=> #t
((G 'add-edge) 'b 'b)    ;=> #t
((G 'add-edge) 'b 'c)    ;=> #t
((G 'add-edge) 'c 'd)    ;=> #f
((G 'display))           ;=> a: b c
                          
((G 'remove-edge) 'a 'c) ;=> #t
((G 'remove-node) 'c)    ;=> #t
((G 'display))           ;=> a: b
                            






