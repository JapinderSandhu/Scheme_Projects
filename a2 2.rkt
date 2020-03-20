
"Question 1"
"a)"

;--------------------parta-------------------

(define (cbrt x)
  (define (cube x) (* x x x))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x )) 0.00001))
  (define (improve guess)
    (/ (+ (/ x
             (* guess guess))
          (* 2 guess))
       3))
  (define (cbrt-iter guess)
    (if(good-enough? guess)
       guess
       (cbrt-iter (improve guess))))
  (cbrt-iter 1.0))

;---------------partb----------------


;It does not work because applicative order means both branches of new-if will always be evaluated.
;This causes the improve step to be evaluated ad infinitum (forever). 
;The standard if is a special form that uses lazy evaluation allowing only the consequent (then-part) to be evaluated
;once good enough is true.



;Question 2
"Question 2"
"a)"



(define (product a b term next)
    (if (> a b)
        1
        (* (term a)
           (product (next a) b term next) )))
"b)"

(define (product-it a b term next)

  (define (helper-it a b term next count)
   (newline)
    (display count) 
    (cond ( (> count b) (term a) )
        
        (else (helper-it (* (next a) count ) b term next (+ count 1))))
    )

  (if (<= a 0) 0
      (helper-it a b term next 1) )


  )



;Question 3
;a)

;--------------------parta-------------------
"Question 3"
"a)"

(define (palindrome s)

      (define len (string-length s))

      (define (counter n) (+ n 1) )
      
    

      (if (< (- len 1) 1) #t

     
          (if( equal? ( string-ref s 0 ) ( string-ref s (- len 1) ) )
      
             (palindrome ( substring s (counter 1) (- len 1) )  )
          
             #f

             )

       )

 
  )

;Question 3
;b)

;---------------partb----------------

(define (kPalhelper s counter k len)


  (display (string-append ( substring s 0 ( - (- len 1) counter) ) ) )

    (newline)
  

  ;(string-append ( substring s 0 (- len (+ counter 2)) ) ( substring s  (- len (+ counter 3) ) ))
  )

(define (isKPal s counter k len)

  ;remove last string and check palindrome
  (if (palindrome ( substring s 0 (- len 1) ) )
      #t
      (palindrome s )
    
   )
)


(define (k-palindrome s k )

  (define len ( string-length s ))


  (if (palindrome s)
      #t
      (cond ((< k 1) #f)
            ((isKPal s 2 k len) #t )
            (else #f) ) ) 
)


#|


(define (reverseString s)

  (define length ( string-length s ) )

  (display ( string-ref s (- length 1) ) )

  (newline)

  (display ( string-ref s 0 ) )
  
  

  (if ( = length 1)
      
      ( s )
      
      ( (reverseString (string-append ( string-ref s (- length 1) ) ( string-ref s 0 ) ) ) )
      ;problem is adding two characters not strings 
      
   )

  )


  



(define (isKPal revStr str string1Index string2Index)
  (display " string1: ")
  (display string1Index)
  (display " string2: ")
  (display string2Index)
  (newline)

  

  (if (= string1Index 0)
      
      ;RETURN STRING2 & END Recursion 
      string2Index
      
      (if (= string2Index 0)
          
          ;RETURN STRING2 & END Recursion 
          string1Index

          (if (equal? ( string-ref str (- string1Index 1)  ) ( string-ref revStr (- string2Index 1)  ) )
              
              (isKpal revStr str (- string1Index 1) (- string2Index 1) )
              
              (+ 1 (if
                        (< (isKPal revStr str (- string1Index 1) string2Index ) (isKPal revStr str string1Index (- string2Index 1) ) )
         
                        (isKPal revStr str (- string1Index 1) string2Index )
         
                        (isKPal revStr str string1Index (- string2Index 1) )) ))

          ) )
  
)

(define (k-palindrome s k)

  (define inputLength ( string-length s ) )

  (if (or (> (isKPal (list->string (reverse (string->list s))) s inputLength inputLength) (* k 2) )
          (= (isKPal (list->string (reverse (string->list s))) s inputLength inputLength) (* k 2) ))
  (display #t)
  (display #f))

  )
  
|#


;Question 4
;---------------parta----------------
;a)
"Question 4"

"a)"

(define (f n)
  (if (< n 3)
      n
      ( + (+ (f (- n 1) ) (* 2 (f (- n 2))) ) ( * 3 (f (- n 3))))
  ))

#|
;substitution
(f 5)
( + (+ (f (- n 1) ) (* 2 (f (- n 2))) ) ( * 3 (f (- n 3))))
( + (+ (f 4 ) (* 2 (f 3)) ) ( * 3 (f 2)))
( + (+ ( + (+ (f 3 ) (* 2 (f 2)) ) ( * 3 (f 1))) (* 2 ( + (+ (f 2 ) (* 2 (f 1)) ) ( * 3 (f 0)))) ) ( * 3 2))
( + (+ ( + (+ ( + (+ (f 2 ) (* 2 (f 1)) ) ( * 3 (f 0))) (* 2 (f 2)) ) ( * 3 (f 1))) (* 2 ( + (+ (f 2 ) (* 2 (f 1)) ) ( * 3 (f 0)))) ) ( * 3 2))
( + (+ ( + (+ ( + (+ 2 (* 2 1) ) ( * 3 0)) (* 2 2) ) ( * 3 1)) (* 2 ( + (+ 2 (* 2 1) ) ( * 3 0))) ) ( * 3 2))
( + (+ ( + (+ ( + (+ 2 2 ) 0) 4 ) 3) (* 2 ( + 4 0)) ) 6)
( + (+ ( + (+ ( + 4 0) 4 ) 3) (* 2 4) ) 6)
( + (+ ( + (+ 4 4 ) 3) 8 ) 6)
( + (+ ( + 8 3) 8 ) 6)
( + (+ 11 8 ) 6)
( + 19 6)
> 25


"b)"

(define (f-iter n)
  (cond ((< n 3) n)
        ()))


|#

"Question 5"

"a)"

  (define (factorial n)
        (if (= n 1)
            1
            (* n (factorial (- n 1)))))

(define (pascals n k)

 (/ (factorial n) (* (factorial k) (factorial (- n k) ) ) )

)

"b)"
#|
(define (printTriangleHelp line n )
  
  (newline)

  (display (pascals n line) )

  (if (> (+ line 1) n )

      (display " ")

      (printTriangleHelp (+ line 1) n )
      
      )
  )
|#
(define (printTriangle n)
  
  (if (= n 0)
      1
      (printTriangleRow 0 n)
  
  ))


(define (printTriangleRow line n )

  (if (> (+ line 1) n )
      (display " ")
      (display (pascals n line))

      )
  )

