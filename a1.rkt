;Japinder Sandhu
;101021899
;Course: COMP 3007
;Date: 2020-01-24


"---------------------------------------------"
"Question 1"

"1 a)"
;[2 marks] 1 + -2 - 3 + 4 + -5 + 6

(+ (- 3) (+ (- 5) (+ (- 2) (+ ( + 1 4 ) 6))))

(newline)
"1 b)"
; 20-1+((26/2 + 2)*(20/5 - 2))
( + ( * ( + ( / 26 2 ) 2 ) ( + ( / 20 5 ) (- 2) ) ) ( + 20 (- 1) ) )

(newline)
"1 c)"
;(63/9*(12/4-14/(13+2))+12)/4
(/ ( +(* (/ 63 9) (- (/ 12 4) (/ 14 (+ 13 2))))12)4)

(newline)
"1 d)"
;((50*20)*2)+((17/4)+3.85)+((30*2)-48)
( + ( + (* 2 (* 50 20)) ( + ( / 17 4 ) 3.85 ) ) ( - ( * 30 2 ) 48 )  )

(newline)
"---------------------------------------------"
"2 a)"
; Question 2. a)[1 mark] Create a procedure (cube x) that computes x3.



( define ( cube x ) 

( * ( * x x) x ) )


(newline)
"test1 (cube 2)expected result 8"
(cube 2)

(newline)
"test2 (cube -2)expected result -8"
(cube -2)
(newline)

"test3 (cube 10)expected result 1000"
(cube 10)
(newline)

(newline)
"2 b)"
;[2 marks] Create a procedure that computes the following function:
;	f(x) = 3x2 + 4

(define ( f x ) ( + ( * ( * x x ) 3 ) 4 ) )


(newline)
"test1 (f 2)expected result 16"
(f 2)

(newline)
"test2 (f -2)expected result 16"
(f -2)
(newline)

"test3 (f 10)expected result 304"
(f 10)
(newline)

"2 c)"
; Question 2. c)[2 marks] Create a procedure that computes the following function:
;	g(x) = f(2x) - 2x^3


(define ( g x ) ( - ( f ( * 2 x ) ) ( * 2 ( cube x ) ) ) )


(newline)
"test1 (g 2)expected result 16"
(f 2)

(newline)
"test2 (g -2)expected result 16"
(f -2)
(newline)

"test3 (g 10)expected result 304"
(f 10)
(newline)

"2 d)"

; Question 2. d) [2 marks] Create a procedure that computes the following function:
;	h(x) = 2f(x/2) + g(x)



( define ( h x ) ( + ( * 2 ( f ( / x 2 ) )) ( g x) ) ) 

(newline)
"test1 (h 2)expected result 50"
(h 2)

(newline)
"test2 (h -2)expected result 82"
(h -2)
(newline)

"test3 (h 10)expected result -638"
(h 10)
(newline)

; Question 2. e) [2 marks] Provide the substitution model using applicative order for (h (* 2 3)).

;( h ( * 2 3 ) )

;( h 6 )

;( + ( * 2 ( f ( / 6 2 ) ) ) ( g 6 ) )

;( + ( * 2 ( f 3 ) ) ( g 6 ) )

;( + ( * 2 ( f 3 ) ) ( g 6 ) )

;( + ( * 2 31 ) ( 4 ) )

;( + 62 4 )

;66

; Question 2. f) [4 marks] Provide the substitution model using normal order for (h (* 2 3)).

;(h (* 2 3))
;(+ (* 2 (f (/ (* 2 3) 2))) (g (* 2 3)))
;(+ (* 2 (+ (* 3 (* (/ (* 2 3) 2) (/ (* 2 3) 2))) 4)) (g (* 2 3)))
;(+ (* 2 (+ (* 3 (* (/ 6 2) (/ 6 2))) 4)) (g (* 2 3)))
;(+ (* 2 (+ (* 3 (* 3 3)) 4)) (g (* 2 3)))
;(+ (* 2 (+ (* 3 9) 4)) (g (* 2 3)))
;(+ (* 2 (+ 27 4)) (g (* 2 3)))
;(+ (* 2 31) (g (* 2 3)))
;(+ 62 (g (* 2 3)))
;(+ 62 (- (f (* 2 (* 2 3))) (* 2 (cube (* 2 3)))))
;(+ 62 (- (+ (* 3 (* (* 2 (* 2 3)) (* 2 (* 2 3)))) 4) (* 2 (cube (* 2 3)))))
;(+ 62 (- (+ (* 3 (* (* 2 6) (* 2 6))) 4) (* 2 (cube (* 2 3)))))
;(+ 62 (- (+ (* 3 (* 12 12)) 4) (* 2 (cube (* 2 3)))))
;(+ 62 (- (+ (* 3 144) 4) (* 2 (cube (* 2 3)))))
;(+ 62 (- (+ 432 4) (* 2 (cube (* 2 3)))))
;(+ 62 (- 436 (* 2 (cube (* 2 3)))))
;(+ 62 (- 436 (* 2 (cube 6))))
;(+ 62 (- 436 (* 2 (* 6 6 6))))
;(+ 62 (- 436 (* 2 216)))
;(+ 62 (- 436 432))
;(+ 62 4)
;66


"---------------------------------------------"
"Question 3."

(newline)
"3 a)"
; Question 3. a)

(define (helper a b c) ( / ( + (* -1 b) (sqrt ( - ( * b b ) (* (* 4 a) c) ) ) ) (* 2 a) ) )

(define (quadratic a b c) (if (< (helper a b c) 0) (display #f) (helper a b c) ) )

  

                                                                        
    ;Real Roots:
"test1 a=1 b=12 c=32 expected result #f"
(quadratic 1 12 32)
(newline)
"test2 a=2 b=24 c=64 expected result #f"
(quadratic 2 24 64)
(newline)
"test 3 a=3 b=-11 c=-4 expected result 4"
(quadratic 3 -11 -4)


(newline)
"3 b)"
; Question 3. b)

(newline)
( define ( convert num a b)
   ( cond (( equal? a "KB")
          ( cond (( equal? b "B")(* num 1000))
                 ((equal? b "Kib")(/ num 1.024))
                 (else "error")
                       )
                 )
          (( equal? a "B")
           (cond (( equal? b "KB")(/ num 1000))
                 (( equal? b "Kib")(/ num 1024))
                 (else "error")))
          
          (( equal? a "Kib")
           (cond (( equal? b "B") (* num 1024))
                 ((equal? b "KB") (* num 1.024))
                 (else"error"))))

          )

(newline)
"test 1 1000 KB to B"
( convert 1000 "KB" "B")

(newline)
"test 2 500 Kib to B"
( convert 500 "KB" "B")

(newline)
"test 3 32 B to KB"
( convert 500 "KB" "B")
"---------------------------------------------"


; Question 4. a) 
;if user returns null,
;(define a (read))
"Question 4"
"a)"

(Define var (read))

(newline)
(define (user-num var)
        (display var)
           (read)
  (newline)
  (if (number? var) (display var)
      (display "0")))
(newline)
(display "Enter a number: ")
(newline)
"Test 1 Input 123 Expected output 123"
;(user-num var)


(newline)


"Question 4"

"b)"

(newline)
; Question 4. b)

(define ( sci-exponent number )

  (if (< number 0) (Inexact->exact  (floor ( / ( log (* -1 number) ) 2 ) ) )  (inexact->exact (floor (/ (log number) (log 10))))))   


(newline)
"Test 1"
(sci-exponent 1.234)
(newline)
"Test 2"
(sci-exponent 12345)
(newline)
"Test 3"
(sci-exponent 0.001234)
(newline)
"Test 4"
(sci-exponent -12345)
(newline)


; Question 4. c)

"c)"

(define ( sci-coefficient number )
  ( if (> number 1 )
       ( / number ( expt 10 (abs ( floor ( / ( log number ) 2 ) ) ) ) )
       (/ number (expt 10 ( sci-exponent number ) ) ) ) )

(newline)
"Test 1"
(sci-coefficient 1.234)
(newline)
"Test 2"
(sci-coefficient 12345) 
(newline)
"Test 3"
(sci-coefficient 0.001234)
(newline)

; Question 4. d)

"d)"



(define sci-read (read))

(define (sci-num)

  (display "Enter a number:" )

  (begin ( display  ( sci-coefficient sci-read )  )
         ( display "x10^" )
         ( display  ( sci-exponent sci-read )  ) ) )

(sci-num)

(newline)

(sci-num) "Enter a number: 12345 → 1.2345x10^4"



"----------------------------------------------------------------------------------------------------------------------------------------------------------------------"
;Question 5)
(define (test x y) 
    (if (= x 0)
        x
        y))

    ;(test 0 (/ 3 0))
;Question 5a) This function tries to divide 3 by 0 resulting in an error so it is unable to return the test function 
;Questoin 5b) In this case it will be able to print 0 as it will not evaluate the inside parameter until it first returns x.
"---------------------------------------------------------------------------------------------------------------------------------------------------------------------"
"Question 6"
;Question 6)
;The foo functions has 3 conditions 1) if b is > 0 then we add a+b 2) if b = 0, we * a and b 3) if b is negative we divide a and b
(define (foo a b)
		((cond ((> b 0) +)((= b 0) *)(else /)) a b))

;substitution model
(foo 1 0)
(* 1 0)
0
(display "Expected :0") (newline)
(foo 2 -1)
(/ 2 -1)
-2
(display "Expected :-2") (newline)
(foo 1 2)
(+ 1 2)
3
(display "Expected :3") (newline)
