;Ali Hassam 100972357
;COMP3007- Assignment 1
;Due Jan 29 2018

;Question (1) Rewrite the following expressions as Scheme expressions:
(display "\n\nQuestion 1:\n")

;(1a)[2 marks] 1 + -2 + 3 + -4 + 5 + -6
(display "\n(a) 1 + -2 + 3 + -4 + 5 + -6")
(display "\nExpected Result: -3 | Result: ")
(+ 1 3 5 -2 -4 -6)

;(1b)[2 marks] 7+12*5-((8/2 + 3)*(9/3 - 1))
(display "\n(b) 7+12*5-((8/2+3)*(9/3-1))")
(display "\nExpected Result: 53 | Result: ")
(- (+ 7 (* 12 5)) (* (+ (/ 8 2) 3) (- (/ 9 3) 1)))

;(1c)[2 marks] (36/9*(6/2+6/3)*5*10+9)*2
(display "\n(c) 36/9*(6/2+6/3)*5*10+9)*2")
(display "\nExpected Result: 2018 | Result: ")
(* (+(* (/ 36 9) (+ (/ 6 2) (/ 6 3)) 5 10) 9) 2)

;(1d)[2 marks] (1/4 + 3/7) * 4.5+2.7
(display "\n(d) 36/9*(6/2+6/3)*5*10+9)*2")
(display "\nExpected Result: 5.753571428571428 | Result: ")
(+ (* (+ (/ 1 4) (/ 3 7)) (/ 9 2)) 2.7)

;Question(2)
(display "\n\nQuestion 2:\n")

;2(a) [2 marks] Create a procedure (reciprocal x) which returns the reciprocal of x
;(that is, x-1 = 1/x), unless that would produce an error,
;in which case the function should return false (#f).
(define (reciprocal x)
  (cond
    ((= x 0) #f) ;cant div by 0
    (else (/ 1 x)) ;return recip
  )
)
(display "(a)\n")
(display "(reciprocal 5/2) : Expected Result: 2/5 | Result:")
(reciprocal 5/2)
(display "(reciprocal -5): Expected Result: -1/5 | Result:")
(reciprocal -5)
(display "(reciprocal 0) Expected Result: #f | Result:")
(reciprocal 0)

;2(b) [1 mark] Create the procedure for the following function:
;f(x)= 3x + 12/(x+1)
(define (f x)
  (if (eq? x -1)
      #f
      (+ (* 3 x) (/ 12 (+ 1 x)))))
(display "\n(b)\n")
(display "(f 3) : Expected Result: 12| Result:")
(f 3)
(display "(f 1) Expected Result: 9| Result:")
(f 1)

;2(c) [1 mark] Create the procedure for the following function:
;g(x) = x * 4^-1
(define (g x)
  (* x (/ 1 4)))
(display "\n(c)\n")
(display "(f 3) : Expected Result: 3/4| Result:")
(g 3)
(display "(f 1) Expected Result: 1/4| Result:")
(g 1)


;2(d)[3 marks] Provide the substitution model using applicative order for (+(f (- (* 3 2) 1))(g (reciprocal (/ 1 12)))).

;(+(f (- (* 3 2) 1))(g (reciprocal (/ 1 12))))
;(+(f (- 6 1))(g 12))                            
;(+(f 5) (g 12))
;(+(+ (* 3 5)(/ 12 (+ 5 1)))(g 12))
;(+(+ 15 (/ 12 (+ 5 1))) (g 12))
;(+(+ 15 (/ 12 6)) (g 12))
;(+(+ 15 2)(g 12))
;(+ 17 (g 12))
;(+ 17 (* 12 (reciprocal 4)))
;(if 4=0 #f else 1/4)
;(+ 17 (* 12 1/4))
;(+ 17 3)
;20


;2(e)[5 marks] Provide the substitution model using normal order for (+(f (- (* 3 2) 1))(g (reciprocal (/ 1 12)))).

;(+(f (- (* 3 2) 1))(g (reciprocal (/ 1 12))))
;(+ (+ (* 3 (-( * 3 2) 1)) (/ 12(+ 1 (-(* 3 2) 1))))(g (recriprocal (/ 1 12)))
;(+ (+(* 3 5)(/ 12 6))(g(recriprocal(/ 1 12)))
;(+ 17 (g recriprocal(/ 1 12)))
;(+ 17 (* recriprocal(/1 12)(/ 1 4)))
;(if 12=0 #f else 12/1)
;(+ 17(* 12 1/4))
;(+ 17 3)
;20

;Got the value 20 for both examples 2d and 2e, I believe these values are both correct
;despite change between applicative and normal order.


;Question 3
;Create the procedure (quadratic a b c) to calculate the roots of a quadratic formula
;with coefficient a, b, and c. For simplicity (since we have not yet covered pairs or lists),
;use only the + version of the equation in place of the ±.
;The procedure should return false (#f) if there are no real roots
(display "\n\nQuestion 3:\n")

(define (quadratic a b c)
  (cond ((< (- (* b b) (* 4 a c)) 0) #f) ((= a 0) #f)
        (else (/ (+ ( * -1 b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a)))
  )
)

(display "\n(quadratic 1 2 0): Expected Result:  0 | Result: ")
(quadratic 1 2 0)

(display "\n(quadratic 2 5 2): Expected Result:  -1/2 | Result:  ")
(quadratic 2 5 2)

(display "\n")



;Question 4
(display "\nQuestion 4:\n")
;(4a) Construct a Scheme procedure (fib n) that computes this value for a Fibonacci Sequence.
;You may wish to make use of the built-in sqrt/1 and expt/2 procedures
;for square roots and exponents respectively.

(define (fib n)
  (- 
    (* 
      (/ 1 (sqrt 5))
      (expt (/ (+ 1 (sqrt 5)) 2) n)) 
    (* 
      (/ 1 (sqrt 5)) 
      (expt (/ (- 1 (sqrt 5)) 2) n)))
  )

(display "\n4(a) (fib 3) Expected Result: 2| Result: ")
(fib 3)


;(4b) [2 marks] Validate the behaviour of part (a)
;by implementing the recursive definition of the fibonnaci sequence as done in lecture 
(define (fibrec n)
    (cond  ((= n 0)0)
           ((= n 1)1)
           (else (+(fib(- n 1))
                 (fib (- n 2)))
           )
     )
)
(display "\n4(b) (fibrec 3) Expected Result: 2| Result: ")
(fib 3)

;(4c) [4 marks] Construct a procedure called testfib that takes in a value n and a precision value.
;The procedure should output the results of calling parts (a) and (b) on the value n,
;and return true if the two values are within the specified precision.
(define (testfib n precision)
  (cond
    ((>= (abs precision)(abs (- (fibrec n)(fib n))))#t)
    (else #f)
  )
)
(display "\n(4c)\n(testfib 40 0.000000001) Expected Result: #f| Result: ")(testfib 40 0.000000001)
(display "\n(testfib 15 0) Expected Result: #t| Result: ")(testfib 15 0)
(display "\n(testfib 20 0)Expected Result: #f| Result:  ")(testfib 20 0)



;Question 5
(display "\n\nQuestion 5: (please refer to comments for written solution)\n")
;[4 marks] The following program can be used to determine if a given interpreter
; is using applicative-order or normal-order evaluation.

;(define (p)(p))
;(define (test x y)
;   (if (= x 0)
;      x
;      y))	
;(test 0 (p))

;(a)What will be the behaviour of this code on an interpreter that uses applicative-order evaluation? Explain why\n\n")

;An interpreter that uses an applicative-order evaluation will cause an infinite loop.
;The test procedure will never get executed becasyse the arguments for the procedure will get evaluated first.
;In this example it will go y, then evaluate p.  By doing so the test procedure never gets activated, thus and infinite loop."

;(b)What behaviour will be observed with an interpreter that uses normal-order evaluation? Explain why.\n")

;An interpreter that uses an normal-order evaluation will result in 0.  The parameter x will be exaluated first and this will cause it to terminate.
;Y does not get evaluated and the return value will be 0.")


;Question 6
(display "\n\nQuestion 6: (please refer to comments this is just a simple test)\n")
(define (a-b a b)
   ((cond ((> b 0) +)((= b 0) -)(else *)) a b))

;Your answer should describe what happens for all integer values of a and b.
;Illustrate your answer using the substitution model.


;Subsitution model using: a= 2 b= 3

;(define (a-b 2 3)
;   ((cond ((> 3 0) +)((= 3 0) -)(else *)) 2 3))
; => (+ 2 3)
; => return 5

(display "\n(a-b 2 3) Expected Result: 5| Result: ")
(a-b 2 3)

;The above example illustrates if B is not negative and greater than 0, we use addition. 

;Subsitution model using: a= 2 b= 0

;(define (a-b 2 0)
;   ((cond ((> b 0) +)((= b 0) -)(else *)) a b))
; => (- 2 0)
; => return 2

(display "\n(a-b 2 0) Expected Result: 2| Result: ")
(a-b 2 0)


;The above example illustrates if B is not negative and not greater than 0, so its 0, we use subtraction. 

;Subsitution model using: a= 2 b= -3

;(define (a-b 2 -3)
;   ((cond ((> -3 0) +)((= -3 0) -)(else *)) 2 -3))
; => (* 2 -3)
; => return -6

(display "\n(a-b 2 -3) Expected Result: -6| Result: ")
(a-b 2 -3)

;The above example illustrates if B is negative we trigger the else and do multipication.

;Therefore the 'b' value determines if we do + , - , or * with a based on if 'b' is >0, =0, or <0 respectively.













