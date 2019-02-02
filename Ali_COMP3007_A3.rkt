;Ali Hassam 100972357
;COMP3007- Assignment 3
;Due March 7 2018

;Question 1 --------------------------------------

;An interval is defined by an upper and lower bound.
;You are required to write the procedures: add-interval, subtract-interval, multiply-interval,
;and divide-interval that add, subtract, multiply, and divide two intervals respectively.
;You should create a constructor procedure make-interval, along with procedures to access the upper and lower bounds and display intervals.
;You must deal with intervals that span zero.

;in: interval | out: upper limit | interval to upper limit func
(define (upperbound x)
  (cdr x))
;in: interval | out: lower limit | interval to lower limit function
(define (lowerbound x)
  (car x))

;in: lower & upper bounds | out: constant | interval return func
(define (make-interval lower upper)
  (cons lower upper))

;in: 2 intervals | out: 1 interval | multiply intervals together
(define (multiply-interval i1 i2)
  (define all (list (* (upperbound i1) (upperbound i2)) (* (upperbound i1) (lowerbound i2)) (* (lowerbound i1) (upperbound i2)) (* (lowerbound i1) (lowerbound i2))))
  (make-interval ( minList all ) ( maxList all )))
;in: 2 intervals | out: 1 interval | divides intervals together
(define (divide-interval list1 list2)
  (cond ((= (car list2) 0) (display "error"))
        ((= (cdr list2) 0) (display "error"))
        (else (multiply-interval list1
                         (make-interval (/ 1 (car list2)) (/ 1 (cdr list2)))))))

;in: 2 intervals | out: 1 interval | adds intervals together
(define (add-interval i1 i2)
  (make-interval ( + (lowerbound i1) (lowerbound i2))
                 ( + (upperbound i1) (upperbound i2))))
;in: 2 intervals | out: 1 interval | subtracts intervals 
(define (subtract-interval i1 i2)
  (make-interval ( - (lowerbound i1) (upperbound i2))
                 ( - (upperbound i1) (lowerbound i2))))

;comparison helper function
(define (compareOperation operation lis curr)
  (if (null? lis) curr
  (if (operation (car lis) curr)
      (compareOperation operation (cdr lis) (car lis))
      (compareOperation operation (cdr lis) curr))))

;in: lis | out: max lis
(define (maxList lis)
  (compareOperation > (cdr lis) (car lis)))
;in list | out; min lis
(define (minList lis)
  (compareOperation < (cdr lis) (car lis)))


;Testing Question 1
(display "Question 1: Testing")(newline)

(define interval_1 (make-interval 2 6))
(define interval_2 (make-interval 5 10))

(display "(add-interval interval_1 interval_2)| Expected Result: (7 . 16) Result: ")
(add-interval interval_1 interval_2)
(newline)

(display "(subtract-interval interval_1 interval_2)| Expected Result: (-8 . 1) Result: ")
(subtract-interval interval_1 interval_2)
(newline)

(display "(multiply-interval interval_1 interval_2)| Expected Result: (10 . 60) Result: ")
(multiply-interval interval_1 interval_2)
(newline)

(display "(divide-interval interval_1 interval_2)| Expected Result: (1/5 . 1 and 1/5 ) Result: ")
(divide-interval interval_1 interval_2)
(newline)



;Question 2 --------------------------------------

;(2a) [4 marks] What are the corresponding definitions of special-car and special-cdr?
;(Note: do not use names that conflict with the existing cons and car).
;For this representation, verify that (special-car (special-cons x y)) yields x for any objects x and y and (special-cdr (special-cons x y)) yields y.
;You may not use the built-in cons, car, cdr, or list in your implementation.

;given
(define (special-cons x y)
    (lambda (m) (m x y)))

(define (special-cdr specialcon)
  (define (return_special-cdr a b) b)
  (specialcon return_special-cdr))

(define (special-car specialcon)
  (define (return_special-car a b) a)
  (specialcon return_special-car))

;Testing Question 2a
(display "Question 2a: Testing")(newline)

(define x1 (special-cons 1 2))
(define x2 (special-cons 5 3))

(display "(special-car x1) | Expected Result: 1  Result: ")
(special-car x1)(newline)

(display "(special-car x2) | Expected Result: 5  Result: ")
(special-car x2)(newline)

(display "(special-cdr x1) | Expected Result: 2  Result: ")
(special-cdr x1)(newline)

(display "(special-cdr x2) | Expected Result: 3  Result: ")
(special-cdr x2)(newline)



;(2b) [3 marks] Create a procedure (triple x y z) that constructs a triplet.
;You may not use cons, car, cdr or lists in the triplet procedures.
;Next, write procedures first, second, third that return the first, second, third element respectively


;create a triple
(define (triple a b c) 
  (special-cons a(special-cons b c)))

;gets first of the triple
(define (first triplet) 
  (special-car triplet))
;returns second of triple
(define (second triplet)
  (special-car(special-cdr triplet)))
;returns third of the triple
(define (third triplet) 
  (special-cdr(special-cdr triplet)))

;Testing Question 2b
(newline)
(display "Question 2b: Testing")(newline)

(define t (triple 1 2 3))
(define t2 (triple 7 8 9))
(display "(define t (triple 1 2 3)\n")
(display "(define t2 (triple 7 8 9)\n\n")

;t1 tests
(display "(first t) | Expected Result: 1  Result: " )
(display (first t))(newline)

(display "(second t) | Expected Result: 2  Result: " )
(display (second t))(newline)

(display "(third t) | Expected Result: 3  Result: " )
(display (third t))(newline)(newline)

;t2 tests
(display "(first t2) | Expected Result: 7  Result: " )
(display (first t2))(newline)

(display "(second t2) | Expected Result: 8  Result: " )
(display (second t2))(newline)

(display "(third t2) | Expected Result: 9  Result: " )
(display (third t2))(newline)



;Question 3 --------------------------------------

;(3a) [2 marks] Create a procedure (count x L) that returns the number of instances of the value x in the list L.

(define (count x L)
  (define (b c l a)
    (if (null? l)
        a
        (if (eq? (car l) c)
            (b c (cdr l) (+ a 1))
            (b c (cdr l) a))))
  (b x L 0))

(display "\nQuestion 3a: Testing")(newline)
(display "(count 7 '(3 6 7 7 7 3) | Expected Result: 3  Result: " )
(count 7 '(3 6 7 7 7 3))


;(3b) [4 marks] Create a procedure (mode L) that returns the most common value in the list L.
;In the case of a tie return the value closest to the front of the list (ie the first one encountered).

;blank



;(3c) [2 mark] Create a procedure (after L n) that returns a list that contains all BUT the first n items of L

(define (after L n)
  (cond ((null? L) '()) ;empty 
        ((<= n 0) L)
        ((> n 0) (after (cdr L)
                        (- n 1)))
  )
  )

(display "\nQuestion 3c: Testing")(newline)
(display "(after '(3 4 4 5 7 7) 2) | Expected Result: '(4 5 7 7)  Result: " )
(after '(3 4 4 5 7 7) 2)


;(3d) [4 marks] Create a procedure (splice L i A) that splices the list A into the list L at index i

(define (splice L i A)
  (cond ((null? L) A)
        ((> i 0) (cons (car L)
                       (splice (cdr L) (- i 1) A)))
        ((= i 0) (append A L)))
  )


(display "\nQuestion 3d: Testing")(newline)
(display "(splice '(1 2 3 4) 2 '(7 8 9 10)) | Expected Result: (1 2 7 8 9 10 3 4) Result: " )
(splice '(1 2 3 4) 2 '(7 8 9 10))



;(3e) [2 marks] Create a procedure (splice2 L i n A) that splices the list A into the list L at index i, and removes n items from the original list L starting at i

(define (splice2 L i n A)
  (cond ((null? L) A) 
        ((> i 0)
         (cons (car L)(splice2 (cdr L) (- i 1) n A)))
        ((= i 0)
         (if (> n 0) (splice2 (cdr L) 0 (- n 1) A)
                     (append A L)))))

(display "\nQuestion 3e: Testing")(newline)
(display "(splice2 '(1 2 3 4) 2  2 '(7 8 9 10)) | Expected Result: (1 2 7 8 9 10) Result: " )
(splice2 '(1 2 3 4) 2  2 '(7 8 9 10))



;Question 4 --------------------------------------

;(4a) [3 marks] Write a procedure height that takes as argument an arbitrarily deeply nested list (ie a tree)
;and returns the maximum depth of any item in any of its sublists
;the depth of an object in a list is the number of cars that can be applied to it, not necessarily in a row


(define (height list)
  (define (iter list depth)
    (cond ((not (pair? list)) depth)
          ((not (pair? (car list)))
           (iter (cdr list) (+ 1 depth)))
          ((pair? (car list))(max (iter (car list) depth) (iter (cdr list) depth)))
     )
    )
  (iter list 0))

(display "\nQuestion 4a: Testing")(newline)
(display "((height '(a (b) c)) | Expected Result: 2 Result: " )
(height '(a (b) c))


;(4b) [3 marks] Write a function called tree-filter for trees that is analogous to the built-in filter for flat lists (see Section 4.4).
;This function should apply a predicate to every element of the tree, keeping only those that pass, and return the results in a tree of the same shape as the original.

;blank


;(4c) [3 marks] Write a function flattenList that takes a tree as an argument and returns a list of the leaf values of the tree.

(define (flattenList tree)
  (cond ( (null? tree) '() ) ;empty
        ( (not (pair? tree)) (list tree))
        ( else
          (append (flattenList (car tree)) (flattenList (cdr tree))))
   )
  )

(display "\nQuestion 4c: Testing")(newline)
(display "(flattenList '(10 (9 8)((7 6)(5 4))(((3 (2))))) | Expected Result: (10 9 8 7 6 5 4 3 2) Result:  " )
(flattenList '(10 (9 8)((7 6)(5 4))(((3 (2))))))


;(4d) [3 marks] Write a function level that takes a tree and a level index as arguments and returns all items from the tree that exist at the given level (in a single list).
;A level, i, in a tree is the set of all nodes in the tree with depth = i.

;blank


;Question 5 --------------------------------------

;(5A) [5 marks] Write stream analogues of some familiar list processing functions:

;helper functions 
(define (stream-car s)(car s))

(define (stream-cdr s)(force (cdr s)))
(define empty-stream '())

(define (stream-null? stream )
    (null? stream))

(define-syntax cons-stream 
  (syntax-rules ()
    ((cons-stream a b)(cons a (delay b)))))



;   (i)[1 marks] (stream-first n str) -- makes a new stream of the first n items in str


;return cons-stream of n elements
(define  (stream-first n str)
  (stream-first-helper 1 n str))

(define (stream-first-helper c t str)
  (if (= c t)
      (cons-stream (stream-car  str) empty-stream)
      (cons-stream (stream-car str) (stream-first-helper (+ 1 c) t (stream-cdr str)))))


;   (ii)[2 marks] (list->stream lis) -- makes a stream from list lis


;list to stream converter
(define (list->stream lis)
  (if (null? (cdr lis))
      (cons-stream (car lis) empty-stream)
      (cons-stream (car lis) (list->stream (cdr lis))))
  )


;   (iii)[2 marks] (stream->list str) -- reverse of above (assume str is finite)


;stream to list converter
(define (stream->list str)
  (if (null? str)
      empty-stream
      (if (null? (stream-cdr str))
          (stream-car str)
          (cons (stream-car str) (stream->list (stream-cdr str))))))

  
;(5B) [6 marks] Write stream generators to help test the above functions:

;   (i)an infinite stream of 1's

(define infinStream (cons-stream 1 infinStream))
(display "\nQuestion 5b (i) Testing:\n")
  
(display "(stream->list (stream-first 5 infinStream)  | Expected Result: 1 1 1 1 1 Result:")
(display (stream->list (stream-first 5 infinStream)))(newline)



;   (ii)an infinite stream of all odd integers


(define (odd-numgenerator)(odd-numgenerator-helper 1))
;creates a stream with first value of n and increases by an even value (2) to maintain an odd output
(define (odd-numgenerator-helper n)(cons-stream n (odd-numgenerator-helper (+ n 2))))
(display"\nQuestion 5b (ii) Testing:")(newline)
(display "(stream->list (stream-first 5 infinStream)  Expected Result: 1 3 5 7 9| Result:")
(display (stream->list(stream-first 5(odd-numgenerator))))



;   (iii)an infinite stream of the values of function f(n) = f(n-1) + 2f(n-2) + 3f(n-3) (given f(n)=n iff n<4).

(define (stream-infin)
  (cons-stream 1
  (cons-stream 2(it-stream-infin 1 2 3))))

(define (it-stream-infin a b c )
  (cons-stream c (it-stream-infin b c (+ c (* 2 b) (* 3 a)))))


(display"Question 5b (iii) Testing:")(newline)
(display "(stream->list(stream-first 7(stream-infin)))  Expected Result: (1 2 3 10 22 51 . 125)| Result:")
(stream->list(stream-first 7(stream-infin)))


;(5c) [2 marks] Write a function combine that takes two streams and combines them using a function passed in as a third parameter.
;You may assume the both streams are of equal length (or infinite). (combine + (1 2 3 ...) (4 5 6 ...)) → (5 7 9 ...)

(define (combine procedure streamA streamB)
  (cond ((eq? streamA '())'())
        ((eq? streamB '())'())
        (else (cons-stream
               (procedure (stream-car streamA) (stream-car streamB))
               (combine procedure (stream-cdr streamA)
                        (stream-cdr streamB))))
        )
  )
