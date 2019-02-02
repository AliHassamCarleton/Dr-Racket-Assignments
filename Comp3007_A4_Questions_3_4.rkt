;Ali Hassam 100972357
;COMP 3007- Assignment 4: Question 3,4
;Due: March 23 2018


;Question 3 - - - - - - - - - - - - - - - - -

;3)   [20 marks] Define a procedure called make-list that implements the List ADT using an object oriented design in Scheme.
;The make-list procedure should return a list object.
;(Note: The List object you'll create is different from a native Scheme list, however, the backing implementation of the List object may use a standard list).

;For each of the functions, bad inputs (i.e. invalid indices) should be checked. Return #f for any operation that should return a value but fails.
;Your code should be written in such a way that the following interactions would be valid (note: this does not constitute a complete test suite):


;make-list function

(define (make-list)
  (let ((lis '()))
    (define (after L n)
      (define (iterator l i)
        (if (= i 0)
            l
            (if (null? (cdr l))
                '()
                (iterator (cdr l) (- i 1)))
            )
        )
      (iterator L n))
    (define (append a b)
      (cond((null? a) b)
           ((pair? a) (cons (car a) (append (cdr a) b) ))
           (else (cons a b) ))
      )
    (define (before l n)
      (if (= 0 n)
          '()
          (cons (car l)(before (cdr l) (- n 1)))
       )
      )


;object behaviour functions
    
;(size) - returns an integer equal to the number of items in the list
    (define (size)
      (define (count l n)
        (if (or (not (pair? l))(null? (car l)))
            n
            (count (cdr l) (+ n 1)))
        )
      (count lis 0))
    
;(get i) - returns the item stored at index i in the list
    (define (get i)
      (define (find l n)
        (if (= 0 n)
            (car l)
            (find (cdr l) (- n 1)))
        )
      (find lis i))
    
;(set i x) - modifies index i in the list to hold item x
(define (set i x)
      (set! lis (append (append (before lis i) x) (after lis (+ i 1))))
  )


    
;(add i x) - adds item x at index i in the list
    (define (add i x)
      (set! lis (append (append (before lis i) x) (after lis i)))
      )

    
;(remove i) - removes and returns the item at index i from the list
    (define (remove i)
      (set! lis (append (before lis i) (after lis (+ i 1))))
      )

   
;(print) - displays the list in the standard scheme form (e.g. (a b c d))

    (define (print)
      (display lis))
    (define (dispatch method)
      (cond ((eq? method 'size) size)
            ((eq? method 'get) get)
            ((eq? method 'set) set)
            ((eq? method 'add) add)
            ((eq? method 'remove) remove)
            ((eq? method 'print) print)
            (else (lambda() (display "Error")(display method)(newline)))))
    dispatch))


;- - - = = = = Question 3 Testing = = = = - - -
(display "- - - = = = = Question 3 Testing = = = = - - -");
(newline)
(define L1 (make-list))
(define L2 (make-list))
(display "(L1 'print) | Expected Result: ()  | Result: ")((L1 'print)) (newline)
(display "(L2 'print) | Expected Result: ()  | Result: ")((L2 'print)) (newline)
((L1 'add) 0 'a)
((L1 'add) 1 'b)
((L1 'add) 2 'c)
((L1 'add) 3 'd)
(display "(L1 'print) | Expected Result: (a b c d)  | Result: ")((L1 'print)) (newline)
((L2 'add) 0 ((L1 'get) 2))
(display "(L2 'print) | Expected Result: (c)  | Result: ")((L2 'print)) (newline)


((L1 'set) 2 'w) ;set tested
((L2 'add) 1 'v) ;add tested
((L1 'remove) 1) ;remove tested
((L2 'add) 1 't)
((L1 'add) 0 1)
(display "(L1 'print) | Expected Result: (1 a w d)  | Result: ")((L1 'print)) (newline)
((L2 'add) 0 ((L1 'get) 2))
(display "(L2 'print) | Expected Result: (w c t v)  | Result: ")((L2 'print)) (newline)

;test size
(newline)
(display "(L1 'size) | Expected Result: 4  | Result: ")((L1 'size))
(display "(L2 'size) | Expected Result: 4  | Result: ")((L2 'size)) 
((L1 'add) 0 1)
((L2 'add) 0 1)
(display "((L1 'add) 0 1)") (newline)
(display "((L2 'add) 0 1)") (newline)
(display "(L1 'size) | Expected Result: 5  | Result: ")((L1 'size))
(display "(L2 'size) | Expected Result: 5  | Result: ")((L2 'size)) (newline)




;Question 4 - - - - - - - - - - - - - - - - -

;4)   [10 marks] Add the special form let to the metacircular interpreter.
;Be sure to clearly label the changes you made. Hint: Remember let is just syntactic sugar for a lambda expression,
;so all that needs to be done is convert the let into a corresponding lambda expression, then evaluate it.

;;;; METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS
;Comp3007 - A4 - W2018

;;; From section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;; SECTION 4.1.1

;classifies the type of expression
;determines the value of an expression
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ;Let #1 - -  - - - -- - - - - - - -  - - - - - - - - - - - - - - - - -
        ((let? exp) (eval (let->combo exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;executes the procedures on the given arguments
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;recurses down the list of operands, calling eval on each of them
;returns a corresponding list of values
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;uses an ifstatement to evaluate a meta-if
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;evals a sequence of subexpressions
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;breaks up an expression to call set-variable-value to change the value of a variable
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;breaks up an expression to call define-variable to create a variable
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;;; SECTION 4.1.2

;predicate method for self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;predicate method for quote (')
(define (quoted? exp)
  (tagged-list? exp 'quote))

;accessor for quote
(define (text-of-quotation exp)(cadr exp))

;predicate checking the type of an expression
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;predicate method for variables
(define (variable? exp) (symbol? exp))

;predicate and accessors for assignment (set!) 
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


;predicate and accessors for define
(define (definition? exp)
  (tagged-list? exp 'define))

;returns the variable to be bound
;note this works for both forms of define
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;predicate and accessors for lambda
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;Let Procedures here - - - - - - - -  - - - - - - - - - - - - - - - - - 
(define (let? expr)
  (tagged-list? expr 'let))

(define let-params
  (lambda (expr)
    (map car (cadr expr))))

(define let-vals
  (lambda (expr)
    (map cadr (cadr expr))))

(define (let->combo expr)
    (let ((params (let-params expr))
          (vals (let-vals expr))
          (body (cddr expr)))
       (cons (make-lambda params body) vals)))


;predicate and accessors for if
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;constructor for if statements
;   returns: (if pred cons alt)
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
  
  
;predicate for cond
(define (cond? exp) (tagged-list? exp 'cond))

;accessors for cond
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

;converts a cond to nested ifs
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;converts a cond to nested ifs
;helper method for cond->if
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;predicate, accessors and constructor for begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;an application is any compound expression that's not one of the above
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;accessors for a list of operands
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;; SECTION 4.1.3

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;procedure objects are created by eval'ing lambdas
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))


(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;acessors for the environment
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

;adds a new binding to the front of the frame
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;adds a new frame to the front of the environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)   ;looks through one frame
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;; SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

;;;; [do later] (define the-global-environment (setup-environment))

;predicate for primitive procedures
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

;accessor for the actual function
  ;primitive values are stored in the frame as  ('primitive car '())
(define (primitive-implementation proc) (cadr proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADD NEW PRIMITIVES HERE (SUCH AS +, -, etc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;concrete definition of primitives (mapped to scheme)
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '= =)
        ;;      more primitives
        ))

;gets all primitive names
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

;gets all primitive procedures in the form ('primitive proc)
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


;applies calls the built-in apply function to execute primitive procedures
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;; Prompts
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

;;;error procedure
(define (error message . args)
  (display message)
  (forEach args (lambda (arg) (display " ")(display arg)))
  (newline)
  ('error);cause a native error
  )
;;helper for error
(define (forEach lis fun)
  (if (not (null? lis)) (begin (fun (car lis))(forEach (cdr lis) fun))))

;;; Main loop to drive metacircular interpreter
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     ;(procedure-environment object))) #| ;uncomment this for verbose environment listing
                     '<procedure-env>)) ;|#
      (display object)))

'METACIRCULAR-INTERPRETER-LOADED

;;Start the interpreter!
(define the-global-environment (setup-environment))
(driver-loop)

