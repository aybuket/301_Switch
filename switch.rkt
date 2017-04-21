;Koc University 
;COMP301 Programming Language Concepts Fall 2016
;Programming Assignment I
;Due Date: 11/03/2016 - 23:55

;Name: Aybüke Buket Akgül
;ID: 27772

;Before start to work on the code please read the instructions written on the top of this file.
;You should first install the "eopl" package. To install the package in DrRacket:
;File->Install Package->Type eopl as package source->Click install
;
;If you are using a Unix machine, following command should also work:
;raco pkg install eopl
;
;Note that the installed DrRacket version in the Computer labs does not support the package installation.
;You should work on your own computer.

;In this assignment you will add to the defined language (let) a facility that adds a switch expression similar to C language.
;Use the following grammar
;Expression :: = switch Expression { case Expression : Expression }∗  default : Expression end
;In this expression, the expressions on the left-hand sides of the :'s are evaluated in
;order until the value of one of them matchs with the value of the first expression. Then the value of the entire expression
;is the value of the corresponding right-hand expression. If none of the cases matches,
;the expression should return the righ-hand expression of the default case. The values of the case Expressions must be number,
;so the value of the first Expression must also be number.
;You can check the example test programs at the end of the file to see the usage of the switch.
;You should write at least 3 additional test cases.
;Do not forget to write your name and id to the specified places at the top of the page.

;Upload your file to the HOMEWORK folder on the F Drive.
;Exact path: /NetStorage/DriveF@VOL/COURSES/UGRADS/COMP301/HOMEWORK
;Name your file as studentName_studentID_version_switch.rkt
;Example: JohnDoe_007_v01_switch.rkt
;Since you cannot overwrite your uploaded file, the last file (specified with the version number) will be graded.

;NOTHING WRITTEN GETS EXCHANGED!
;It is OK to verbally discuss the concepts with your friends as long as you acknowledge this at the end of the file.
;Exceptions will not be tolerated.
;
;Enjoy!

;If you read the instructions at this point, please write "I read all the instructions." under this line.
; I read all the instructions.


#lang eopl

;Interpreter

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (apply-env env var))
      
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      ;;;
      ;Your answer here
      ;;;
      (switch-exp (var1 s-cases1 s-cases2 s-def)
                  (let ((num1 (expval->num (value-of var1 env))))
                    (define (helper num1 s-cases1 s-cases2 s-default)
                      (if (null? s-cases1) (value-of s-default env)
                      (let ((s-cs (value-of (car s-cases1) env)))
                        (if (eq? num1 (expval->num s-cs)) (value-of (car s-cases2) env)
                            (helper num1 (cdr s-cases1) (cdr s-cases2) s-default)))))
                    (helper num1 s-cases1 s-cases2 s-def)))
      )))

;Datatypes: program and expression

(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp (exp1 expression?) (exp2 expression?) (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp (var symbol?) (exp1 expression?) (body expression?))
  ;;;
  ;Your answer here
  ;;;
  (switch-exp (var1 expression?)
              (s-cases1 (list-of expression?))
              (s-cases2 (list-of expression?))
              (s-def expression?))
  )

;Datatypes: expval

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?)))

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;Environment records
(define empty-env-record
  (lambda () 
    '()))

(define extended-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-env-record? null?)

(define environment?
  (lambda (x)
    (or (empty-env-record? x)
        (and (pair? x)
             (symbol? (car (car x)))
             (expval? (cadr (car x)))
             (environment? (cdr x))))))

(define extended-env-record->sym
  (lambda (r)
    (car (car r))))

(define extended-env-record->val
  (lambda (r)
    (cadr (car r))))

(define extended-env-record->old-env
  (lambda (r)
    (cdr r)))

;Environment
(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;Let lang parser

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    
    (expression
     ("switch" expression (arbno "case" expression ":" expression) "default" ":" expression "end")
     switch-exp)
    ))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;Test Cases
(define test1
  "switch x
  	default : 0
	end")

(define test2
  "switch -(10,5)
  	case 1 : -(x , 5)
	case 2 : -(x , 4)
   case 5 : -(x , - (x, 5))
   case 10 : 10
	default : 0
	end")

(define test3 
  "switch -(x, 9)
  case -(x , 4) : 5
  case -(x, 9) : x
  default : 1
  end")

(define test4
  "switch -(x, 1)
  case 0 : zero?(x)
  case 9 : zero?(0)
  default : 1
  end")

(display (run test1));num-val 0
(newline)
(display (run test2));num-val 5
(newline)
(display (run test3));num-val 10
(newline)
(display (run test4));bool-val #t

;Write at least three different test cases
;Your answer here

(newline)
(define mytest1
  "switch -(i,v)
  case -(0,4): zero?(0)
  case 4 : zero?(4)
  case 6 : -(v,i)
  default : 0
  end")
(display (run mytest1));bool-val #t
(newline)
(define mytest2
  "switch v
  case 0 : zero?(0)
  case 1 : 1
  case 2 : 2
  case 3 : 3
  case 4 : 4
  case 5 : v
  default : -(-(x,v),i)
  end")
(display (run mytest2));num-val 5
(newline)
(define mytest3
  "switch -(14,6)
  case 2 : x
  case 4 : zero?(i)
  case 8 : i
  default : -(x,v)
  end")
(display (run mytest3));num-val 1
(newline)
(define mytest4
  "switch -(v,x)
  case 0: 12
  case 4: 23
  case 8: 86
  default: -(i,-(v,x))
  end")
(display (run mytest4));num-val 6

