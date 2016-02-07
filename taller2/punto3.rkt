; Integrantes:
;  Andrés Felipe Valencia Rivera      1523227
;  Jefferson Estiven Mendoza Hoyos    1410233
;***********************************************
;
; Punto 3, Parte 1.
; listas


#lang eopl

;******************************************************************************************
;;;;; Interpretador Simple

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= list({expression}*(,))
;;                      <list-exp (list-exp)>
;;
;;  <primitive>     ::= + | - | * | add1 | sub1 | car | cdr | null | nth-list | element-list ;; se agregan primitivas de listas

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)
  (identifier
   (letter (arbno (or letter digit "?"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    ;;se implementa el tipo de las listas
     (expression
      ("list(" (separated-list expression ",")")")
     list-exp)
    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
;;primitiva para las listas
    (primitive ("car") car-prim)
    (primitive ("cdr") cdr-prim)
    (primitive ("null?") null-prim)
    (primitive ("nth-list") nth-list-prim)
    (primitive ("element-list") element-list-prim)
    ))


;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))

;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?))))

;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (list-exp (elements)
                   (let ((args (eval-rands elements env)))
                     args))
      )))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))

      ;;***************************************************
      ;;Se agregan las interfaces para el manejo de listas
      ;;la entrada de cada una de estás funciones son  del
      ;;tipo abstracto list-exp, ya trasformadas en listas
      ;;en el eval-expression
      ;;
      ;;INPUT: LISTA
      ;;****************************************************

      ;;
      ;;INPUT: LISTA
      ;;
      ;;RETURN: number
      ;;Proposito: Devuelve el primer elemento de la lista
      (car-prim  () (car(car args)))

      ;;
      ;;INPUT: LISTA
      ;;
      ;;RETURN: lista
      ;;Proposito: Devuelve el resto de la lista
      (cdr-prim  () (cdr(car args)))

      ;;
      ;;INPUT: LISTA
      ;;
      ;;RETURN: #f es un lista y tiene elementos
      ;;        #t no es una lista, no hay elementos
      ;;Proposito: Determina si una lista es null o tiene elementos
     
      (null-prim  () (null? (car args)))

      ;;
      ;;INPUT: LISTA - posición(number)
      ;;
      ;;RETURN:  number
      ;;Proposito: Obtiene un elemento en la posición determinada
      (nth-list-prim () (getElement (car args) (car (cdr args))) ) ;;(getElement (car args) (cdr args))


      ;;
      ;;INPUT: LISTA - elemento(number)
      ;;
      ;; RETURN: #f no está contenido en la lista
      ;;         #t está contenido en la lista
      
      ;;Proposito: Determina si un elemento está contenido en la lista
      (element-list-prim () (contain? (car args) (car (cdr args))))
  

      )))


;;INPUT: LISTA - posición
;;
;;RETURN:  number
;;Proposito: Obtiene un elemento en la posición determinada
(define getElement
  (lambda (list pos)
        (if (null? list)
            #f    
            (if (= pos 0)
                (car list)
                (getElement (cdr list) (- pos 1))))
    ))

;;
;;INPUT: LISTA - elemento(number)
;;
;; RETURN: #f no está contenido en la lista
;;         #t está contenido en la lista
;;Proposito: Determina si un elemento está contenido en la lista
(define contain?
  (lambda (list x)
        (if (null? list)
            #f    
            (if (= (car list) x)
                #t
                (contain? (cdr list) x)))
    ))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;Pruebas

;(show-the-datatypes)
;just-scan
;scan&parse
;(just-scan "add1(x)")
;(just-scan "add1(   x   )%cccc")
;(just-scan "add1(  +(5, x)   )%cccc")
;(just-scan "add1(  +(5, %ccccc x) ")
;(scan&parse "add1(x)")
;(scan&parse "add1(   x   )%cccc")
;(scan&parse "add1(  +(5, x)   )%cccc")
;(scan&parse "add1(  +(5, %cccc
;x)) ")

;(define caso1 (primapp-exp (incr-prim) (list (lit-exp 5))))
;(define exp-numero (lit-exp 8))
;(define exp-ident (var-exp 'c))
;(define exp-app (primapp-exp (add-prim) (list exp-numero exp-ident)))
;(define programa (a-program exp-app))
;(define una-expresion-dificil (primapp-exp (mult-prim)
;                                           (list (primapp-exp (incr-prim)
;                                                              (list (var-exp 'v)
;                                                                    (var-exp 'y)))
;                                                 (var-exp 'x)
;                                                 (lit-exp 200))))
;(define un-programa-dificil
;    (a-program una-expresion-dificil))

(interpretador)
