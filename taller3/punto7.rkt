#lang eopl

;^;; 5-3.scm: basis for OOP interps

(let ((time-stamp "Time-stamp: <2001-05-10 16:18:14 dfried>"))
  (eopl:printf "5-3.scm - basis for OOP interps ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level and tests ;;;;;;;;;;;;;;;;

;(define run
;  (lambda (string)
;    (eval-program (scan&parse string))))
;
;(define functional-groups '(lang3-5 lang3-6 lang3-7))
;
;(define oop-groups '(oop))
;
;(define run-all
;  (lambda ()
;    (run-experiment run use-execution-outcome
;      (append functional-groups oop-groups) all-tests)))
;
;(define run-functional
;  (lambda ()
;    (run-experiment run use-execution-outcome
;      functional-groups all-tests)))
;
;(define run-oop
;  (lambda ()
;    (run-experiment run use-execution-outcome
;      oop-groups all-tests)))
;
;(define run-one
;  (lambda (test-name)
;    (run-test run test-name)))
;
;;; needed for testing
;(define equal-external-reps? equal?)

;^;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

(define the-grammar
  '((program ((arbno class-decl) expression) a-program)

    (expression (number) lit-exp)
    (expression (identifier) var-exp)   
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
   (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    (primitive ("list") list-prim)
    (primitive ("cons") cons-prim)
    (primitive ("nil")  nil-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)
    (primitive ("null?") null?-prim)

;^;;;;;;;;;;;;;;; new productions for oop ;;;;;;;;;;;;;;;;

    (class-decl                         
      ("class" identifier 
        "extends" identifier                   
         (arbno "field" identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (method-decl
      ("method" identifier 
        "("  (separated-list identifier ",") ")" ; method ids
        expression 
        )
      a-method-decl)

    (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)

    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)

    (expression                                
      ("super" identifier    "("  (separated-list expression ",") ")")
      super-call-exp)

;^;;;;;;;;;;;;;;; end new productions for oop ;;;;;;;;;;;;;;;;

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;^;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls) ;\new1
        (eval-expression exp (empty-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body)
        (closure ids body env))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands      rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression 
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      (varassign-exp (id rhs-exp)
        (setref!
          (apply-env-ref env id)
          (eval-expression rhs-exp env))
        1)
;&
      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))
;^;;;;;;;;;;;;;;; begin new cases for chap 5 ;;;;;;;;;;;;;;;;
      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply
            'initialize class-name obj args)
          obj))
      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply
            method-name (apply-env env '%super) obj args)))
;^;;;;;;;;;;;;;;; end new cases for chap 5 ;;;;;;;;;;;;;;;;
      )))
      

(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (eval-expression exp env))
      exps)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      (list-prim () args)               ;already a list
      (nil-prim () '())
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (cons-prim () (cons (car args) (cadr args)))
      (null?-prim () (if (null? (car args)) 1 0))
      )))

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;^;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))


;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;


(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        class-name))))

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        super-name))))

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        field-ids))))

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls)
        m-decls))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))
        
;^;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env))))))
               
;^;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val)))
    1))

;^;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;(define rib-find-position 
;  (lambda (sym los)
;    (list-find-position sym los)))

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

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))


;^; new for ch 5
(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec env)))

;^; waiting for 5-4-2.  Brute force code.
(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))

;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

;;;; constructing classes

(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls
              c-decl super-name field-ids)))))))

(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (map
      (lambda (m-decl)
        (a-method m-decl super-name field-ids))
      (class-decl->method-decls c-decl))))


;^;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

;^; an object is now just a single part, with a vector representing the
;^; managed storage for the all the fields. 

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name))))) ;\new1

;^;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (super-name symbol?)
    (field-ids (list-of symbol?))))

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (let loop ((host-name host-name))
      (if (eqv? host-name 'object)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)
          (let ((method (lookup-method m-name ;^ m-decl -> method
                          (class-name->methods host-name))))
            (if (method? method)
                (apply-method method host-name self args)
                (loop (class-name->super-name host-name))))))))

(define apply-method
  (lambda (method host-name self args)                ;\new5
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (extend-env-refs field-ids fields (empty-env)))))))

(define rib-find-position
  (lambda (name symbols)
    (list-find-last-position name symbols)))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

(define method-environment? (list-of method?)) 

(define lookup-method                   
  (lambda (m-name methods)
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of classes (not class decls)

(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

(define lookup-class                    
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        super-name))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))


(define read-eval-print 
  (sllgen:make-rep-loop  "-->" eval-program
                         (sllgen:make-stream-parser
                                  the-lexical-spec 
                                  the-grammar)))

(scan&parse "
class c_1 extends object
field a
field b
method initialize () 0
method setup (x, y)
begin
set a=x;
set b=+(y,2);
-(y,x)
end
method m1 () send self m2 (+(a,b))
method m2 (n) +(n, -(b,a))

class c_2 extends c_1
field b
field c
method setup (x, y)
begin
set b=x;
set c=super setup(y, *(b,2));
super m1()
end
method m2 (n) +(n,*(a, -(b,c)))
method m3 (n) +(b, super m2(+(c,n)))

class c_3 extends c_2
field a
method setup(x,y)
begin
set a=super setup(y,x);
*(x,y)
end
method m2 (n) super m3(n)
method m3 (n) +(n, -(c,b))
method m4 (n) super m2(+(n, +(b, 2)))

let p=proc (o)
         let r_1 = send o setup(1,3)
         in let r_2 = send o m2(+(r_1 ,1))
                r_3 = send o m1()
            in +(r_1 , +(r_2 ,r_3 ))
o_1 = new c_1()
o_2 = new c_2()
o_3 = new c_3()
in let x= (p o_1)
y= (p o_2)
z= (p o_3)
in send o_3 m4(+(x, +(y,z )))
")

(scan&parse "class Pila extends object
field lista
field i

method initialize () set lista = list()

method size()
begin
   i
end

method push(x)
begin
   set lista=cons(x,lista);
   set i = add1(i)
end

method pop()
begin
   set lista=cdr(lista);
   set i=sub1(i)
   car(lista)
end
  

method getLista() lista


let
   pila = new Pila()
in
   let
      b = send pila push(2)
      c = send pila push(3)
   in
      begin
         %send pila pop();
         send pila size()
      end

")

(read-eval-print)

;;Ejemplos 
;; class c1 extends object  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  let o1 = new c1() in send o1 m1()


;;;; class c1 extends object  field x field y  method initialize()  begin set x = 1; set y = 2 end method m1() x method m2() y  class c2 extends c1  field x field y  method initialize()  begin set x = 2; set y = 3 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()


;;;; class c1 extends object  field x field y  method initialize()  begin   set x = 1; set y = 2 end method m1() x method m2() y  class c2 extends c1  field x field y  method initialize()  begin   super initialize(); set  x = 2; set y = 3 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()

;;class c1 extends object  field x field y  method initialize()  begin   set x = 1; set y = 2 end method m1() x method m2() send self m1()  class c2 extends c1  field x field y  method initialize()  begin   super initialize(); set  x = 9; set y = 10 end method m1() x  let o1 = new c1() o2 = new c2() in send o2 m2()

