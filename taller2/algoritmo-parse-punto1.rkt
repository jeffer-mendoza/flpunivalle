
;;*************************************************************
;; función parse para leer expressiones con el siguiente flujo:
;; sitaxis concreta >> just-scan(sintaxis concreta) >> parse(just-scan(sintaxis concreta))
;; se obtine el arbol de sintaxis abstracta de las expresiones
;;*************************************************************

(define scan
  (lambda (lista)
    (if (and (number? (cadr (car lista))) (equal? (cadr (cadr lista)) ")" ))
        ;(lit-exp (cadr (car list)))
        (lit-exp 6)
    (cond
    ((equal? (cadr (car lista)) "(" ) (scan (cdr lista)))
    ((equal? (cadr (car lista)) ")" ) (scan (cdr lista)))
    ((equal? (cadr (car lista)) "+") (primapp-exp (add-prim)  (scan (cdr lista))))
    ((number? (cadr (car lista)))  (list (lit-exp (cadr (car lista))) (scan (cdr lista))))
    (else 3)
    ))))

(scan (just-scan "(+ 1 2)"))
(scan (just-scan "(+ 1 (+ 1 2))"))
(scan (just-scan "(+ 1 (+ 1 2))"))