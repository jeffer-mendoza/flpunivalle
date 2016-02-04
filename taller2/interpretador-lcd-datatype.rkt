#lang eopl
(define-datatype chip chip?
  (prim-chip
   (p-chip chip-prim?))
  (comp-chip
   (ipl (list-of symbol?))
   (opl (list-of symbol?))
   (circ circuito?))
   )

(define-datatype circuito circuito?
  (simple-circuit
   (icl (list-of symbol?))
   (ocl (list-of symbol?))
   (chip chip?))
  (complex-circuit
   (circ circuito?)
   (lcircs (list-of circuito?))
   (icl (list-of symbol?))
   (ocl (list-of symbol?)))
   )

(define-datatype chip-prim chip-prim?
  (chip-or)
  (chip-not)
  (chip-and)
  (chip-xor)
  (chip-nand)
  (chip-nor)
  (chip-xnor))

;=============================================
; BEGIN ejemplo del taller
;
;Nota:
;la lista de los circuitos debe crearse con
;"list" no con "'" y la creación del tipo
;chip-prim en los simple-circuit debe hacerse
;con la variante prim-chip
;=============================================
(define circuit-example
(comp-chip
  '(INA INB INC IND)
  '(OUTA)
   (complex-circuit
    (simple-circuit '( a b ) '( e )  (prim-chip (chip-and)))
      (list
           (simple-circuit '( c d ) '( f )  (prim-chip (chip-and)))
           (simple-circuit '( e f ) '( g )  (prim-chip (chip-or)))
      )
      '( a b c d )
      '( g )
   )
))
;==========================
; END ejemplo del taller
;==========================

;==========================
; BEGIN SUMADOR

;;
;; in PA PB
;; out PS PC
;; complex-cir (a b , d, or)
;;             list (a b , c, and)
;;                  (c , e, not)
;;                  (d e, s, and)
;;             a b
;              s c
;==========================
(comp-chip
 '(PA PB)
 '(PS PC)
 (complex-circuit
   (simple-circuit '(a b) '(d) (prim-chip (chip-or)))
   (list
        (simple-circuit '(a b) '(c) (prim-chip (chip-and)))
        (simple-circuit '(c) '(e) (prim-chip (chip-not)))
        (simple-circuit '(d e) '(s) (prim-chip (chip-and)))
   )
   '(a b)
   '(s c)
))
;==========================
; END SUMADOR
;==========================

;==================================
; BEGIN SUMADOR COMPLETO

;;
;; in PA PB PC
;; out PSUM PCout
;; complex-cir (half-adder b cin)
;;             list (half-adder (a e))
;;                  (d f, cout, or)
;;             a b cin
;              sum cout
;==================================
(comp-chip
 '(PA PB)
 '(PS PC)
 (complex-circuit
   (half-adder 'b 'cin)
   (list
        (half-adder 'a 'e)
        (simple-circuit '(d f) '(cout) (prim-chip (chip-or)))
   )
   '(a b cin)
   '(sum cout)
))
;==========================
; END SUMADOR COMPLETO
;==========================

;==================================
; BEGIN SUMADOR AUXILIAR
;
; Función que hace uso del sumador
; para construir un chip
;==================================
(define half-adder
  (lambda (a b)
    (simple-circuit
   '(a b)
   '(s c)
    (comp-chip
 '(PA PB)
 '(PS PC)
 (complex-circuit
   (simple-circuit '(a b) '(d) (prim-chip (chip-or)))
   (list
        (simple-circuit '(a b) '(c) (prim-chip (chip-and)))
        (simple-circuit '(c) '(e) (prim-chip (chip-not)))
        (simple-circuit '(d e) '(s) (prim-chip (chip-and)))
   )
   '(a b)
   '(s c)
))
    )))

;==========================
; END SUMADOR AUXILIAR
;==========================