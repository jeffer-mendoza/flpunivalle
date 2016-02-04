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





;;***************
;;*** PUNTO 3 ***
;;***************


;==================================
; BEGIN CREAR-CHIP
;
;Recibe un circuito, una lista de identificadores de los puertos de entrada y una lista de
;identificadores de los puertos de salida y produce la representaci ́on del chip correspondiente a unir
;los cables de entrada y de salida del circuito, con los puertos de entrada y de salida se˜nalados.
;==================================
(define crear-chip
  (lambda (cir in out)
    (comp-chip
      in
      out
      cir
    )))

;;************* PRUEBAS **********************
(crear-chip
 (simple-circuit '(a b) '(c)  (prim-chip (chip-or)))
 '(INA INB)
 '(OUTC)
 )
;==========================
; END CREAR-CHIP
;==========================

;==================================
; BEGIN CREAR-SHIP-PRIM
;
;Recibe un  ́atomo: ‘or, ‘and, ‘not, ‘xor, ‘nand, ‘nor, ‘xnor devuelve la representaci ́on
;del respectivo chip. Se usa crear chip prim(x) donde x es un ´atomo.
;==================================
(define crear-chip-prim
  (lambda (x)
    (cond
      ((eq? x "or") (prim-chip (chip-or)))
       ((eq? x "not") (prim-chip (chip-not)))
        ((eq? x "and") (prim-chip (chip-and)))
         ((eq? x "xor") (prim-chip (chip-xor)))
          ((eq? x "nand") (prim-chip (chip-nand)))
           ((eq? x "nor") (prim-chip (chip-nor)))
             ((eq? x "xnor") (prim-chip (chip-xnor)))
      ('error)
    )))

;;************* PRUEBAS **********************
(crear-chip-prim "or")
(crear-chip-prim "not")
(crear-chip-prim "and")
(crear-chip-prim "xor")
(crear-chip-prim "nand")
(crear-chip-prim "nor")
(crear-chip-prim "xnor")
(crear-chip-prim "suma")
;==========================
; END CREAR-SHIP-PRIM
;==========================