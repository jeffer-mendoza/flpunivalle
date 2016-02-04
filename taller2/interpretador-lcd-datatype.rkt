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
 

  

