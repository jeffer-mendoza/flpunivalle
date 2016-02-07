; Integrantes:
;  Andrés Felipe Valencia Rivera      1523227
;  Jefferson Estiven Mendoza Hoyos    1410233
;***********************************************
;
; Parte 2.
; Definiendo LCD

#lang eopl
;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales y ligadura local

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <chip>
;;               ::= <chip-prim>

;;                   <prim-chip (exp)>
;;
;;               ::= chip(--> {( port ) } ∗)
;;                       (<−− {( port ) } ∗)
;;                   <circuito>

;;                   <comp-chip (ipl,opl,circ)>
;;                      
;;                      
;;  <circuito>    ::= circ-simple ({(cable)}*)
;;                                ({(cable)}*)
;;                                 <chip>
;;                    <simple-circuit(icl,ocl,chip)>
;;
;;                ::= circ-comp <circuito> {<circuito>}+
;;                              input {cable}*
;;                              output {cable}*
;;                                  <chip>)
;;                    <complex-circuit(circ,lcircs,icl,ocl)>


;;   <chip-prim>   := chor
;;                   chip-or()
;;                 := ch-not
;;                   chip-not()
;;                 := ch-and
;;                   chip-and()
;;                 := ch-xor
;;                   chip-xor()
;;                 := ch-nand
;;                   chip-nand()
;;                 := ch-nor
;;                   chip-nor()
;;                 := ch-xnor
;;                   chip-xnor()


;;=============================
;; BEGIN defición de datatypes
;;=============================
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
;;=============================
;; END defición de datatypes
;;=============================

;;**********************************************************
;;************* 2. punto, creación de los tipos ************
;;************* abstractos de las figuras       ************
;;**********************************************************

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





;;******************************************************
;;************              PUNTO 3             ********
;;************ IMPLEMENTACIÓN DE LAS INTERFACES ********
;;******************************************************


;===========================================================
; BEGIN CREAR-CHIP
;
; INPUT:
;        cir: <circuito> 
;        in:  (list-of symbol)
;        out: (list-of symbol)
;
; RETURN:  <chip>
;
;Proposito: Recibe un circuito, una lista de identificadores
;de los puertos de entrada y una lista de identificadores de
;los puertos de salida y produce la representacion del chip.
;===========================================================
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
;===========================================================
; END CREAR-CHIP
;===========================================================

;===========================================================
; BEGIN CREAR-SHIP-PRIM
;
; INPUT:
;        x : or || and || not || xor || nand || nor || xnor
;
; RETURN:  <chip-prim>
;
;Recibe un átomo: ‘or, ‘and, ‘not, ‘xor, ‘nand, ‘nor, ‘xnor
;devuelve la representacion del respectivo chip.
;Se usa crear chip prim(x) donde x es un átomo.
;============================================================
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
;=============================================================
; END CREAR-SHIP-PRIM
;=============================================================

;==================================
; BEGIN CREAR-CIRCUITO
;
; INPUT:
;        lcxs: {<circuito>}*
;        in:  (list-of symbol)
;        out: (list-of symbol)
;
; RETURN:  <circuito>
;
; Proposito: Retorna el circuito que es creado leyendo
; el parametro de las listas y las entradas y las
; salidas.
;==================================
(define crear-circuito
  (lambda (lcxs icl ocl)
    (complex-circuit;;inicia el complex-circuit
         (simple-circuit
             icl
             (caddr(car lcxs));;obtiene la salida(tercer elemento)
             (car(car lcxs));;obtiene el chip(primer elemento)
             )
         (list (crear-circuito-aux (cdr lcxs)));;creacion de la lista de circuitos
         icl
         ocl
         );;finaliza el complex-circuit
    )
  )

(define crear-circuito-aux
  (lambda (lcxs)
    (if (pair? (cdr lcxs))
      
    (cons  
             (simple-circuit
               (cadr(car lcxs));;la lista de entradas(segundo elemento)
               (caddr(car lcxs));;obtiene la salida(tercer elemento)
               (car(car lcxs));;obtiene el chip
              )
             (crear-circuito-aux (cdr lcxs));; resto de la lista y la lista de salidas de este chip
     )
     (simple-circuit
               (cadr(car lcxs));;la lista de entradas
               (caddr(car lcxs));;obtiene la salida(tercer elemento)
               (car(car lcxs));;obtiene el chip
              )
   ))
)



;;************* PRUEBAS **********************

;==========================
; END CREAR-CIRCUITO
;==========================

(define lcxs
(list
  (list
    ;;primer chip
    (comp-chip
      '(INA INB INC IND)
      '(OUTE OUTF)
       (complex-circuit
            (simple-circuit '( a b ) '( e ) (prim-chip (chip-and)))
            (list (simple-circuit '( c d ) '( f ) (prim-chip (chip-and))))
            '( a b c d )
            '( e f )
       ))
  '(m n o p )
  '( e f ))
  
  ;;segundo chip
  
  (list
     (comp-chip
         '( INE INF )
         '(OUTA)
          (simple-circuit '(a f) '(g) (prim-chip (chip-or)))
     )
     '(e f)
     '( z )
    )
)
)

;;definen las entradas
(define icl '(m n o p ))
(define ocl '( z ) )

;Ahoraa crear el circuito
(crear-circuito lcxs icl ocl)
;Debe producir :
;
;#(struct:complex-circuit
;  #(struct:simple-circuit
;    (m n o p)
;    (e f)
;    #(struct:comp-chip
;      (INA INB INC IND)
;      (OUTE OUTF)
;      #(struct:complex-circuit
;        #(struct:simple-circuit (a b) (e) #(struct:prim-chip #(struct:chip-and)))
;        (#(struct:simple-circuit (c d) (f) #(struct:prim-chip #(struct:chip-and))))
;        (a b c d)
;        (e f))))
;  (#(struct:simple-circuit
;     (e f)
;     (z)
;     #(struct:comp-chip (INE INF) (OUTA) #(struct:simple-circuit (a f) (g) #(struct:prim-chip #(struct:chip-or))))))
;  (m n o p)
;  (z))


;==================================
; BEGIN LISTA-CONEXIONES
;
; INPUT:
;        circ-val: <circuito>
;
; RETURN:  <circuito>
;
;Proposito: Recibe un valor en CircVal y devuelve la
;lista de conexiones asociada. Para el caso del
;circuito anterior retorna
;==================================
(define lista-conexiones
  (lambda (circ-val)
     (if (circuito? circ-val)
      (cases circuito circ-val

      (simple-circuit (icl ocl chip)
                (lista-conexion circ-val))
      (complex-circuit (circ lcirs icl ocl)
               (list icl ocl
                     (lista-conexion circ-val);;resolver lista de simple-circuit
                     )
                       )
      )
      (lista-conexion circ-val)
      )
  ))
(define lista-conexion
  (lambda (circ-val)
    (if (circuito? circ-val)
    (cases circuito circ-val

      (simple-circuit (icl ocl chip)
                (list icl
                      ocl
                      (lista-conexion chip);;resolver chip
                      ))
      (complex-circuit (circ lcirs icl ocl)
               (list (lista-conexion circ);resolver simple-circuit
                     (aux-lista-conexion lcirs);;resolver lista de simple-circuit
                     )
                       )
      )
    (cases chip circ-val
      (prim-chip (p-chip) (list empty))
  
      (comp-chip (ipl opl circ)
                 (lista-conexion circ))))
    ))

(define aux-lista-conexion
  (lambda (list)
    (if (pair? (cdr list))
        (list (lista-conexion (car list))
              (aux-lista-conexion (cdr list))
        )
        (lista-conexion (car list))
    )
  ))

;;************* PRUEBAS **********************
(lista-conexiones (crear-circuito lcxs icl ocl))
;valor esperado ((m n o p) (z) (((m n o p) (e f) (((a b) (e) (())) ((c d) (f) (())))) ((e f) (z) ((a f) (g) (())))))
;==========================
; END LISTA-CONEXIONES
;==========================
