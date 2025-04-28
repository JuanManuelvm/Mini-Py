#lang eopl

;; Gramatica

;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;
;;  <expresion>    ::= <numero>  --(number)--
;;                     <num-exp (number)>
;;                 ::= <identificador>  --(identifier)--
;;                     <text-exp (id)>
;;                 ::= var {<identificador> = <expresion>}*(,) in <expresion> --(let)--
;;                     <var-exp (ids rands body)>
;;                 ::= const {<identificador> = <expresion>}*(,) in <expresion> --(let)--
;;                     <constante-exp (ids rands body)>
;;                 ::= rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion> --(letrec)--
;;                     <rec-exp (proc-names ids bodies bodyrec)>
;;                 ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= if <expr-bool> then <expresion> [ else <expression> ] end
;;                      <if-exp (exp-bool exp2 exp3)>

;;                 ::= <cadena>  
;;                 ::= <bool> 
;;
;;                 ::= <lista>
;;                 ::= <tupla>
;;                 ::= <registro>
;;                 ::= <expr-bool>
;;                 ::= <circuit>
;;                 ::= while <expr-bool> do <expresion> done
;;                 ::= for <identificador> in <expresion> do <expresion> done
;;
;; <lista>         ::= [{<expresion>}∗(;)]
;; <tupla>         ::= tupla[{<expresion>}∗(;)]
;; <registro>      ::= {{<identificador> = <expresion>}+(;)}
;; <expr-bool>     ::= <pred-prim>(<expresion> , <expresion>)
;;                 ::= <oper-bin-bool>(<expr-bool> , <expr-bool>)
;;                 ::= <bool>
;;                 ::= <oper-un-bool >(<expr-bool >)
;; <pred-prim>     ::= < | > | <= | >= | == | <>
;; <oper-bin-bool> ::= and | or
;; <oper-un-bool>  ::= not
;;  <primitive> = UN POCO DE PRIMITIVAS

;; Especificación Léxica

(define scanner
'((white-sp (whitespace) skip)
  (comment ("#" (arbno (not #\newline))) skip)
  (identifier (letter (arbno (or letter digit "?"))) symbol)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)))

;; Especificación Sintáctica (gramática)

(define grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    ))

;; Tipos de datos para la sintaxis abstracta de la gramática

(sllgen:make-define-datatypes scanner grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner grammar)))

;; Parser, Scanner, Interfaz

;; El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner grammar))

;; El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner grammar))

;; El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner
      grammar)))

;;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) id)
      )))

(define init-env 'nada)



