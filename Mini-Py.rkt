#lang eopl

;; Gramatica

;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;
;;  <expresion>    ::= <numero>  
;;                     <num-exp (number)>
;;                 ::= <identificador>  
;;                     <id-exp (id)>
;;                 ::= <hexadecimal>  
;;                     <hexa-exp (base hexa-number)>


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

;;************************************************************************************************************

;;Especificación Léxica

(define scanner
'((white-sp (whitespace) skip)
  (comment ("#" (arbno (not #\newline))) skip)
  (identifier (letter (arbno (or letter digit "?"))) symbol)
  (number (digit (arbno digit) "." digit (arbno digit)) number)
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (string ("\"" (arbno (not #\")) "\"") string)))

;;Especificación Sintáctica (gramática)

(define grammar
  '((program (expresion) a-program)
    (expresion (number) num-exp)
    (expresion (identifier) id-exp)
    (expresion ("(" number (arbno number)")") hexa-exp)
    (expresion (string) text-exp)
    ))

;;************************************************************************************************************

;;Tipos de datos para la sintaxis abstracta de la gramática

(sllgen:make-define-datatypes scanner grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner grammar)))

;; -- Parser, Scanner, Interfaz --

;;Análisis léxico (scanner) y sintáctico (parser) integrados

(define scan&parse
  (sllgen:make-string-parser scanner grammar))

;;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner grammar))

;;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner
      grammar)))

;;************************************************************************************************************

;;El Interprete

;;eval-program: <programa> -> numero
;;función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expresion body (init-env))))))

;;Ambiente inicial

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))


;;eval-expression: <expresion> <enviroment> -> numero
;;evalua la expresión en el ambiente de entrada

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp (numero) numero)
      (id-exp (id) (apply-env env id))
      (hexa-exp (base hexa-number) (base-hexa->numero base hexa-number))
      (text-exp (cadena) (substring cadena 1 (- (string-length cadena) 1))) ;; Esto aun esta en veremos
      )))

;;************************************************************************************************************

;; -- Ambientes --

;;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

;;empty-env:  -> enviroment
;;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record))) ;; llamado al constructor de ambiente vacío 

;;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;;función que busca un símbolo en un ambiente y retorna el valor almacenado en la referencia
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

;;función que busca un símbolo en un ambiente y retorna la referencia
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

;;************************************************************************************************************

;; -- Funciones Auxiliares --

;;funciones auxiliares para encontrar la posición de un símbolo
;;en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

;;funciones auxiliares para cambiar un numero en base hexa-decimal
;;con la representacion bignum a un numero en base 10

(define base-hexa->numero
  (lambda(base numeros)
    (let ((n (length numeros)))
      (base-hexa->numero-aux base numeros n 0))
    ))

(define base-hexa->numero-aux
  (lambda (base numeros n-lim n)
    (if (= n n-lim)
        0
        (if (null? numeros)
            0
            (+(* (car numeros) (expt base n))
              (base-hexa->numero-aux base (cdr numeros) n-lim (+ n 1)))
        ))
    ))

;; (16 1 2) --> 33
;; (16 2 0 1) --> 258

;;************************************************************************************************************

;; -- Referencias --

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))