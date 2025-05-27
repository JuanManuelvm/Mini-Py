#lang eopl

;;************************************************************************************************************
;;
;;  _______  ______    _______  __   __  _______  _______  _______  _______   
;; |       ||    _ |  |       ||  | |  ||       ||       ||       ||       |  
;; |    _  ||   | ||  |   _   ||  |_|  ||    ___||       ||_     _||   _   |  
;; |   |_| ||   |_||_ |  | |  ||       ||   |___ |       |  |   |  |  | |  |  
;; |    ___||    __  ||  |_|  ||_     _||    ___||      _|  |   |  |  |_|  |  
;; |   |    |   |  | ||       |  |   |  |   |___ |     |_   |   |  |       |  
;; |___|    |___|  |_||_______|  |___|  |_______||_______|  |___|  |_______|  
;;  __   __  ___   __    _  ___          _______  __   __                     
;; |  |_|  ||   | |  |  | ||   |        |       ||  | |  |                    
;; |       ||   | |   |_| ||   |  ____  |    _  ||  |_|  |                    
;; |       ||   | |       ||   | |____| |   |_| ||       |                    
;; |       ||   | |  _    ||   |        |    ___||_     _|                    
;; | ||_|| ||   | | | |   ||   |        |   |      |   |                      
;; |_|   |_||___| |_|  |__||___|        |___|      |___|                      
;;
;;
;;************************************************************************************************************
;;
;; Integrantes:
;;
;; Fernando Cardona Giraldo - 2241381
;; Juan Manuel Vargas - 
;; Pablo Esteban Becerra - 2243506
;;
;;************************************************************************************************************
;;
;;
;; -- Gramatica -- 
;;
;;
;; <program>      ::= <expression>
;;                    <a-program (exp)>
;;
;; <expresion>    ::= <numero>  
;;                    <num-exp (number)>
;;                ::= <identificador>  
;;                    <id-exp (id)>
;;                ::= (<hexadecimal>)  
;;                    <hexa-exp (base hexa-number)>
;;                ::= num->hexa(<numero>)  
;;                    <covertir-num-a-hexa (num)>
;;                ::= <cadena>  
;;                    <text-exp (cadena-texto)>
;;                ::= <bool>
;;                    <bool-exp (bool)>
;;                ::= <primitive> ({<expression>}*(,))
;;                    <primapp-exp (prim rands)>
;;                ::= <expr-bool> 
;;                    <bool-app-exp (expresionbooleana)>
;;                ::= <lista>
;;                    <lista-exp (lista-expresion)>
;;                ::= <prim-tupla>[{<expresion>}∗(;)]
;;                    <tupla-exp (prim exps)>
;;                ::= var {<identificador> = <expresion>}*(,) in <expresion> 
;;                    <var-exp (ids rands body)>
;;                ::= const {<identificador> = <expresion>}*(,) in <expresion> 
;;                    <constante-exp (ids rands body)>
;;                ::= rec {<identificador> ({<identificador>}(,)) = <expresion>} in <expresion> --(letrec)--
;;                    <rec-exp (proc-names ids bodies bodyrec)>
;;                ::= if <expr-bool> then <expresion> [ else <expression> ] end
;;                    <if-exp (exp-bool true-exp false-exp)>
;;                ::= proc({<identificador>}*(,)) <expresion>
;;                    <proc-exp (ids body)>
;;                ::= aplicar(<expresion> {<expresion>}*)
;;                    <app-exp proc rands>
;;                ::= begin <expression> {; <expression>}* end
;;                    <begin-exp (exp exps)>
;;                ::= set <identificador> = <expression>
;;                    <set-exp (exp exps)>
;;                ::= while <expr-bool> do <expresion> done
;;                    <while-exp (expr-bool expresion)>
;;                ::= for <identificador> in <expresion> do <expresion> done
;;                    <for-exp (ids expresion body)>
;;                ::= print (<expresion>)
;;                    <print-exp (expresion)>
;;                ::= <circuit>
;;                    <circuit-exp (circuito)>
;;
;;                 ::= <registro> // Falta
;;
;; <primitive>    ::= + | - | * | / | % | add1 | sub1
;;                ::= len | concat
;;                ::= eval-circuit | merge-circuit | connect-circuits
;;
;; <expr-bool>    ::= <pred-prim>(<expresion> , <expresion>)
;;                     <expr-bool-exps (prim exp1 exp2)>
;;                ::= <oper-bin-bool>(<expr-bool> , <expr-bool>)
;;                     <expr-bool-bools (prim expbool expbool)>
;;                ::= <oper-un-bool>(<expr-bool>)
;;                     <expr-bool-unbool (prim expbool)>
;;                ::= expr <bool>
;;                     <exp-bool-booleano (bool)>
;;
;; <pred-prim>     ::= < | > | <= | >= | == | !=
;; <oper-bin-bool> ::= and | or
;; <oper-un-bool>  ::= not
;; <bool>          ::= True | False
;;
;; <lista>         ::= <prim-lista> [{<expresion>}∗(;)]
;;                     <expr-lista-normal (prim exprs)>
;;                 ::= set-list [<lista-a-modificar> ; <expresion>; <expresion>]
;;                     <expr-lista-set (prim exprs)>
;;
;; <lista-a-modificar> ::= id <identificador>
;;                     ::= lista- <lista>
;;
;; <prim-lista>    ::= crear-lista | crear-null | cabeza | cola | null? | lista? | append | ref-list  
;; <prim-tupla>    ::= crear-tupla | crear-null-tupla | cabeza-tupla | cola-tupla
;;                     | null-tupla? | tupla? | ref-tupla
;;
;; <registro>      ::= {{<identificador> = <expresion>}+(;)} // FALTA
;; <primitive> = UN POCO DE PRIMITIVAS
;;
;; <circuit>       ::= ( circuit ( <gatelist> ) )
;;                      <a-circuit (gatelist)>
;;
;; <gatelist>      ::= gate_list (<gate>)*
;;                      <a-gate-list (gates)>
;;
;; <gate>          ::= gate identifier ( type <type> ) ( input_list <input_list> )
;;                      <a-gate (id type input_list)>
;;
;; <type>          ::= and | or | not | xor
;;
;; <input_list>    ::= empty
;;                      <input-list-empty>
;;                 ::= <bool> <input_list>
;;                      <input-list-bool (bool resto)>
;;                 ::= identifier <input_list>
;;                      <input-list (id resto)>
;;
;; <bool>          ::= True | False

;;************************************

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
    (expresion ("(" number (arbno number)")") hexa-exp) ;; Representacion bignum
    (expresion ("num->hexa" "(" number ")") convertir-num-a-hexa)
    (expresion (string) text-exp)
    (expresion (bool) bool-exp)
    (expresion (primitive "(" (separated-list expresion ",")")") primapp-exp) ;; Base de interpretador del curso
    (expresion (expr-bool) bool-app-exp)
    (expresion (expr-lista) lista-exp)
    (expresion (prim-tupla "[" (separated-list expresion ";") "]") tupla-exp)
    (expresion (expr-registro) registro-exp)
    (expresion ("circuito" circuit) circuit-exp) ; Circuito como expresion para procesar un circuito
    (expresion (type) type-exp) ; Tipo como expresion para ser procesado correctamente en primapp-exp al aplicar la primitiva merge-circuit
    (expresion ("'" identifier) var-exp-connect) ; 'id como expresion para poder utilizarlo en connect-circuits
    
    (expresion ("var" (arbno identifier "=" expresion ",") "in" expresion) let-exp)
    (expresion ("const" (arbno identifier "=" expresion ",") "in" expresion) const-exp)
    (expresion ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expresion)  "in" expresion) rec-exp)
    (expresion ("if" expr-bool "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("proc" "(" (separated-list identifier ",") ")" expresion) proc-exp)
    (expresion ("aplicar" "(" expresion (arbno expresion) ")") app-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identifier "=" expresion) set-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("for" identifier "in" expresion "do" expresion "done") for-exp)
    (expresion ("print" "(" expresion ")") print-exp)
    
    (primitive ("+") add-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("-") substract-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("*") mult-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("/") div-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("%") residuo-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("add1") incr-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("sub1") decr-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("sub1") decr-prim-aritmetica) ;; Base de interpretador del curso
    (primitive ("len") longitud-prim-text) ;; Base de python
    (primitive ("concat") concatenar-prim-text) ;; Base de java
    (primitive ("eval-circuit") eval-circuit-primitive) ; Nueva primitiva
    (primitive ("merge-circuit") merge-circuit-primitive) ; Nueva primitiva
    (primitive ("connect-circuits") connect-circuits-primitive) ; Nueva primitiva
    
    ;; -- Gramatica Booleanos --
    
    (bool ("True") bool-true)
    (bool ("False") bool-false)
    (expr-bool (pred-prim "(" expresion "," expresion ")") expr-bool-exps)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") expr-bool-bools)
    (expr-bool (oper-un-bool "(" expr-bool ")") expr-bool-unbool)
    (expr-bool ("expr" bool) expr-bool-boleano)
    (pred-prim ("<") menor-que)
    (pred-prim (">") mayor-que)
    (pred-prim ("<=") menor-igual-que)
    (pred-prim (">=") mayor-igual-que)
    (pred-prim ("==") igual-que)
    (pred-prim ("!=") diferente-que)
    (oper-bin-bool ("and-bool") and-bool)
    (oper-bin-bool ("or-bool") or-bool)
    (oper-un-bool ("not-bool") not-bool)

    ;; -- Gramatica para Listas --
    
    (expr-lista (prim-lista "[" (separated-list expresion ";") "]") expr-lista-normal)
    (expr-lista ("set-list" "[" lista-a-modificar ";" expresion ";" expresion "]") expr-lista-set) 
    (lista-a-modificar ("id" identifier) lista-a-modificar-id)
    (lista-a-modificar ("lista-" expr-lista) lista-a-modificar-lista)
    (prim-lista ("crear-lista") prim-crear-lista)
    (prim-lista ("cabeza") prim-cabeza-lista)
    (prim-lista ("null?") prim-pregunta-vacioLista) 
    (prim-lista ("cola") prim-cola-lista)
    (prim-lista ("crear-null") prim-crear-vacioLista)
    (prim-lista ("lista?") prim-pregunta-lista) 
    (prim-lista ("append") prim-adicionar-lista)
    (prim-lista ("ref-list") prim-ref-lista)

    ;; -- Gramatica para Tuplas --

    (prim-tupla ("crear-tupla") prim-crear-tupla)
    (prim-tupla ("cabeza-tupla") prim-cabeza-tupla)
    (prim-tupla ("null-tupla?") prim-pregunta-vacioTupla)
    (prim-tupla ("crear-null-tupla") prim-crear-vacioTupla)
    (prim-tupla ("tupla?") prim-pregunta-tupla)
    (prim-tupla ("cola-tupla") prim-cola-tupla)
    (prim-tupla ("ref-tupla") prim-ref-tupla)

    ;  -- Gramatica de los circuitos --
    
    (circuit ("(" "circuit" "(" gatelist ")" ")") a-circuit)
    (gatelist ("gate_list" (arbno  "(" gate ")")) a-gate-list)
    (gate ("gate" identifier "(" "type " type ")" "(" "input_list" input_list ")") a-gate)  
    (type ("and") and-type)
    (type ("or") or-type)
    (type ("not") not-type)
    (type ("xor") xor-type)    
    (input_list () input-list-empty)
    (input_list (bool input_list) input-list-bool)
    (input_list (identifier input_list) input-list)


    ;; -- Gramatica para registros
    (expr-registro ("crear-registro" "{" (arbno identifier ":" expresion",") "}" ) registro-expr-create)
    (expr-registro ("registro?" "{" expresion "}" ) registro-expr-validator)
    (expr-registro ("registro-set" "{" identifier ":" identifier ":" expresion "}" ) registro-expr-set)
    
    ))

;;************************************

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

;;************************************

;;El Interprete

;;eval-program: <programa> -> numero
;;función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expresion body (init-env))))))

;;Ambiente inicial

;; (define init-env
;;   (lambda ()
;;     (extend-env
;;      '(i v x)
;;      '(1 1 10)
;;      (empty-env))))

;; (define init-env
;;   (lambda ()
;;     (extend-env
;;      '(x y z)
;;      (list (direct-target 1)
;;            (direct-target 5)
;;            (direct-target 10))
;;      (empty-env))))

(define init-env
  (lambda ()
    (empty-env)))

;;eval-expression: <expresion> <enviroment> -> numero
;;evalua la expresión en el ambiente de entrada

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp (numero) numero)
      (id-exp (id) (apply-env env id))
      (hexa-exp (base hexa-number) (base-hexa->numero base hexa-number))
      (convertir-num-a-hexa (num) (numero->base-hexa num))
      (text-exp (cadena) (substring cadena 1 (- (string-length cadena) 1)))
      (bool-exp (booleano) (eval-bool booleano))
;;       (primapp-exp (prim rands) (let ((args (eval-rands rands env)))
;;                                   (apply-primitive prim args env)))
      (primapp-exp (prim rands) (let ((args (eval-primapp-exp-rands rands env)))
                                  (apply-primitive prim args env)))
      (bool-app-exp (exprbooleana) (eval-expr-bool exprbooleana env))
      (lista-exp (expr-lista) (eval-expr-lista expr-lista env))
      (tupla-exp (prim tupla) (apply-prim-tupla prim tupla env))
;;       (let-exp (ids exps body) (let ((args (eval-rands exps env)))
;;                                  (eval-expresion body (extend-env ids args env))))
      (let-exp (ids exps body) (let ((args (eval-let-exp-rands exps env))                                     )
                                 (eval-expresion body (extend-env ids args env))
                                 ))
;;       (const-exp (ids exps body) (let ((args (eval-rands exps env)))
;;                                    (eval-expresion body (extend-const-env ids args env))))
      (const-exp (ids exps body) (let ((args (eval-let-exp-rands exps env)))
                                    (eval-expresion body (extend-const-env ids args env))))
      (rec-exp (proc-names idss bodies rec-body)
                  (eval-expresion rec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (if-exp (exp-bool true-exp false-exp) (if (eval-expr-bool exp-bool env)
                                                (eval-expresion true-exp env)
                                                (eval-expresion false-exp env)))
      (proc-exp (ids body) (closure ids body env))
      (app-exp (rator rands) (let ((proc (eval-expresion rator env))
                                   (args (eval-rands rands env)))
                               (if (procval? proc)
                                   (apply-procedure proc args)
                                   (eopl:error 'eval-expresion "Attempt to apply non-procedure ~s" proc))))
      (begin-exp (exp exps) (let loop ((acc (eval-expresion exp env))
                                       (exps exps))
                              (if (null? exps)
                                  acc
                                  (loop (eval-expresion (car exps) env)
                                        (cdr exps)))))
      (set-exp (id new-exps) (begin
                               (setref! (apply-env-ref env id) (eval-expresion new-exps env))
                               1))
      (while-exp (exp-bool exp) (let loop ()
                                  (if (eval-expr-bool exp-bool env)
                                      (begin (eval-expresion exp env)
                                             (loop))
                                      'done)))
      (for-exp (id iterable exp) (let* ((arg (eval-expresion iterable env))
                                        (vec (if (vector? arg) arg (list->vector arg)))
                                        (n (vector-length vec)))
                                   (define (loop i)
                                     (if (>= i n)
                                         'done
                                         (let* ((elem (direct-target (eval-expresion (vector-ref vec i) env)))
                                                (nuevo-env (extend-env (list id) (list elem) env)))
                                           (eval-expresion exp nuevo-env)
                                           (loop (+ i 1)))))
                                   (loop 0)))
      (registro-exp (val)
                  (eval-expr-registro val env))
      (print-exp (exp) (let ((resultado (eval-expresion exp env)))
                         (display resultado)
                         resultado))
      (circuit-exp (circuito) (eval-circuit circuito env)) ; evaluacion de circuit-exp
      (type-exp (type) type) ; evaluacion de type-exp
      (var-exp-connect (id) id) ; evaluacion de var-exp-connect ('id)
      )))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expresion x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env)) rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expresion rand env))))

;;apply-primitive: <primitiva> <list-of-expression> -> numero | text 
;;función que aplica la primitiva dada

(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim
      (add-prim-aritmetica () (+ (car args) (cadr args)))
      (substract-prim-aritmetica () (- (car args) (cadr args)))
      (mult-prim-aritmetica () (* (car args) (cadr args)))
      (div-prim-aritmetica () (/ (car args) (cadr args)))
      (residuo-prim-aritmetica () (remainder (car args) (cadr args)))
      (incr-prim-aritmetica () (+ (car args) 1))
      (decr-prim-aritmetica () (- (car args) 1))
      (longitud-prim-text () (string-length (car args)))
      (concatenar-prim-text () (string-append (car args) (cadr args)))
;;    -- Ciruitos --
      (eval-circuit-primitive () (guardar-gates (eliminar_parentesis args) env 'noHayValorAun)) ;; Procesar la primitiva eval-circuit
      (merge-circuit-primitive () (evaluar-merge args env)) ;; Procesar la primitiva merge-circuit
      (connect-circuits-primitive () (connect-circuits (eliminar_parentesis args) env)) ;; Procesar la primitiva connect-circuits-primitive
      )))

;;************************************

;; -- Ambientes --

;;definición del tipo de dato ambiente  
;;Puede ser (ambientevacio) | (ambienteextendido (a b c) (2 4 6) (#t #t #t) (ambienteviejo))
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (mutable vector?) ;;Vector que almacena si las variables guardadas son mutables
   (env environment?))
  )

;;empty-env: -> enviroment
;;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record))) ;; llamado al constructor de ambiente vacío 

;;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;;función que crea un ambiente extendido con variables mutables
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) (make-vector (length syms) #t) env)))

;;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;;función que crea un ambiente extendido con variables no mutables
(define extend-const-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) (make-vector (length syms) #f) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec (make-vector (length idss) #t) old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))


;;función que busca un símbolo en un ambiente y retorna el valor almacenado en la referencia
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

;;función que busca un símbolo en un ambiente y retorna la referencia
;;acaaaaaaaa para que me retorne la ref 
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals sonmutables env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals (vector-ref sonmutables pos))
                                 (apply-env-ref env sym))))
      )))

;;funcion para determinar si una variable es mutable
;;recibe la referencia a dicha variable
(define ismutable?
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mut) mut)
      )))

;;************************************

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

;;funcion auxiliar para cambiar un numero en base 10
;;a un numero en base hexa-decimal con la representacion bignum

(define numero->base-hexa
  (lambda (num)
    (cons 16 (let rec ((n num))
               (if (< n 16)
                   (list n)
                   (let ((conciente (quotient n 16))
                         (resto (remainder n 16)))
                     (cons resto (rec conciente))))))))

;; num->hexa(258) --> (16 2 0 1)
;; num->hexa(33) --> (16 1 2)

;;funciones auxiliares para aplicar eval-expresion a cada elemento de una 
;;lista de operandos (expresiones)

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;; (define eval-rand
;;   (lambda (rand env)
;;     (eval-expresion rand env)))

(define eval-rand
  (lambda (rand env)
    (cases expresion rand
;;         (id-exp (id) (indirect-target
;;                        (let ((ref (apply-env-ref env id)))
;;                          (cases target (primitive-deref ref)
;;                            (direct-target (expval) ref)
;;                            (indirect-target (ref1) ref1)))))
      (id-exp (id) (let ((ref (apply-env-ref env id)))
                      (cases target (primitive-deref ref)
                        (direct-target (expval) (if (vector? expval)
                                                    (indirect-target ref)
                                                    (direct-target expval)))
                        (indirect-target (ref1) (indirect-target ref1)))))
      (else (direct-target (eval-expresion rand env))))))


;; -- Funciones auxiliares para los booleanos --


;;eval-bool: <bool> -> True | False
;;función auxiliar que evalúa un booleano 
;;(procesa/elimina la sintaxis abstracta de bool 
;;y retorna un valor True o False)

(define eval-bool
  (lambda (booleano)
    (cases bool booleano
      (bool-true () #t)
      (bool-false () #f))
    ))

;;eval-expr-bool: <expr-bool> <enviroment> -> True | False
;;función que evalúa una expresion booleana 
;;(procesa/elimina la sintaxis abstracta de expr-bool 
;;y retorna un valor True o False)

(define eval-expr-bool
  (lambda (exp env)
    (cases expr-bool exp
      (expr-bool-exps (prim exp1 exp2) (let((args (eval-rands2 (list exp1 exp2) env)))
                                        (apply-pred-prim prim args env)))
      (expr-bool-bools (prim exp1 exp2) (let ((args (eval-rands-bools (list exp1 exp2) env)))
                                          (apply-oper-bin-bool prim args env)))
      (expr-bool-unbool (prim exp1) (let ((arg (eval-rands-bools (list exp1) env)))
                                          (apply-oper-un-bool prim arg env)))
      (expr-bool-boleano (booleano) (eval-bool booleano))
     )))

;;eval-bool: <pred-prim> expresiones-evaluadas <enviroment> -> True | False
;;función que aplica la <pred-prim> dada 

(define apply-pred-prim
  (lambda (prim args env)
    (cases pred-prim prim
      (menor-que () (< (car args) (cadr args)))
      (mayor-que () (> (car args) (cadr args)))
      (menor-igual-que () (<= (car args) (cadr args)))
      (mayor-igual-que () (>= (car args) (cadr args)))
      (igual-que () (equal? (car args) (cadr args)))
      (diferente-que () (not (equal? (car args) (cadr args))))
      )))

;;eval-bool: <oper-bin-bool> expresiones-booleanas-evaluadas <enviroment> -> True | False
;;función que aplica la <oper-bin-bool> dada

(define apply-oper-bin-bool
  (lambda (prim args env)
    (cases oper-bin-bool prim
      (and-bool () (and (car args) (cadr args)))
      (or-bool () (or (car args) (cadr args)))
      )))

;;eval-bool: <oper-un-bool> expresion-booleana-evaluada <enviroment> -> True | False
;;función que aplica la <oper-bin-bool> dada

(define apply-oper-un-bool
  (lambda (prim args env)
    (cases oper-un-bool prim
      (not-bool () (not (car args)))
      )))

;;funciones auxiliares para aplicar eval-expr-bool a cada elemento de una 
;;lista de operandos (expresiones booleanas)

(define eval-rands-bools
  (lambda (rands env)
    (map (lambda (x) (eval-rand-bool x env)) rands)))

(define eval-rand-bool
  (lambda (rand env)
    (eval-expr-bool rand env)))


;; -- Funciones auxiliares para las listas --


;;eval-expr-lista: <expr-lista> <enviroment> -> vector | bool
;;| elemvector(Todo lo que se pueda guardar) | vacio
;;función que evalúa una expresion lista 
;;(procesa/elimina la sintaxis abstracta de expr-lista
;;para segun sea el caso)

(define eval-expr-lista
  (lambda (expr env)
    (cases expr-lista expr
      (expr-lista-normal (prim lista) (apply-prim-lista prim lista env))
      (expr-lista-set (lista pos nuevo-valor) (eval-lista-a-modificar lista pos nuevo-valor env))
      )))

;;apply-prim-lista: <primitiva> <list-of-expresion> -> vector | bool
;;| elemvector(Todo lo que se pueda guardar) | vacio
;;función que aplica la primitiva dada a un vector

(define apply-prim-lista
  (lambda (prim lista env)
    (cases prim-lista prim
      (prim-crear-vacioLista () #())
      (prim-pregunta-vacioLista () (if (= 0 (vector-length (car (eval-rands2 lista env))))
                                       (eval-bool (bool-true))
                                       (eval-bool (bool-false))))
      (prim-crear-lista () (apply vector lista))
      (prim-pregunta-lista () (let ((args (eval-rands2 lista env)))
                                (if (vector? (car args))
                                    (eval-bool (bool-true))
                                    (eval-bool (bool-false)))))
      (prim-cabeza-lista () (let* ((args (eval-rands2 lista env))
                                   (vec (car args)))
                              (eval-expresion (vector-ref vec 0) env)))
      (prim-cola-lista () (let* ((args (eval-rands2 lista env))
                                 (vec (car args)))
                            (list->vector (cdr (vector->list vec)))))

      ;;Prim para seleccionar un elemento de una lista
      (prim-ref-lista () (let* ((args (eval-rands2 lista env))
                                (vec (car args))
                                (index (cadr args)))
                           (eval-expresion (vector-ref vec index) env)))
      (prim-adicionar-lista ()
                            (let* ((args (eval-rands-exp2 (list (cadr lista)) env))
                                   (vec-ref (apply-env-ref env (obtener-id-lista (car lista)))))
                              (cases reference vec-ref
                                (a-ref (pos vec mutable?)
                                       (if mutable?
                                           (cases target (vector-ref vec pos)
                                             (direct-target (expval1) (let ((new-list (append (vector->list expval1) args)))
                                                                        (setref! vec-ref (apply vector new-list))
                                                                        vec))
                                             (indirect-target (ref1)
                                                              (cases reference ref1
                                                                (a-ref (pos2 vec2 mutable2?)
                                                                       (cases target (vector-ref vec2 pos2)
                                                                         (direct-target (expval2)
                                                                                        (if mutable2?
                                                                                            (let ((new-list (append (vector->list expval2) args)))
                                                                                              (setref! ref1 (apply vector new-list))
                                                                                              vec2)
                                                                                            (eopl:error 'prim-adicionar-lista "Cannot modify immutable list") ))
                                                                         (indirect-target (p) (eopl:error 'deref "Illegal reference: ~s" ref1)))))
                                                              ))
                                           (eopl:error 'prim-adicionar-lista "Cannot modify immutable list")
                                           )))
                              )))))

;;eval-lista-a-modificar: <lista-a-modificar> <expresion>
;;<expresion> <enviroment> -> vector-modificado
;;función que evalúa <lista-a-modificar>
;;(procesa/elimina la sintaxis abstracta de lista-a-modificar
;;para segun sea el caso)

(define eval-lista-a-modificar
  (lambda (lista pos nuevo-valor env)
    (cases lista-a-modificar lista
      (lista-a-modificar-id (id) (let* ((vec (apply-env env id))
                                        (index (eval-rand2 pos env))
                                        (ref (apply-env-ref env id))
                                        (mutable (ismutable? ref)))
                                   (if mutable
                                       (vector-set! vec index nuevo-valor)
                                       (eopl:error 'vector-set! "No se puede modificar la lista constante"))
                                   vec))
      (lista-a-modificar-lista (exp) (let ((lista (eval-rands-list (cons exp '()) env))
                                           (index (eval-rands2 (cons pos '()) env)))
                                       (vector-set! (car lista) (car index) nuevo-valor)
                                       (car lista)))
      )))


(define eval-rands2
  (lambda (rands env)
    (map (lambda (x) (eval-rand2 x env)) rands)))

(define eval-rand2
  (lambda (rand env)
    (eval-expresion rand env)))

;;funciones auxiliares para aplicar eval-expresion2 a cada elemento de una 
;;lista de operandos (expresiones)

(define eval-rands-exp2
  (lambda (rands env)
    (map (lambda (x) (eval-rand-exp2 x env)) rands)))

(define eval-rand-exp2
  (lambda (rand env)
    (eval-expresion2 rand env)))

;;eval-expresion2: <expresion> <enviroment>
;;evalua la expresión segun sea el caso en el ambiente de entrada
;;sirve para diferenciar lo que se desea evaluar
;;o dejar en sintaxis abstracta al introducirlo a la lista

(define eval-expresion2
  (lambda (lista env)
    (cases expresion lista
      (id-exp (id) (apply-env env id))
      (lista-exp (expr-lista) (eval-expr-lista expr-lista env))
      (tupla-exp (prim tupla) (apply-prim-tupla prim tupla env)) ;;Tupla
      (proc-exp (ids body) (closure ids body env))
      (else lista)
      )))

;;funcion auxiliar para obtener el id sin sintaxis abstracta 
;;de una lista en una expresion append

(define obtener-id-lista
  (lambda (expr)
    (cases expresion expr
      (id-exp (id) id)
      (else (eopl:error 'obtener-id-lista "Expected named list")))))

;;funciones auxiliares para aplicar eval-expr-lista a cada elemento  
;;de una lista de operandos (expr-lista-normal)

(define eval-rands-list
  (lambda (rands env)
    (map (lambda (x) (eval-rand-list x env)) rands)))

(define eval-rand-list
  (lambda (rand env)
    (eval-expr-lista rand env)))




;; -- Funciones auxiliares para los Registros --

;; cree esta funcion se supone que es aca en donde se crea ya el vector con los ids y los valors 

(define eval-expr-registro
  (lambda (expr env)
    (cases expr-registro expr
      (registro-expr-create (ids vals) (apply vector (list (apply vector ids) (apply vector vals))))
      (registro-expr-validator (exp)
                               (let* ((args (eval-expresion exp env))
                                     )
                                   (if (vector? args)
                                       (if (and (vector? (vector-ref args 0)) (vector? (vector-ref args 1)))
                                          (eval-bool (bool-true))
                                          (eval-bool (bool-false))
                                       )
                                       (eval-bool (bool-false))
                                       )                                                               
                                 )
                               )
      (registro-expr-set (id-registro keyword value )
                         (list id-registro keyword value))
      )))





;; -- Funciones auxiliares para las tuplas --

;;apply-prim-lista: <primitiva> <list-of-expresion> -> lista | bool
;;| elemlista(Todo lo que se pueda guardar)
;;función que aplica la primitiva dada a una lista

(define apply-prim-tupla
  (lambda (prim tupla env)
    (cases prim-tupla prim
      (prim-pregunta-vacioTupla () (if (null? (car (eval-rands2 tupla env))) (eval-bool (bool-true)) (eval-bool (bool-false))))
      (prim-crear-vacioTupla () '())
      (prim-crear-tupla () tupla)
      (prim-pregunta-tupla () (let ((args (eval-rands2 tupla env)))
                                (if (list? (car args)) (eval-bool (bool-true)) (eval-bool (bool-false)))))
      (prim-cabeza-tupla () (let* ((args (eval-rands2 tupla env))
                                   (tup (car args)))
                              (eval-expresion (car tup) env)))
      (prim-cola-tupla () (let* ((args (eval-rands2 tupla env))
                                 (tup (car args)))
                            (cdr tup)))
      (prim-ref-tupla () (let* ((args (eval-rands2 tupla env))
                                (tup (car args))
                                (vec (list->vector tup))
                                (index (cadr args)))
                           (eval-expresion (vector-ref vec index) env)))
      )))

;;************************************

;; -- Referencias --

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (mutable? boolean?))) ;;Una referencia tendra un nuevo elemento booleano para saber si es mutable

;; (define deref
;;   (lambda (ref)
;;     (primitive-deref ref)))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p) (eopl:error 'deref "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mutable?)
             (vector-ref vec pos)))))

;; (define setref!
;;   (lambda (ref val)
;;     (primitive-setref! ref val)))

(define setref!
  (lambda (ref expval)
    (let ((ref (cases target (primitive-deref ref)
                 (direct-target (expval1) ref)
                 (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec mutable?)
             (if mutable? ;;Condicional para saber si la variable es mutable, si es #t seteara la variable con un nuevo valor 
                 (vector-set! vec pos val)
                 (eopl:error 'setref! "No se puede modificar la constante en la posición ~s" pos)
                 )))))

;;************************************

;; -- Procedimientos --

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;;apply-procedure: evalua el cuerpo de un procedimientos
;;en el ambiente extendido correspondiente

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expresion body (extend-env ids args env))))))

;;Definición del tipo de dato blanco (target)

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (list? x) (vector? x) (boolean? x) (string? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec m)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

;;************************************

;; -- Circuitos --

;; guardar-gates: (gates_organizados) (environment) (valor_inicial_nulo) -> (valor_del_ultimo_gate)
;; Función que recorre una lista de compuertas (gates) organizadas con eval-gate, 
;; luego evalúa cada una usando su tipo e inputs, y actualiza el ambiente con el resultado
;; de cada compuerta. Devuelve el valor de la última compuerta evaluada.


(define guardar-gates
  (lambda (gates env ultimoValor)
    (if (null? gates)
        ultimoValor
    (let* ((gate (car gates))
               (id (cons (car gate) '()))
               (valor (eval-type (cadr gate) (eval-input-list (caddr gate) env)))
               (lista-evaluda (eval-input-list (caddr gate) env))
               (valor_lista (cons valor '()))
               (updated-env (extend-env id valor_lista env))
               )
          (guardar-gates (cdr gates) updated-env valor)
      ))))

(define eliminar_parentesis
  (lambda (lista)
    (if (null? lista)
        '()
        (if (list? (car lista))
            (append (car lista) (eliminar_parentesis (cdr lista)))
            (cons (car lista) (eliminar_parentesis (cdr lista)))
         ) 
     )
   )
 )

;; evaluar-merge: <tipo compuerta> <circuito1> <circuito2> <id> <env> -> <resultado_nuevo_circuito>
;; Función que recibe una compuerta, dos circuitos y un entorno (env).
;; Primero, evalúa las compuertas de los dos circuitos con la función guardar-gates.
;; Luego, crea un nuevo circuito usando los resultados obtenidos de las evaluaciones.

(define evaluar-merge
  (lambda (arg env)
    (let* ((compuerta (car arg))
           (resultado1 (guardar-gates (cadr arg) env 'noHayValorAun))
           (resultado2 (guardar-gates (caddr arg) env 'noHayValorAun))
           (id (cadddr arg)))
              (eval-expresion (circuit-exp 
                      (a-circuit (a-gate-list (list
                                               (a-gate id compuerta
                                                       (input-list-bool
                                                        (if (equal? resultado1 (direct-target #t)) (bool-true) (bool-false))
                                                        (input-list-bool
                                                         (if (equal? resultado2 (direct-target #t)) (bool-true) (bool-false))
                                                         (input-list-empty)))))))) env))))

;; connect-circuits: <circuit> <circuit> <var-exp-connect> <enviroment> -> <gate_list> evaluado
;; Función que recive unos argumentos, conformados por dos circuitos
;; y un symbol, obtiene el resultado del primer circuito y despues asocia ese
;; resultado a el symbol que recibe. Por ultimo ejecuta el siguiente circuito con el
;; valor actualizado en el entorno

; (define connect-circuits
;   (lambda (args env)
;     (let* ((circuit1 (car args))
;            (circuit2 (cadr args))
;            (simbolo-reemplazar (last args))
;            (valorCircuit1  (guardar-gates (cons circuit1 '()) env 'noHayValorAun))
;            (env-actualizado (extend-env (list simbolo-reemplazar) (list valorCircuit1) env))
;            (nuevosValores (eval-input-list (caddr circuit2) env-actualizado))
;            )
;       (eval-expresion (circuit-exp 
;                       (a-circuit (a-gate-list (list
;                                                (a-gate (car circuit2) (cadr circuit2)
;                                                        (input-list-bool
;                                                         (if (equal? (car nuevosValores) #t) (bool-true) (bool-false))
;                                                         (input-list-bool
;                                                          (if (equal? (cadr nuevosValores) #t) (bool-true) (bool-false))
;                                                          (input-list-empty)))))))) env-actualizado)      
;       )))


(define connect-circuits
  (lambda (args env)
    (let* ((circuit1 (car args))
           (circuit2 (cadr args))
           (simbolo-reemplazar (last args))
           (valorCircuit1  (guardar-gates (cons circuit1 '()) env 'noHayValorAun))
           (env-actualizado (extend-env (list simbolo-reemplazar) (list valorCircuit1) env))
           (nuevosValores (eval-input-list (caddr circuit2) env-actualizado))
           )
      (eval-expresion (circuit-exp 
                      (a-circuit (a-gate-list (list
                                               (a-gate (car circuit2) (cadr circuit2)
                                                       (input-list-bool
                                                        (if (equal? (car nuevosValores) #t) (bool-true) (bool-false))
                                                        (input-list-bool
                                                         (if (equal? (cadr nuevosValores) #t) (bool-true) (bool-false))
                                                         (input-list-empty)))))))) env-actualizado)      
      )))

;; last <list> -> <value>
;; Funcion que se encarga de retornar el ultimo valor de una lista
;; se creo unicamente para ser usada en la primitiva "connect-circuits"
(define last
  (lambda (lst)
    (if (null? (cdr lst))
      (car lst)
      (last (cdr lst)))))

;; eval-circuit: <circuit> <enviroment> -> <gate_list> evaluado
;; Función que evalúa un circuito (procesa/elimina la sintaxis abstracta de circuito
;; y envia a evaluar el gate_list)
(define eval-circuit
  (lambda (crt env)
    (cases circuit crt
      (a-circuit (gatelst) (eval-gate-list gatelst env)))
    ))

;; eval-gate-list: <gatelist> <enviroment> -> lista de gates evaluados
;; Función que evalúa un gatelist (procesa/elimina la sintaxis abstracta de gatelist
;; y envia a evaluar cada gate)
(define eval-gate-list
  (lambda (gatelst env)
    (cases gatelist gatelst
      (a-gate-list (gates) (map (lambda (g) (eval-gate g env)) gates)))
   ))

;; eval-gate: <gate> <enviroment> -> lista con los datos de un gate
;; Función que evalúa un gate (procesa/elimina la sintaxis abstracta de gate
;; y retorna una lista con los datos del gate)
(define eval-gate
  (lambda (gates env)
    (cases gate gates
      (a-gate (id type input) (list id type input))))
   )

;; Pruebas de eval-circuit | eval-gate-list | eval-gate
;;
;; (interpretador)
;; (circuit (gate_list (gate G1 (type and) (input_list A B))))
;; Salida: ((G1 #(struct:and-type) #(struct:input-list A #(struct:input-list B #(struct:input-list-empty)))))
;;
;; (interpretador)
;; (circuit (gate_list (gate G1 (type and) (input_list A B)) (gate G2 (type not) (input_list B))))
;; Salida: ((G1 #(struct:and-type) #(struct:input-list A #(struct:input-list B #(struct:input-list-empty))))
;;          (G2 #(struct:not-type) #(struct:input-list B #(struct:input-list-empty))))

;*******************************

;; eval-input-list: <input_list> <enviroment> -> lista con los input_list evaluados
;; Función que evalúa cada input_list (procesa la sintaxis abstracta de input_list
;; y retorna una lista con cada input evaluado (True o False))
(define eval-input-list
  (lambda (inptls env)
    (cases input_list inptls
      (input-list-empty () empty)
      (input-list (id input) (let* ((valor (apply-env env id))
                                    ;(valor (if (list? id-encontrado) (cadr id-encontrado) id-encontrado))
                                    (valor-en-booleano (if (equal? valor #t) (bool-true) (bool-false)))
                                    )
                                 (cons (eval-bool valor-en-booleano) (eval-input-list input env))
                               ))
      (input-list-bool (bool input) (cons (eval-bool bool) (eval-input-list input env))))
    ))

;; eval-type: <type> (lista de inputs evaluados) -> booleano
;; Función que evalúa el valor de un gate (procesa la sintaxis abstracta del tipo
;; y opera los inputs evaluados con el tipo)
(define eval-type
  (lambda (tp inputs_evaluados)
    (cases type tp
      (and-type () (if (boolean? (car inputs_evaluados))
                       (if (null? inputs_evaluados)
                           (direct-target #t)
                           (if (and (eqv? #t (car inputs_evaluados))
                                    (eqv? #t (cadr inputs_evaluados)))
                               (direct-target #t)
                               (direct-target #f)))
                       (if (null? inputs_evaluados)
                           (direct-target #t)
                           (cases target (car inputs_evaluados)
                             (direct-target (expresion) (if (and (eqv? #t expresion)
                                                                 (eqv? #t (cadr inputs_evaluados)))
                                                            (direct-target #t)
                                                            (direct-target #f)))
                             (else 0))
                           )))

      (or-type () (if (boolean? (car inputs_evaluados))
                       (if (null? inputs_evaluados)
                           (direct-target #t)
                           (if (or (eqv? #t (car inputs_evaluados))
                               (eqv? #t (cadr inputs_evaluados)))
                               (direct-target #t)
                               (direct-target #f)))
                       (if (null? inputs_evaluados)
                           0
                           (cases target (car inputs_evaluados)
                             (direct-target (expresion) (if (or (eqv? #t expresion)
                                                                (eqv? #t (cadr inputs_evaluados)))
                                                            (direct-target #t)
                                                            (direct-target #f)))
                             (else 0))
                           )))

      (not-type () (if (boolean? (car inputs_evaluados))
                       (if (null? inputs_evaluados)
                           '()
                           (if (eqv? #t (car inputs_evaluados))
                               (direct-target #f) (direct-target #t)))
                       (if (null? inputs_evaluados)
                           0
                           (cases target (car inputs_evaluados)
                             (direct-target (expresion) (if (eqv? #t expresion)
                                                            (direct-target #f) (direct-target #t)))
                             (else 0))
                           )))
      
      (xor-type ()
        (if (null? inputs_evaluados)
            '()
            (let* ((elemento1 (car inputs_evaluados))
                   (elemento2 (cadr inputs_evaluados))
                   (resultado (if (not (eqv? elemento1 elemento2)) (direct-target #t) (direct-target #f))))
              resultado))))
    ))