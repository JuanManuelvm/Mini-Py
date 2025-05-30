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
;;                ::= lista->tupla(<lista>)  
;;                    <lista-a-tupla (lista)>
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
;;
;;************************************************************************************************************

;;Especificación Léxica

(define scanner
'((white-sp (whitespace) skip)
  (comment ("#" (arbno (not #\newline))) skip)
  (identifier ((or letter "_") (arbno (or letter digit "?" "_"))) symbol)
  (number (digit (arbno digit) "." digit (arbno digit)) number)
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (number (digit (arbno digit)) number)
  (number ("-" digit (arbno digit)) number)
  (string ("\"" (arbno (not #\")) "\"") string)
  ))

;;Especificación Sintáctica (gramática)

(define grammar
  '(
    (program ((arbno class-decl) expresion) a-program)
    
    (expresion (number) num-exp)
    (expresion (identifier) id-exp)
    (expresion ("(" number (arbno number)")") hexa-exp) ;; Representacion bignum
    (expresion ("num->hexa" "(" number ")") convertir-num-a-hexa)
    (expresion (string) text-exp)
    (expresion (bool) bool-exp)
    (expresion (primitive "(" (separated-list expresion ",")")") primapp-exp)
    (expresion (expr-bool) bool-app-exp)
    (expresion (expr-lista) lista-exp)
    (expresion (prim-tupla "[" (separated-list expresion ";") "]") tupla-exp)
    (expresion (expr-registro) registro-exp)
    (expresion ("'" identifier) var-exp-connect) ;; 'id como expresion para poder utilizarlo en connect-circuits
    (expresion (type) type-exp) ;; Tipo como expresion para ser procesado correctamente en primapp-exp al aplicar la primitiva merge-circuit
    (expresion ("lista->tupla" "(" expresion ")") lista-a-tupla) 

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
    (expresion ("circuito" circuit) circuit-exp) ;; Circuito como expresion para procesar un circuito
    (expresion ("new" identifier "(" (separated-list expresion ",") ")") new-object-exp)
    (expresion ("ejecutar" expresion "." identifier "("  (separated-list expresion ",") ")") method-app-exp)
    (expresion ("super" identifier "("  (separated-list expresion ",") ")") super-call-exp)
    (expresion (return) return-exp)
    
    (primitive ("+") add-prim-aritmetica) 
    (primitive ("-") substract-prim-aritmetica) 
    (primitive ("*") mult-prim-aritmetica) 
    (primitive ("/") div-prim-aritmetica) 
    (primitive ("%") residuo-prim-aritmetica) 
    (primitive ("add1") incr-prim-aritmetica) 
    (primitive ("sub1") decr-prim-aritmetica) 
    (primitive ("len") longitud-prim-text) 
    (primitive ("concat") concatenar-prim-text)
    (primitive ("eval-circuit") eval-circuit-primitive) 
    (primitive ("merge-circuit") merge-circuit-primitive) 
    (primitive ("connect-circuits") connect-circuits-primitive) 
    
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

     ;; -- Gramatica para Registros --
    
    (expr-registro ("crear-registro" "{" (arbno identifier ":" expresion",") "}" ) registro-expr-create)
    (expr-registro ("registro?" "{" expresion "}" ) registro-expr-validator)
    (expr-registro ("registro-set" "{" identifier ":" identifier ":" expresion "}" ) registro-expr-set)
    (expr-registro ("registro-ref" "{" expresion ":" identifier"}" ) registro-expr-ref)

    ;; -- Gramatica para circuitos --
    
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

    ;; -- Gramatica para objetos --

    (class-decl ("class" identifier "(" identifier ")" ":" (arbno "self." identifier) (arbno method-decl)) a-class-decl)
    (method-decl ("def" identifier "("  (separated-list identifier ",") ")" ":" expresion) a-method-decl)
    (return ("return f" (arbno fstring-part)) fstring-exp)
    (return ("return" expresion) fexpresion-exp)
    (fstring-part ("s" "(" string ")") fstring-literal)
    (fstring-part ("{" expresion "}") fstring-interp)
    ;(expresion ("mostrar") mostrar-exp)
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

;;eval-program: <programa> -> valores...
;;Evalúa un programa completo. Primero procesa las declaraciones de clases
;;para configurar el ambiente global de clases, y luego evalúa la expresión principal
;;del programa en un ambiente vacío.

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (elaborate-class-decls! c-decls) 
        (eval-expresion exp (empty-env))))))

;;Ambiente inicial

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
      (primapp-exp (prim rands) (let ((args (eval-primapp-exp-rands rands env)))
                                  (apply-primitive prim args env)))
      (bool-app-exp (exprbooleana) (eval-expr-bool exprbooleana env))
      (lista-exp (expr-lista) (eval-expr-lista expr-lista env))
      (lista-a-tupla (lista) (lista-tupla lista env))
      (tupla-exp (prim tupla) (apply-prim-tupla prim tupla env))
      (let-exp (ids exps body) (let ((args (eval-let-exp-rands exps env)))
                                 (eval-expresion body (extend-env ids args env))
                                 ))
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
      (registro-exp (val) (eval-expr-registro val env))
      (print-exp (exp) (let* ((resultado (eval-expresion exp env))
                              (validacion (es-un-objeto? resultado)))
                         (if validacion
                             (find-method-and-apply '__str__ (object->class-name resultado) resultado '())
                             (begin (display resultado)
                                    (display " ")
                                    1
                               ))))
      (circuit-exp (circuito) (eval-circuit circuito env)) 
      (type-exp (type) type) 
      (var-exp-connect (id) id)
      (new-object-exp (class-name rands) (let ((args (eval-rands rands env))
                                               (obj (new-object class-name)))
                                           (find-method-and-apply '__init__ class-name obj args)
                                           obj))
      
      (method-app-exp (obj-exp method-name rands) (let ((args (eval-rands rands env))
                                                        (obj (eval-expresion obj-exp env)))
                                                    (find-method-and-apply method-name (object->class-name obj) obj args)))
      (super-call-exp (method-name rands) (let ((args (eval-rands rands env))
                                                (obj (apply-env3 env 'self)))
                                            (find-method-and-apply method-name (apply-env3 env '%super) obj args)
                                            ))
      (return-exp (exp-return) (eval-return exp-return env))
      ;(mostrar-exp () the-class-env)
      )))

;;apply-primitive: <primitiva> <list-of-expression> -> numero | text | circuit
;;función que aplica la primitiva dada

(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim
      (add-prim-aritmetica () (+ (car args) (cadr args)))
      (substract-prim-aritmetica () (- (car args) (cadr args)))
      (mult-prim-aritmetica () (* (car args) (cadr args)))
      (div-prim-aritmetica () (/ (car args) (cadr args)))
      (residuo-prim-aritmetica () (modulo-real (car args) (cadr args)))
      (incr-prim-aritmetica () (+ (car args) 1))
      (decr-prim-aritmetica () (- (car args) 1))
      (longitud-prim-text () (string-length (car args)))
      (concatenar-prim-text () (string-append (car args) (cadr args)))
      
      ;; Primitivas para ciruitos 
      
      (eval-circuit-primitive () (guardar-gates (eliminar_parentesis args) env 'noHayValorAun)) ;; Procesar la primitiva eval-circuit
      (merge-circuit-primitive () (evaluar-merge args env)) ;; Procesar la primitiva merge-circuit
      (connect-circuits-primitive () (connect-circuits (eliminar_parentesis args) env)) ;; Procesar la primitiva connect-circuits-primitive
      )))


;;************************************************************************************************************

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

;;extend-env-refs: <list-of symbols> <vector> enviroment -> enviroment
;;función que crea un ambiente extendido con variables mutables
;;
;; Argumentos:
;; - syms: lista de identificadores de campos (símbolos)
;; - vec: vector que contiene los valores actuales de esos campos
;; - env: entorno actual que será extendido

(define extend-env-refs
  (lambda (syms vec env)
    (extended-env-record syms vec (make-vector (length syms) #t) env)))

;;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;;función que crea un ambiente extendido para procedimientos recursivos

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec (make-vector (length idss) #t) old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))


;;iota: number -> list
;;función que retorna una lista de los números desde 0 hasta end

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))


;;función que busca un símbolo en un ambiente y retorna el valor almacenado en la referencia

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym)))) 

;;función que busca un símbolo en un ambiente y retorna el valor almacenado en la referencia
;;se utiliza para la implementacion de objetos (no utiliza el deref)

(define apply-env3
  (lambda (env sym)
    (primitive-deref (apply-env-ref env sym))))

;;función que busca un símbolo en un ambiente y retorna la referencia

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

;;************************************************************************************************************

;; -- Funciones Auxiliares generales --

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

;;Funcion auxiliar para evalúar una lista de operandos (rands)
;;en un entorno dado aplicando "eval-rand" a cada uno

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;;Funcion auxiliar que evalúa un operando (rand) en el entorno dado:
;;Si es una expresión id-exp, obtiene la referencia del id en el entorno
;;y aplica `primitive-deref` para obtener su valor de target:
;;   - Si el valor es un vector, retorna un indirect-target.
;;   - Si no, retorna un direct-target con el valor.
;;Si no es una id-exp, evalúa la expresión completa y retorna su valor como direct-target.

(define eval-rand
  (lambda (rand env)
    (cases expresion rand
      (id-exp (id) (let ((ref (apply-env-ref env id)))
                      (cases target (primitive-deref ref)
                        (direct-target (expval) (if (vector? expval)
                                                    (indirect-target ref)
                                                    (direct-target expval)))
                        (indirect-target (ref1) (indirect-target ref1)))))
      (else (direct-target (eval-expresion rand env))))))


;;funciones auxiliares para aplicar eval-expresion a cada elemento de una 
;;lista de operandos (expresiones)

(define eval-rands2
  (lambda (rands env)
    (map (lambda (x) (eval-rand2 x env)) rands)))

(define eval-rand2
  (lambda (rand env)
    (eval-expresion rand env)))

;;funcion auxiliar para aplicar eval-expresion a cada elemento de una 
;;lista de operandos (expresiones) (se utiliza en el primapp)

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expresion x env)) rands)))

;;funciones auxiliares para aplicar eval-expresion a cada elemento de una 
;;lista de operandos (expresiones) y retornarlo como direct-target
;;(se utiliza en el var/const)

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env)) rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expresion rand env))))

(define es-un-objeto?
  (lambda (obj)
    (if (null? obj)
        #f
        (if (list? obj)
            (if (part? (car obj))
                #t
                #f)
            #f)
        )))

(define modulo-real
  (lambda (a b)
    (- a (* b (floor (/ a b))))))

;;************************************************************************************************************

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
    (define arg1 (bool->num (car args)))
    (define arg2 (bool->num (cadr args)))
    (cases pred-prim prim
      (menor-que () (< arg1 arg2))
      (mayor-que () (> arg1 arg2))
      (menor-igual-que () (<= arg1 arg2))
      (mayor-igual-que () (>= arg1 arg2))
      (igual-que () (equal? (car args) (cadr args)))
      (diferente-que () (not (equal? (car args) (cadr args))))
      )))

(define bool->num
  (lambda (v)
    (cond ((eq? v #t) 1)
          ((eq? v #f) 0)
          (else v))))

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

;;************************************************************************************************************

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
                                             (direct-target (expval1) (let ((new-list (append (vector->list expval1) (if (number? (car args))
                                                                                                                         (list (num-exp (car args)))
                                                                                                                         args))))
                                                                        (setref! vec-ref (apply vector new-list))
                                                                        vec))
                                             (indirect-target (ref1)
                                                              (cases reference ref1
                                                                (a-ref (pos2 vec2 mutable2?)
                                                                       (cases target (vector-ref vec2 pos2)
                                                                         (direct-target (expval2)
                                                                                        (if mutable2?
                                                                                            (let ((new-list (append (vector->list expval2) (if (number? (car args))
                                                                                                                                               (list (num-exp (car args)))
                                                                                                                                               args))))
                                                                                              (setref! ref1 (apply vector new-list))
                                                                                              vec2)
                                                                                            (eopl:error 'prim-adicionar-lista "Cannot modify immutable list") ))
                                                                         (indirect-target (p) (eopl:error 'deref "Illegal reference: ~s" ref1)))))
                                                              ))
                                           (eopl:error 'prim-adicionar-lista "Cannot modify immutable list")
                                           )))))
      )))

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
      (lista-a-tupla (lista) (lista-tupla lista env))
      (primapp-exp (prim rands) (let ((args (eval-primapp-exp-rands rands env)))
                                  (apply-primitive prim args env)))
      (app-exp (rator rands) (let ((proc (eval-expresion rator env))
                                   (args (eval-rands rands env)))
                               (if (procval? proc)
                                   (apply-procedure proc args)
                                   (eopl:error 'eval-expresion "Attempt to apply non-procedure ~s" proc))))
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

(define lista-tupla
  (lambda (lista env)
    (cases expresion lista
      (id-exp (id) (let ((ref(apply-env-ref env id)))
                     (cases reference ref
                       (a-ref (pos vec mutable?)
                              (if mutable?
                                  (cases target (vector-ref vec pos)
                                    (direct-target (expval1) (apply-prim-tupla (prim-crear-tupla) (vector->list expval1) env))
                                    (indirect-target (ref1) (cases reference ref1
                                                              (a-ref (pos1 vec1 mutable1?)
                                                                     (if mutable1?
                                                                         (cases target (vector-ref vec1 pos1)
                                                                           (direct-target (expval2) (apply-prim-tupla (prim-crear-tupla) (vector->list expval2) env))
                                                                           (indirect-target (ref2) (eopl:error 'deref "Illegal reference: ~s" ref2)))
                                                                         (eopl:error 'prim-adicionar-lista "Cannot modify immutable list")
                                  )))))
                                  (eopl:error 'prim-adicionar-lista "Cannot modify immutable list")
                                  )))))
      (else 0))))

;;************************************************************************************************************

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

;;************************************************************************************************************

;; -- Funciones auxiliares para los Registros --


;;eval-expr-registro: <primitiva> enviroment -> vector | bool
;;| elemvector(Todo lo que se pueda guardar) 
;;función que aplica la primitiva dada a un registro

(define eval-expr-registro
  (lambda (expr env)
    (cases expr-registro expr
      (registro-expr-create (ids vals) (apply vector (list (apply vector ids) (apply vector (eval-rands-exp2 vals env)))))
      (registro-expr-validator (exp) (let* ((args (eval-expresion exp env)))
                                       (if (vector? args)
                                           (if (and (vector? (vector-ref args 0)) (vector? (vector-ref args 1)))
                                               (eval-bool (bool-true))
                                               (eval-bool (bool-false)))
                                           (eval-bool (bool-false))
                                           )))
      (registro-expr-ref (exp keyword) (let* ((registro (eval-expresion exp env)))
                                         (if (and (vector? registro) (= (vector-length registro) 2) (vector? (vector-ref registro 0)) (vector? (vector-ref registro 1)))
                                             (let* ((campos (vector-ref registro 0))
                                                    (valores (vector-ref registro 1))
                                                    (index (buscar-indice campos keyword)))
                                               (if index
                                                   (eval-expresion (vector-ref valores index) env)
                                                   (eopl:error 'registro-ref "Campo no encontrado en el registro: ~a" keyword)))
                                             (eopl:error 'registro-ref "La expresión no evalúa a un registro válido: ~a" registro))))
      (registro-expr-set (id-registro keyword value) (let* ((registro (apply-env env id-registro))
                                                            (campos (vector-ref registro 0))
                                                            (valores (vector-ref registro 1))
                                                            (index (buscar-indice campos keyword))
                                                            (nuevo (eval-expresion value env)))
                                                       (if index
                                                           (begin
                                                             (vector-set! valores index value)
                                                             registro)
                                                           (eopl:error 'registro-set "Campo no encontrado en el registro: ~a" keyword))))
      )))

(define buscar-indice
  (lambda (vec key)
    (let loop ((i 0))
      (cond ((= i (vector-length vec)) #f) ;; no encontrado
            ((eq? (vector-ref vec i) key) i)
            (else (loop (+ i 1))))))
)

;;************************************************************************************************************

;; -- Referencias --


(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (mutable? boolean?))) ;;Una referencia tendra un nuevo elemento booleano para saber si es mutable

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

;;************************************************************************************************************

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

;;Definición del tipo de dato target

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (list? x) (vector? x) (boolean? x) (string? x) (part? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec m)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

;;************************************************************************************************************

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
               (updated-env (extend-env id valor_lista env)))
          (guardar-gates (cdr gates) updated-env valor)
      ))))

(define eliminar_parentesis
  (lambda (lista)
    (if (null? lista)
        '()
        (if (list? (car lista))
            (append (car lista) (eliminar_parentesis (cdr lista)))
            (cons (car lista) (eliminar_parentesis (cdr lista)))))))

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

;; eval-input-list: <input_list> <enviroment> -> lista con los input_list evaluados
;; Función que evalúa cada input_list (procesa la sintaxis abstracta de input_list
;; y retorna una lista con cada input evaluado (True o False))

(define eval-input-list
  (lambda (inptls env)
    (cases input_list inptls
      (input-list-empty () empty)
      (input-list (id input) (let* ((valor (apply-env env id))
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
              resultado))))))

;;************************************************************************************************************

;; -- Objetos --


;; -- Ambiente de clases --


;; the-class-env: lista-de-declaraciones-de-clase
;; Representa el entorno global de clases del programa. Se inicializa como una
;; lista vacía y luego es actualizado por la función `elaborate-class-decls!`.
;; Contiene las definiciones de clases disponibles para ser usadas en la evaluación
;; de expresiones del lenguaje.

(define the-class-env '())

;;elaborate-class-decls!: lista-de-declaraciones-de-clase -> indefinido
;;Modifica el entorno global de clases (`the-class-env`) asignándole las
;;declaraciones de clases pasadas como argumento. Este procedimiento muta
;;el entorno global, por lo que afecta futuras evaluaciones de expresiones
;;relacionadas con clases.

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

;;lookup-class: símbolo -> declaración-de-clase
;;Busca una clase por su nombre en el entorno global `the-class-env`.
;;Si la clase se encuentra, retorna su declaración. Si no, lanza un error
;;indicando que la clase es desconocida.

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env) (eopl:error 'lookup-class "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;lookup-method-decl: símbolo × lista-de-declaraciones-de-método -> declaración-de-método o #f
;;Busca una declaración de método con nombre `m-name` dentro de una lista de
;;declaraciones de método `m-decls`. Si encuentra una coincidencia, retorna
;;la declaración correspondiente. Si no, retorna `#f`.

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls))) (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))


;; -- Funciones para objetos (Partes) --


;;new-object: símbolo -> objeto
;;Crea una nueva instancia (objeto) de una clase dada por su nombre (class-name).
;;Si la clase es 'object' (la clase raíz), retorna una lista vacía.
;;De lo contrario, busca la declaración de la clase en el entorno de clases,
;;construye la parte correspondiente a esa clase, y recursivamente crea el objeto
;;de su superclase, concatenando ambos con `cons`.

(define new-object
  (lambda (class-name)
    (if (eqv? class-name 'object)
        '()
        (let ((c-decl (lookup-class class-name)))
          (cons
           ;(direct-target (make-first-part c-decl))
           (make-first-part c-decl)
           (new-object (class-decl->super-name c-decl)))))))


;;part: Representa una parte de un objeto, correspondiente a una clase en la jerarquía.
;;Cada objeto está compuesto por múltiples partes (una por cada clase en la cadena de herencia).
;;a-part: símbolo × vector -> part
;;Constructor que crea una parte de objeto. Contiene:
;; - class-name: el nombre de la clase (símbolo)
;; - fields: un vector con los valores de los campos de instancia de esa clase

(define-datatype part part? 
  (a-part (class-name symbol?)
          (fields vector?)))

;;make-first-part: declaración-de-clase -> parte-de-objeto
;;Construye una parte de un objeto a partir de una declaración de clase.
;;Esta parte contiene el nombre de la clase y un vector con tantos espacios
;;como campos definidos en la clase. El vector representa el almacenamiento
;;de los valores de los campos de instancia.

(define make-first-part
  (lambda (c-decl)
    (a-part (class-decl->class-name c-decl) (make-vector (length (class-decl->field-ids c-decl)) (direct-target 0)))))


;; -- Metodos -- 


;;find-method-and-apply: símbolo × símbolo × objeto × lista -> valor
;;Busca un método con nombre `m-name` en la clase `host-name`, subiendo por la
;;jerarquía de herencia si es necesario. Si encuentra la declaración del método,
;;aplica el método usando `apply-method`. Si llega a la clase raíz 'object' sin
;;encontrar el método, lanza un error.
;;
;; Argumentos:
;; - m-name: nombre del método a buscar
;; - host-name: nombre de la clase desde donde iniciar la búsqueda
;; - self: el objeto receptor del mensaje (this)
;; - args: argumentos con los que se invocará el método

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
        (eopl:error 'find-method-and-apply "No method for name ~s" m-name)
        (let ((m-decl (lookup-method-decl m-name (class-name->method-decls host-name))))
          (if (method-decl? m-decl)
              (apply-method m-decl host-name self args)
              (find-method-and-apply m-name (class-name->super-name host-name) self args))))))

;;apply-method: declaración-de-método × símbolo × objeto × lista -> valor
;;Aplica un método a un objeto dado. Evalúa el cuerpo del método en un entorno extendido
;;que asocia:
;; - `%super` con el nombre de la superclase del `host-name`
;; - `self` con el objeto receptor
;; - los parámetros formales (`ids`) con los argumentos reales (`args`)
;; Además, incluye las variables de instancia disponibles para `host-name`
;; mediante `build-field-env`.
;;
;; Argumentos:
;; - m-decl: declaración del método a aplicar
;; - host-name: clase desde donde se accedió al método
;; - self: el objeto que recibe el mensaje
;; - args: lista de argumentos para el método

(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name)))
      (eval-expresion body (extend-env (cons '%super (cons 'self ids))
                                       (cons super-name (cons self args))
                                       (build-field-env (view-object-as self host-name)))))))

;;build-field-env: lista-de-partes -> entorno
;;Construye un entorno que asocia los identificadores de campos con sus referencias
;;reales en el vector de campos, para cada parte del objeto.
;;Recorre recursivamente las partes (desde la clase más derivada hasta `object`)
;;y combina los entornos parciales usando `extend-env-refs`.

(define build-field-env
  (lambda (parts)
    (if (null? parts)
        (empty-env)
        (extend-env-refs (part->field-ids (car parts))
                         (part->fields    (car parts))
                         (build-field-env (cdr parts))))))

;;view-object-as: objeto × símbolo -> lista-de-partes
;;Extrae una "vista" del objeto como si fuera una instancia de una clase específica.
;;Retorna la sublista de partes del objeto comenzando desde la parte correspondiente
;;a `class-name`.

(define view-object-as
  (lambda (parts class-name)
    (if (eqv? (part->class-name (car parts)) class-name)
        parts
        (view-object-as (cdr parts) class-name))))


;; -- Extractores para declaraciones (de clases y metodos) --


;;class-decl->class-name: declaración-de-clase -> símbolo
;;Extrae el nombre de la clase desde una declaración de clase.

(define class-decl->class-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls) class-name))))

;;class-decl->super-name: declaración-de-clase -> símbolo
;; Extrae el nombre de la superclase desde una declaración de clase.

(define class-decl->super-name
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls) super-name))))

;;class-decl->field-ids: declaración-de-clase -> lista-de-símbolos
;;Extrae la lista de identificadores de campos definidos en una clase.

(define class-decl->field-ids
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls) field-ids))))

;;class-decl->method-decls: declaración-de-clase -> lista-de-declaraciones-de-método
;;Extrae la lista de métodos definidos en una clase.

(define class-decl->method-decls
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (class-name super-name field-ids m-decls) m-decls))))

;;method-decls->method-names: lista-de-declaraciones-de-método -> lista-de-símbolos
;;Dada una lista de declaraciones de método, extrae los nombres de cada uno.

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

;;method-decl->method-name: declaración-de-método -> símbolo
;;Extrae el nombre del método desde una declaración de método

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) method-name))))

;;method-decl->ids: declaración-de-método -> lista-de-símbolos
;;Extrae los parámetros (identificadores) de una declaración de método.

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) ids))))

;;method-decl->body: declaración-de-método -> expresión
;;Extrae el cuerpo del método desde una declaración de método.

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (method-name ids body) body))))


;; -- Extractores para partes (objetos) y otros --


;;part->class-name: part -> símbolo
;;Extrae el nombre de la clase desde una parte del objeto.

(define part->class-name
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields) class-name))))

;;part->fields: part -> vector
;;Extrae el vector de campos de instancia desde una parte del objeto.

(define part->fields
  (lambda (prt)
    (cases part prt
      (a-part (class-name fields) fields))))

;;part->field-ids: part -> lista-de-símbolos
;;Extrae los identificadores de los campos de una parte del objeto, usando su declaración de clase.

(define part->field-ids
  (lambda (part)
    (class-decl->field-ids (part->class-decl part))))

;;part->class-decl: part -> declaración-de-clase
;;Obtiene la declaración de clase asociada a una parte del objeto.

(define part->class-decl
  (lambda (part)
    (lookup-class (part->class-name part))))

;;part->method-decls: part -> lista-de-declaraciones-de-método
;;Extrae las declaraciones de método desde la clase asociada a una parte.

(define part->method-decls
  (lambda (part)
    (class-decl->method-decls (part->class-decl part))))

;;part->super-name: part -> símbolo
;;Obtiene el nombre de la superclase de la clase asociada a una parte.

(define part->super-name
  (lambda (part)
    (class-decl->super-name (part->class-decl part))))

;;class-name->method-decls: símbolo -> lista-de-declaraciones-de-método
;;Dado el nombre de una clase, retorna la lista de métodos definidos en su declaración.

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))

;;class-name->super-name: símbolo -> símbolo
;;Dado el nombre de una clase, retorna el nombre de su superclase.

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

;;object->class-name: objeto -> símbolo
;;Dado un objeto (representado como lista de partes), retorna el nombre de su clase (la más derivada).
;;Esto se logra extrayendo el nombre de clase de la primera parte del objeto.

(define object->class-name
  (lambda (parts)
    (part->class-name (car parts))))


;; -- Funciones auxiliares para el return  --


(define eval-return
  (lambda (exp-return env)
    (cases return exp-return
      (fstring-exp (fstring-parts) (mostrar-return (eval-fstring-parts fstring-parts env)))
      (fexpresion-exp (exp) (eval-expresion exp env))
      )))

(define eval-fstring-parts
  (lambda (fstring-parts env)
    (map (lambda (x) (eval-fstring-part x env)) fstring-parts)))

(define eval-fstring-part
  (lambda (fpart env)
    (cases fstring-part fpart
      (fstring-literal (string) (substring string 1 (- (string-length string) 1)) )
      (fstring-interp (expr)  (eval-expresion expr env)))
    ))

(define mostrar-return
  (lambda (lst)
    (cond
      ((null? lst) (newline))
      (else (display (car lst))
            (mostrar-return (cdr lst))
            1))))