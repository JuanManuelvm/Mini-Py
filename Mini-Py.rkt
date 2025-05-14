#lang eopl

;; Gramatica

;;  <program>      ::= <expression>
;;                     <a-program (exp)>
;;
;;  <expresion>    ::= <numero>  
;;                     <num-exp (number)>
;;                 ::= <identificador>  
;;                     <id-exp (id)>
;;                 ::= <hexadecimal>  
;;                     <hexa-exp (base hexa-number)>
;;                 ::= <cadena>  
;;                     <text-exp (cadena-texto)>
;;                 ::= <bool>
;;                     <bool-exp (bool)>
;;                 ::= <primitive> ({<expression>}*(,))
;;                     <primapp-exp (prim rands)>
;;                 ::= <expr-bool> 
;;                     <bool-app-exp (expresionbooleana)>
;;                 ::= var {<identificador> = <expresion>}*(,) in <expresion> 
;;                     <var-exp (ids rands body)>
;;                 ::= const {<identificador> = <expresion>}*(,) in <expresion> 
;;                     <constante-exp (ids rands body)>
;;                 ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                 ::= set <identificador> = <expression>
;;                     <set-exp (exp exps)>
;;                 ::= if <expr-bool> then <expresion> [ else <expression> ] end
;;                     <if-exp (exp-bool true-exp false-exp)>
;; Pregunta ********************************************************************************
;;                 ::= while <expr-bool> do <expresion> done
;;                     <while-exp (expr-bool expresion)>
;;                 ::= for <identificador> in <expresion> do <expresion> done
;;                     <for-exp (ids expresion body)
;; Pregunta ********************************************************************************
;;                 ::= proc({<identificador>}*(,)) <expresion>
;;                     <proc-exp (ids body)>
;;                 ::= (<expresion> {<expresion>}*)
;;                     <app-exp proc rands>
;;
;;
;; <primitive>     ::= + | - | * | / | % | add1 | sub1
;;                 ::= len | concat
;;
;; <expr-bool>     ::= <pred-prim>(<expresion> , <expresion>)
;;                     <expr-bool-exps (prim exp1 exp2)>
;;                 ::= <oper-bin-bool>(<expr-bool> , <expr-bool>)
;;                     <expr-bool-bools (prim expbool expbool)>
;;                 ::= <oper-un-bool>(<expr-bool>)
;;                     <expr-bool-unbool (prim expbool)>
;;                 ::= expr <bool>
;;                     <exp-bool-booleano (bool)>
;;
;; <pred-prim>     ::= < | > | <= | >= | == | !=
;; <oper-bin-bool> ::= and | or
;; <oper-un-bool>  ::= not
;; <bool>          ::= True | False

;;

;;                 ::= rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion> --(letrec)--
;;                     <rec-exp (proc-names ids bodies bodyrec)>
;;
;;                 ::= <lista>
;;                 ::= <tupla>
;;                 ::= <registro>
;;                 ::= <circuit>
;;                 ::= while <expr-bool> do <expresion> done
;;                 ::= for <identificador> in <expresion> do <expresion> done
;;
;; <lista>         ::= [{<expresion>}∗(;)]
;; <tupla>         ::= tupla[{<expresion>}∗(;)]
;; <registro>      ::= {{<identificador> = <expresion>}+(;)}

;; <primitive> = UN POCO DE PRIMITIVAS


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
    (expresion ("(" number (arbno number)")") hexa-exp) ;; Base nuestra
    (expresion (string) text-exp)
    (expresion (bool) bool-exp)
    (expresion (primitive "(" (separated-list expresion ",")")") primapp-exp) ;; Base de interpretador del curso
    (expresion (expr-bool) bool-app-exp)

    (expresion (expr-lista) lista-exp) ;; YA QUEDO
    (expr-lista (prim-lista "[" (separated-list expresion ";") "]") expr-lista-normal) ;; YA QUEDO
    (expr-lista ("set-list" "[" lista-a-modificar ";" expresion ";" expresion "]") expr-lista-set) ;; YA QUEDO
    (lista-a-modificar ("id" identifier) lista-a-modificar-id) ;; YA QUEDO
    (lista-a-modificar ("lista-" expr-lista) lista-a-modificar-lista) ;; YA QUEDO
    (prim-lista ("crear-lista") prim-crear-lista) ;; YA QUEDO
    (prim-lista ("cabeza") prim-cabeza-lista) ;; YA QUEDO
    (prim-lista ("null?") prim-pregunta-vacioLista) ;; YA QUEDO
    (prim-lista ("cola") prim-cola-lista) ;; YA QUEDO
    ;(prim-lista ("crear-null") prim-crear-vacioLista)
    (prim-lista ("lista?") prim-pregunta-lista) 
    (prim-lista ("append") prim-adicionar-lista)
    (prim-lista ("ref-list") prim-ref-lista)
    
    ;;Tuplas
    (expresion ("tupla" "[" prim-tupla (separated-list expresion ";") "]") tupla-exp)
    (prim-tupla ("crear-tupla") prim-crear-tupla)
    (prim-tupla ("cabeza") prim-cabeza-tupla)
    (prim-tupla ("null?") prim-pregunta-vacioTupla)
    (prim-tupla ("crear-null") prim-crear-vacioTupla)
    (prim-tupla ("tupla?") prim-pregunta-tupla)
    (prim-tupla ("cola") prim-cola-tupla)
    (prim-tupla ("ref-tupla") prim-ref-tupla)

    (expresion ("var" (arbno identifier "=" expresion ",") "in" expresion) let-exp)
    (expresion ("const" (arbno identifier "=" expresion ",") "in" expresion) const-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identifier "=" expresion) set-exp)
    (expresion ("if" expr-bool "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("for" identifier "in" expresion "do" expresion "done") for-exp) 
    (expresion ("proc" "(" (separated-list identifier ",") ")" expresion) proc-exp)
    (expresion ("aplicar" "(" expresion (arbno expresion) ")") app-exp)

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
    (oper-bin-bool ("and") and-bool)
    (oper-bin-bool ("or") or-bool)
    (oper-un-bool ("not") not-bool)
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
     '(1 1 10)
     (empty-env))))


;;eval-expression: <expresion> <enviroment> -> numero
;;evalua la expresión en el ambiente de entrada

(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (num-exp (numero) numero)
      (id-exp (id) (apply-env env id))
      (hexa-exp (base hexa-number) (base-hexa->numero base hexa-number))
      (text-exp (cadena) (substring cadena 1 (- (string-length cadena) 1)))
      (bool-exp (booleano) (eval-bool booleano))
      (primapp-exp (prim rands) (let ((args (eval-rands rands env)))
                                  (apply-primitive prim args env)))
      (bool-app-exp (exprbooleana) (eval-expr-bool exprbooleana env))
      (lista-exp (expr-lista) (eval-expr-lista expr-lista env))
      (tupla-exp (prim tupla) (apply-prim-tupla prim tupla env)) ;;Tupla
      (let-exp (ids exps body) (let ((args (eval-rands exps env)))
                                 (eval-expresion body (extend-env ids args env))))
      (const-exp (ids exps body) (let ((args (eval-rands exps env)))
                                   (eval-expresion body (extend-const-env ids args env))))
      (begin-exp (exp exps) (let loop ((acc (eval-expresion exp env))
                                       (exps exps))
                              (if (null? exps)
                                  acc
                                  (loop (eval-expresion (car exps) env)
                                        (cdr exps)))))
      (while-exp (exp-bool exp) (let loop ()
                                  (if (eval-expr-bool exp-bool env)
                                      (begin (eval-expresion exp env)
                                             (loop))
                                      'done)))
      (for-exp (id iterable exp) (let* ((vec (eval-expresion iterable env))
                                        (n (vector-length vec)))
                                   (define (loop i)
                                     (if (>= i n)
                                         'done
                                         (let* ((elem (eval-expresion (vector-ref vec i) env))
                                                (nuevo-env (extend-env (list id) (list elem) env)))
                                           (eval-expresion exp nuevo-env)
                                           (loop (+ i 1)))))
                                   (loop 0)))
      (set-exp (id new-exps) (begin
                               (setref! (apply-env-ref env id) (eval-expresion new-exps env))
                               env))
      (if-exp (exp-bool true-exp false-exp) (if (eval-expr-bool exp-bool env)
                                                (eval-expresion true-exp env)
                                                (eval-expresion false-exp env)))
      (proc-exp (ids body) (closure ids body env))
      (app-exp (rator rands) (let ((proc (eval-expresion rator env))
                                   (args (eval-rands rands env)))
                               (if (procval? proc)
                                   (apply-procedure proc args)
                                   (eopl:error 'eval-expresion "Attempt to apply non-procedure ~s" proc))))
      )))


(define eval-expr-lista
  (lambda (expr env)
    (cases expr-lista expr
      (expr-lista-normal (prim lista) (apply-prim-lista prim lista env))
      (expr-lista-set (lista pos nuevo-valor) (eval-lista-a-modificar lista pos nuevo-valor env))
      )))

(define eval-lista-a-modificar
  (lambda (lista pos nuevo-valor env)
    (cases lista-a-modificar lista
      (lista-a-modificar-id (id) (let* ((vec (apply-env env id))
                                        (index (eval-rands (cons pos '()) env))
                                        (ref (apply-env-ref env id))
                                        (mutable (ismutable? ref)))
                                   (if mutable
                                       (vector-set! vec (car index) nuevo-valor)
                                       (eopl:error 'vector-set! "No se puede modificar la lista constante"))
                                   vec))
      (lista-a-modificar-lista (exp) (let ((lista (eval-rands-list (cons exp '()) env))
                                           (index (eval-rands (cons pos '()) env)))
                                       (vector-set! (car lista) (car index) nuevo-valor)
                                       (car lista)))
      )))

(define ismutable?
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mut) mut)
      )))

(define eval-rands-list
  (lambda (rands env)
    (map (lambda (x) (eval-rand-list x env)) rands)))

(define eval-rand-list
  (lambda (rand env)
    (eval-expr-lista rand env)))


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
      )))

;;************************************************************************************************************
;;apply-primitive-lista:
;;función que aplica la primitiva dada a las listas
(define apply-prim-lista
  (lambda (prim lista env)
    (cases prim-lista prim
      ;;Prim para preguntar si esta vacia la lista
      (prim-pregunta-vacioLista () (if (= 0 (vector-length (car (eval-rands lista env))))
                                       (eval-bool (bool-true))
                                       (eval-bool (bool-false))))
      ;;Prim para crear listas vacias
      ;(prim-crear-vacioLista () #())
      ;;Prim para crear listas
      (prim-crear-lista () (apply vector lista))
      ;;Prim para preguntar si es una lista
      (prim-pregunta-lista () (let ((args (eval-rands lista env))) 
                                (if (vector? (car args))
                                    (eval-bool (bool-true))
                                    (eval-bool (bool-false)))))
      ;;Prim para extraer la cabeza de la lista
      (prim-cabeza-lista () (let* ((args (eval-rands lista env))
                                  (vec (car args)))
                                  (eval-expresion (vector-ref vec 0) env)))
      ;;Prim para extraer la cola de la lista
      (prim-cola-lista () (let* ((args (eval-rands lista env))
                                  (vec (car args)))
                            (vector-cdr vec)))
      ;;Prim para adicionar elementos a una lista
      (prim-adicionar-lista () (let* ((args (eval-rands lista env))
                                      (vec (vector->list (car args)))
                                      (elem (if (vector? (cadr args)) (vector->list(cadr args)) (cadr args))))
                                  (append (eliminar_parentesis vec) (list elem))))
      ;;Prim para seleccionar un elemento de una lista
      (prim-ref-lista () (let* ((args (eval-rands lista env))
                                (vec (car args))
                                (index (cadr args)))
                           (eval-expresion (vector-ref vec index) env)))
      ;;Prim para cambiar un elemento de una lista
;;       (prim-set-lista () (let* ((idvec (car lista))
;;                                 (args (eval-rands lista env))
;;                                 (vec (car args))
;;                                 (index (cadr args))
;;                                 (nuevo-valor (caddr lista))
;;                                 ;(ref (apply-env-ref env idvec))
;;                                 )
;;                            (vector-set! vec index nuevo-valor)
;;                            env)) ;;Deberia extender un ambiente?
      )))

;;Funcion Auxiliar para listas
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

(define vector-cdr
  (lambda (v)
    (list->vector (cdr (vector->list v)))))
  
;;************************************************************************************************************
;;apply-primitive-tupla:
;;función que aplica la primitiva dada a las listas
(define apply-prim-tupla
  (lambda (prim tupla env)
    (cases prim-tupla prim
      ;;Prim para preguntar si esta vacia la tupla
      (prim-pregunta-vacioTupla () (if (= 0 (vector-length (car (eval-rands tupla env))))
                                       (eval-bool (bool-true))
                                       (eval-bool (bool-false))))
      ;;Prim para crear tupla vacias
      (prim-crear-vacioTupla () #())
      ;;Prim para crear tupla
      (prim-crear-tupla () (apply vector tupla))
      ;;Prim para preguntar si es una tupla
      (prim-pregunta-tupla () (let ((args (eval-rands tupla env))) 
                                (if (vector? (car args))
                                    (eval-bool (bool-true))
                                    (eval-bool (bool-false)))))
      ;;Prim para extraer la cabeza de la tupla
      (prim-cabeza-tupla () (let* ((args (eval-rands tupla env))
                                  (vec (car args)))
                                  (eval-expresion (vector-ref vec 0) env)))
      ;;Prim para extraer la cola de la tupla
      (prim-cola-tupla () (let* ((args (eval-rands tupla env))
                                  (vec (car args)))
                                  (eval-expresion (vector-ref vec (- (vector-length vec) 1)) env)))
      ;;Prim para seleccionar un elemento de una tupla
      (prim-ref-tupla () (let* ((args (eval-rands tupla env))
                                (vec (car args))
                                (index (cadr args)))
                           (eval-expresion (vector-ref vec index) env)))
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
   (env environment?)))

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
      (extended-env-record (syms vals sonmutables env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals (vector-ref sonmutables pos))
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

;;funciones auxiliares para aplicar eval-expresion a cada elemento de una 
;;lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))


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
;;y retorna un valor True o False)¡

(define eval-expr-bool
  (lambda (exp env)
    (cases expr-bool exp
      (expr-bool-exps (prim exp1 exp2) (let((args (eval-rands (list exp1 exp2) env)))
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

;;************************************************************************************************************

;; -- Referencias --

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (mutable? boolean?))) ;;Una referencia tendra un nuevo elemento booleano para saber si es mutable

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec mutable?)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

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