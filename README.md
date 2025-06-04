# Mini_Py

**Mini_Py** es un lenguaje de programación desarrollado en Racket como parte de un proyecto académico del curso FUNDAMENTOS DE INTERPRETACIÓN Y COMPILACIÓN DE LENGUAJES DE PROGRAMACIÓN basado en el libro *Essentials of Programming Languages (EOPL)*. El objetivo del lenguaje es explorar conceptos fundamentales de semántica operacional, análisis léxico, y construcción de interpretes mediante un diseño propio que toma inspiración de Python.

## Descripción General

Mini_Py es un lenguaje expresivo con características tanto funcionales como imperativas. Incorpora elementos típicos de lenguajes modernos como funciones de orden superior, expresiones booleanas, estructuras de datos como listas y tuplas, control de flujo con `if`, `while` y `for`, y mecanismos para trabajar con circuitos lógicos.

Se diseñó una gramática formal junto con una especificación léxica y sintáctica para representar el comportamiento del lenguaje en un interprete implementado en Racket.

## Características del Lenguaje

- Soporte para expresiones numéricas, booleanas y de texto
- Definiciones de variables (`var`, `const`)
- Funciones anónimas y recursivas (`proc`, `rec`)
- Control de flujo (`if`, `while`, `for`)
- Operaciones primitivas aritméticas, booleanas y de texto
- Manipulación de estructuras de datos: listas, tuplas y registros
- Soporte para circuitos lógicos mediante primitivas especiales
- Modelo de objetos con invocación de métodos (`new`, `ejecutar`, `super`)
- Impresión por consola y soporte para evaluación múltiple (`begin`)
- Conversión entre listas y tuplas

## Gramática

La gramática está compuesta por las siguientes construcciones principales:

- **Programa:** Un programa consiste en una sola expresión.
- **Expresiones:** Incluyen números, identificadores, hexadecimales, cadenas, booleanos, listas, tuplas, registros, funciones, llamadas a funciones, estructuras de control, impresiones, etc.
- **Primitivas:** Se definen operaciones básicas como `+`, `-`, `*`, `/`, `add1`, `len`, `concat`, así como primitivas para manipulación de circuitos (`eval-circuit`, `merge-circuit`, `connect-circuits`).
- **Expresiones Booleanas:** Comparaciones (`<`, `>`, `==`), operadores lógicos (`and`, `or`, `not`) y valores booleanos (`True`, `False`).
- **Listas y Tuplas:** Soporte para creación, acceso y modificación de listas y tuplas.
- **Registros:** Estructura similar a objetos o diccionarios (en construcción).
- **Circuitos:** Modelado de puertas lógicas (`and`, `or`, `not`, `xor`) y su interconexión.

## Especificación Léxica

La especificación léxica define cómo se reconocen los distintos tokens del lenguaje:

- **Espacios en blanco** y **comentarios** son ignorados.
- **Identificadores** permiten letras, números, guiones bajos y signos de interrogación.
- **Números** pueden ser enteros o flotantes, positivos o negativos.
- **Cadenas** se delimitan por comillas dobles (`"`).

## Implementación

El lenguaje fue implementado en Racket utilizando técnicas de parsing y evaluación definidas en *EOPL*. Se estructuró el proyecto utilizando definiciones claras de escáner, gramática, y funciones de interpretación, permitiendo la evaluación directa de programas escritos en Mini_Py.

## Créditos

Este proyecto fue desarrollado por un grupo de estudiantes de la Universidad del Valle como parte del curso de lenguajes de programación, tomando como base conceptual el libro *Essentials of Programming Languages*.

- Fernando Cardona Giraldo - 2241381
- Juan Manuel Vargas - 
- Pablo Esteban Becerra - 2243506

## Estado del Proyecto

Mini_Py actualmente cumple con todos los parametros establecidos en la descripcion del proyecto del curso.

## Requisitos

- Racket
- Conocimientos básicos en semántica formal y construcción de lenguajes

## Ejecución

Para ejecutar un programa en Mini_Py:

1. Abrir el archivo principal del intérprete en DrRacket
2. Ejecutar la funcion (interpretador) desde la consola de DrRacket
3. Escribir una expresión válida siguiendo la gramática.
4. Ejecutar el intérprete para ver el resultado.

---

Este proyecto representa un esfuerzo por entender y aplicar los principios fundamentales del diseño de lenguajes de programación.
