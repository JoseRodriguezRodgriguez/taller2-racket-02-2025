#lang racket
;;==================================
;;Taller 2 – Programación Declarativa
;;==================================

;;Ejercicio 1 – Contar elementos positivos en una lista
;;Objetivo: Usar filter y length para determinar cuántos elementos positivos contiene
;;una lista.
;;Entrada: Entrada: '(3 -2 7 0 -5 9).
;;Salida esperada: Salida esperada: 3 elementos positivos.
(define (contar-positivos lst)
  (length (filter positive? lst)))
(displayln (string-append "Ejercicio 1: " (number->string (contar-positivos '(3 -2 7 0 -5 9))) " elementos positivos"))

;;Ejercicio 2 – Generar lista de cuadrados pares
;;Objetivo: Practicar map y filter aplicados secuencialmente.
;;Entrada: Entrada: '(1 2 3 4 5 6 7 8).
;Salida esperada: Salida esperada: '(4 16 36 64).
(define (cuadrados-pares lst)
  (map (λ (x) (* x x)) (filter even? lst)))
(displayln (string-append "Ejercicio 2: " (format "~a" (cuadrados-pares '(1 2 3 4 5 6 7 8)))))

;;Ejercicio 3 – Calcular el factorial de un número
;;Objetivo: Implementar recursión simple en Racket.
;;Entrada: Entrada: n = 5.
;;Salida esperada: Salida esperada: 120.
(define (factorial n)
  (if (zero? n) 1 (* n (factorial (sub1 n)))))
(displayln (string-append "Ejercicio 3: " (format "~a" (factorial 5))))

;;Ejercicio 4 – Elevar cada número al cubo
;;Objetivo: Aplicar funciones lambda dentro de map.
;;Entrada: Entrada: '(2 3 4).
;;Salida esperada: Salida esperada: '(8 27 64).
(define (cubos lst)
  (map (λ (x) (* x x x)) lst))
(displayln (string-append "Ejercicio 4: " (format "~a" (cubos '(2 3 4)))))

;;Ejercicio 5 – Sumar todos los elementos impares
;;Objetivo: Usar filter y foldl combinados para obtener una suma condicional.
;;Entrada: Entrada: '(1 2 3 4 5 6 7).
;;Salida esperada: Salida esperada: 16.
(define (suma-impares lst)
  (foldl + 0 (filter odd? lst)))
(displayln (string-append "Ejercicio 5: " (format "~a" (suma-impares '(1 2 3 4 5 6 7)))))

;;Ejercicio 6 – Determinar si una lista contiene números negativos
;;Objetivo: Usar any oormap con una función lambda para verificar condiciones.
;;Entrada: Entrada: '(5 9 -3 2).
;;Salida esperada: #t (contiene negativos)
(define (contiene-negativos? lst)
  (ormap (λ (x) (< x 0)) lst))
(displayln (string-append "Ejercicio 6: " (format "~a" (contiene-negativos? '(5 9 -3 2)))))

;;Ejercicio 7 – Calcular la suma acumulada de una lista
;;Objetivo: Reforzar el uso de foldl para crear acumuladores personalizados.
;;Entrada: Entrada: '(1 2 3 4).
;;Salida esperada: Salida esperada: '(1 3 6 10).
(define (suma-acumulada lst)
  (reverse
   (cdr
    (foldl (λ (x acc)
             (let* ([s (+ x (car acc))]
                    [out (cdr acc)])
               (cons s (cons s out))))
           (cons 0 '())
           lst))))

(displayln (string-append "Ejercicio 7: " (format "~a" (suma-acumulada '(1 2 3 4)))))

;;Ejercicio 8 – Concatenar cadenas de texto en una lista
;;Objetivo: Practicar foldl con operaciones sobre cadenas.
;;Entrada: Entrada: '("Hola" " " "Mundo").
;;Salida esperada: Salida esperada: "Hola Mundo".
;; Ejercicio 8 – Concatenar cadenas de texto en una lista
(define (concatenar-cadenas lst)
  (foldl (λ (x acc) (string-append acc x)) "" lst))

(displayln (string-append "Ejercicio 8: " (concatenar-cadenas '("Hola" " " "Mundo"))))

;;Ejercicio 9 – Generar lista con el doble de los números mayores que 5
;;Objetivo: Combinar map y filter con condiciones lógicas.
;;Entrada: Entrada: '(3 6 8 2 10).
;;Salida esperada: Salida esperada: '(12 16 20).
(define (doble-mayores-5 lst)
  (map (λ (x) (* 2 x)) (filter (λ (x) (> x 5)) lst)))
(displayln (string-append "Ejercicio 9: " (format "~a" (doble-mayores-5 '(3 6 8 2 10)))))

;;Ejercicio 10 – Invertir el orden de una lista
;;Objetivo: Usar foldl o recursión para invertir el orden de los elementos.
;;Entrada: Entrada: '(1 2 3 4).
;;Salida esperada: Salida esperada: '(4 3 2 1).
(define (invertir lst)
  (foldl (λ (x acc) (cons x acc)) '() lst))
(displayln (string-append "Ejercicio 10: " (format "~a" (invertir '(1 2 3 4)))))

;;Ejercicio 11 – Crear una función que reciba una función como parámetro
;;Objetivo: Comprender la naturaleza de las funciones de orden superior.
;;Entrada: Entrada: Función cuadrado y lista '(1 2 3 4).
;;Salida esperada: Salida esperada: '(1 4 9 16).
(define (aplicar-a-lista f lst)
  (map f lst))
(define (cuadrado x) (* x x))
(displayln (string-append "Ejercicio 11: " (format "~a" (aplicar-a-lista cuadrado '(1 2 3 4)))))

;;Ejercicio 12 – Reto integrador: combinar múltiples funciones
;;Objetivo: Usar map, filter y foldl en un mismo programa para calcular el promedio de
;;los números mayores a 5 en una lista.
;;Entrada: Entrada: '(3 8 10 4 9 2 7).
;;Salida esperada: Salida esperada: 8.5.
(define (promedio-mayores-5 lst)
  (let* ([filtrados (filter (λ (x) (> x 5)) lst)]
         [suma (foldl + 0 filtrados)]
         [n (length filtrados)])
    (if (zero? n) +nan.0 (exact->inexact (/ suma n)))))
(displayln (string-append "Ejercicio 12: " (format "~a" (promedio-mayores-5 '(3 8 10 4 9 2 7)))))