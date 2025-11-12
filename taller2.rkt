#lang racket

; ejercicio 1

(define (contar-positivos lista)
  (length (filter (lambda (x) (positive? x)) lista)))

(displayln (contar-positivos '(3 -2 7 0 -5 9)))


; ejercicio 2
(define (lista-cuadrados lista)
  (let* (
         (pares (filter (lambda (x) (even? x)) lista))
         (cuadrados (map (lambda (x) (* x x)) pares))
         )
    cuadrados))

(displayln (lista-cuadrados '(1 2 3 4 5 6 7 8)))

; ejercicio 3

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(displayln (factorial 5))


; ejercicio 4
(define (cubo lista)
  (map (lambda (x) (* x x x)) lista))


(displayln (cubo '(2 3 4)))

 ; ejercicio 5

(define (sumar-impares lista)
  (let* (
         (impares (filter (lambda (x) (odd? x)) lista))
         (suma (foldl + 0 impares))
         )
    suma))

(displayln (sumar-impares '(1 2 3 4 5 6 7)))

; ejercicio 6

(define (contiene-negativos lista)
  (ormap (lambda (x) (< x 0)) lista))

(displayln (contiene-negativos '(5 9 -3 2)))

; ejercicio 7

(define (suma-acumulada lista)
  (reverse
   (cdr
    (foldl
     (lambda (x acc)
       (cons (+ x (car acc)) acc))
     '(0)
     lista))))

(displayln (suma-acumulada '(1 2 3 4)))

; ejercicio 8

(define (concatenar-cadenas lista)
  (foldr string-append "" lista))


(concatenar-cadenas '("Hola" " " "Mundo"))

; ejercicio 9

(define (lista-mayores-5 lista)
  (let* (
         (mayores (filter (lambda(x) (> x 5)) lista))
         (doble (map (lambda (x) (* x 2)) mayores))
         )
    doble))

(displayln (lista-mayores-5 '(3 6 8 2 10)))


; ejercicio 10

(define (invertir-lista lista)
  (foldl (lambda (x acc) (cons x acc)) '() lista))


(invertir-lista '(1 2 3 4))


; ejercicio 11

(define (aplicar-funcion f lista)
  (map f lista))

(define (cuadrado x) (* x x))
(aplicar-funcion cuadrado '(1 2 3 4))

; ejercicio 12

(define (promedio-mayores-que-5 lista)
  (let* ((mayores (filter (lambda (x) (> x 5)) lista))
         (suma (foldl + 0 mayores)))
    (/ suma (length mayores))))

(promedio-mayores-que-5 '(3 8 10 4 9 2 7))