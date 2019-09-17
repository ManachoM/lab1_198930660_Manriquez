#lang racket
(require "TDA_personajes.rkt")
(require "TDA_equipo.rkt")
;; Este archivo contiene un Tipo de Dato Abstracto para el desarrollo del laboratorio 1
;; del ramo Paradigmas de Programacion. En particular, contiene el modelado de las escenas

;; Representacion: escena -> '(N M estado equipo1 equipo2)

;; Funcion constructora de escenas
;; Dominio: entero X entero X string X equipo X equipo
;; Recorrido: escena
(define (escena N M est eq1 eq2) (
                                  if(and (> N 0) (> M 0) (string? est) (equipo? eq1) (equipo? eq2))
                                    (list N M est eq1 eq2)
                                    null
                                    )
  )

;; Funcion de pertenencia de escenas
;; Dominio: cualquier cosa
;; Recorrido: boolean
(define (escena? s) (
                     if(and (number? (car s)) (number? (cadr s)) (> (car s) 0) (> (cadr s) 0) (string? (caddr s)) (equipo? (cadddr s)) (equipo? (car (cddddr s))))
                       #t
                       #f
                       )
  )

;; Funcion selectora de alto de escenas
;; Domiino: escena
;; Recorrido: entero
(define (getNescena s) (
                        if(equipo? s)
                          (car s)
                          0)
  )

                          