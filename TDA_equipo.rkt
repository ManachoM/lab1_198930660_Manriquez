#lang racket
;; Este archivo contiene el Tipo de Dato Abstracto para un equipo.

(require "TDA_personajes.rkt")
;; Respresentacion: equipo -> '(flag.(list personajes))

;; Funcion constructora de equipo
;; Dominio: str X personajes
;; Recorrido: equipo
(define (equipo flag p) (
                      cons flag p
                           )
  )

;; Funcion de pertenencia de equipo
;; Dominio: cualquier cosa
;; Recorrido: boolean
(define (equipo? eq) (
                     if(and (string? (car eq)) (sonPersonaje (cdr eq)))
                       #t
                       #f
                       )
  )

