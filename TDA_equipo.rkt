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

;; Funcion selectora de bandera de equipo
;; Dominio: equipo
;; Recorrido: string
(define (getFlequipo p) (
                           if(equipo? p)
                             (car p)
                             "No es equipo"
                             )
  )

;; Funcion selectora de lista de personajes del equipo
;; Dominio: equipo
;; Recorrido: lista de personajes
(define (getPequipo p) (
                        if(equipo? p)
                          (cdr p)
                          null
                          )
  )

;; Funcion modificadora de bandera de equipo
;; Dominio: equipo X str
;; Recorrido: equipo
(define (setFlequipo eq fl) (
                             if(and (equipo? eq) (string? fl))
                               (equipo fl (getPequipo eq))
                               null
                               )
  )

;; Funcion modificadora de lista de personajes
;; Dominio: equipo X lista (de personajes)
;; Recorrido: equipo
(define (setPequipo eq p) (
                           if(and (equipo? eq) (list? p) (sonPersonaje p))
                             (equipo (getFlequipo eq) p)
                             null)
  )


(provide equipo)
(provide equipo?)
(provide getFlequipo)
(provide getPequipo)
(provide setFlequipo)
(provide setPequipo)
