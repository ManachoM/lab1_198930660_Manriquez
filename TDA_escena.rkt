#lang racket
(require "TDA_personajes.rkt")
(require "TDA_equipo.rkt")
;; Este archivo contiene un Tipo de Dato Abstracto para el desarrollo del laboratorio 1
;; del ramo Paradigmas de Programacion. En particular, contiene el modelado de las escenas

;; Representacion: escena -> '(N M estado ptje equipo1 equipo2 tr)

;; Funcion constructora de escenas
;; Dominio: entero X entero X string X entero X equipo X equipo X lista de trayectoria (solo sera usada para playLazy, por lo que estara vacia en cualquier otro caso)
;; Recorrido: escena
(define (escena N M est ptje eq1 eq2 l) (
                                  if(and (> N 0) (> M 0) (string? est) (equipo? eq1) (equipo? eq2))
                                    (list N M est ptje eq1 eq2 l)
                                    null
                                    )
  )

;; Funcion de pertenencia de escenas
;; Dominio: cualquier cosa
;; Recorrido: boolean
(define (escena? s) (
                     if(and (number? (car s)) (number? (cadr s)) (> (car s) 0) (> (cadr s) 0) (number? (cadddr s)) (> (cadddr s) -1) (string? (caddr s)) (equipo? (car (cddddr s))) (equipo? (car (cdr (cddddr s)))))
                       #t
                       #f
                       )
  )

;; Funcion selectora de alto de escenas
;; Dominio: escena
;; Recorrido: entero
(define (getNescena s) (
                        if(escena? s)
                          (car s)
                          0)
  )

;; Funcion selectora de ancho de escenas
;; Dominio: escena
;; Recorrido: entero
(define (getMescena s) (
                        if(escena? s)
                          (cadr s)
                          0
                          )
  )

;; Funcion selectora de estado de escena
;; Dominio: escena
;; Recorrido: string
(define (getEstescena s) (
                          if(escena? s)
                            (caddr s)
                            null
                            )
  )

;; Funcion selectora de ptje de escena
;; Dominio: escena
;; Recorrido: entero
(define (getPtjescena s) (
                          if(escena? s)
                            (cadddr s)
                            null
                            )
  )

;; Funcion selectora de equipo 1 de escena
;; Dominio: escena
;; Recorrido: equipo
(define (getEq1escena s) (
                          if(escena? s)
                            (car (cddddr s))
                            null
                            )
 )

;; Funcion selectora de equipo 2 de escena
;; Dominio: escena
;; Recorrido: equipo
(define (getEq2escena s) (
                          if(escena? s)
                            (car (cdr (cddddr s)))
                            null
                            )
  )

;; Funcion selectora de elemento proyectil (para playLazy)
;; Dominio: escena
;; Recorrido: par
(define (getPescena s) (
                        if(escena? s)
                          (last s)
                          null
                          )
  )

;; Funcion modificadora de alto de escena
;; Dominio: escena X entero
;; Salida: escena
(define (setNescena s n) (
                        if(and (escena? s) (number? n) (> n -1))
                          (escena n (getMescena s) (getPtjescena s) (getEstescena s) (getEq1escena s) (getEq2escena s) (getPescena s))
                          s
                          )
  )


;; Funcion modificadora de ancho de escena
;; Dominio: escena X entero
;; Recorrido: escena
(define (setMescena s m) (
                          if(and (escena? s) (number? m) (> m -1))
                            (escena (getNescena s) m (getPtjescena s) (getEstescena s) (getEq1escena s) (getEq2escena s) (getPescena s))
                            s
                            )
  )

;; Funcion modificadora de puntaje de escena
;; Dominio: escena X entero
;; Recorrido: escena
(define (setPtjescena s p) (
                            if(and (escena? s) (number? p) (> p -1))
                              (escena (getNescena s) (getMescena s) p (getEstescena s) (getEq1escena s) (getEq2escena s) (getPescena s))
                              s
                              )
  )

;; Funcion modificadora de estado de escena
;; Dominio: escena X string
;; Recorrido: escena
(define (setEstescena s st) (
                             if(and (escena? s) (string? st))
                               (escena (getNescena s) (getMescena s) (getPtjescena s) st (getEq1escena s) (getEq2escena s) (getPescena s))
                               s
                               )
  )

;; Funcion modificadora de primer equipo de escena
;; Dominio: escena X equipo
;; Recorrido: escena
(define (setEq1escena s eq) (
                             if(and (escena? s) (equipo? eq))
                               (escena (getNescena s) (getMescena s) (getPtjescena s) (getEstescena s) eq (getEq2escena s) (getPescena s))
                               s
                               )
  )

;; Funcion modificadora de segundo equipo de escena
;; Dominio: escena X equipo
;; Recorrido: escena
(define (setEq2escena s eq) (
                             if(and (escena? s) (equipo? eq))
                               (escena (getNescena s) (getMescena s) (getPtjescena s) (getEstescena s) (getEq1escena s) eq (getPescena s))
                               s
                               )
  )

;; Funcion modificadora de par proyectil (solo playLazy)
;; Dominio: escena X pair
;; Recorrido: escena
(define (setPescena s l) (
                          if(escena? s)
                            (escena (getNescena s) (getMescena s) (getPtjescena s) (getEstescena s) (getEq1escena s) (getEq2escena s) l)
                            null
                            )
  )

(provide escena)
(provide escena?)
(provide getNescena)
(provide getMescena)
(provide getEstescena)
(provide getPtjescena)
(provide getEq1escena)
(provide getEq2escena)
(provide getPescena)
(provide setNescena)
(provide setMescena)
(provide setEstescena)
(provide setPtjescena)
(provide setEq1escena)
(provide setEq2escena)
(provide setPescena)