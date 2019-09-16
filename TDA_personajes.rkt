#lang racket
;;Este archivo contiene el Tipo de Dato Abstracto para el desarrollo
;;del laboratorio de paradigmas de programación. En particular, contendrá el TDA de
;;los personajes del equipo.

;;Representacion: personaje -> '(entero_posicionX entero_posicionY entero_vida)


;;Funcion constructora de personajes
;;Dominio: Entero X Entero X Entero
;;Recorrido: personaje (entero)
(define (personaje X Y vida) (
                              if (and (> X -1)
                                      (> Y -1)
                                      (> vida -1)
                                      )
                                 (list X Y vida)
                                 0
                                 )
  )

;; Funcion de pertenencia de personajes
;; Dominio: cualquier cosa
;; Recorrido: boolean
(define (personaje? p) (
                        if(and
                          (> (car p) -1)
                          (> (car (cdr p)) -1)
                          (> (car (cdr (cdr p))) -1)
                          )
                          #t
                          #f)
  )

;; Funcion selectora de posicion X de personajes
;; Dominio: personaje
;; Recorrido: entero, representa posicion X
(define (getXpersonaje p) (
                           if (personaje? p)
                               (car p)
                               null
                               )
                           )
 

;; Funcion selectora de posicion Y de personajes
;; Dominio: personaje
;; Recorrido: entero, representa posicion Y
(define (getYpersonaje p) (
                           if (personaje? p)
                               (car (cdr p))
                               null
                               )
                           
  )

;; Funcion selectora de vida de personajes
;; Dominio: personaje
;; Recorrido: entero, representa vida restante
(define (getVidapersonaje p) (
                              if(personaje? p)
                                 (car (cdr (cdr p)))
                                 null
                                 
                              )
  )

;; Funcion modificadora de posicion X de personaje
;; Dominio: personaje X entero
;; Recorrido: personaje
(define (setXpersonaje p x) (
                            if (and (personaje? p) (number? x) (> x -1))
                               (personaje x (getYpersonaje p) (getVidapersonaje p))
                               null
                               )
  )

;; Funcion modificadora de posicion Y de personaje
;; Dominio: personaje X entero
;; Recorrido: personaje
(define (setYpersonaje p y) (
                             if(and (personaje? p) (number? y) (> y -1))
                               (personaje (getXpersonaje p) y (getVidapersonaje p))
                               null
                               )
  )

;; Funcion modificadora de vida de personaje
;; Dominio: personaje X entero
;; Recorrido: personaje
(define (setVidapersonaje p v) (
                                if(and (personaje? p) (number? v) (> v -1))
                                  (personaje (getXpersonaje p) (getYpersonaje p) v)
                                  null
                                  )
  )


;; Funcion que determina si todos los elementos de una lista son personajes
;; Dominio: lista
;; Recorrido: boolean
;; Recursion: natural
(define (sonPersonaje p) (
                          if(null? p)
                            #t
                            (if(personaje? (car p))
                               (sonPersonaje (cdr p))
                               #f)
                            )
  )
(provide sonPersonaje)
(provide personaje)
(provide personaje?)
(provide getXpersonaje)
(provide getYpersonaje)
(provide getVidapersonaje)
(provide setXpersonaje)
(provide setYpersonaje)
(provide setVidapersonaje)