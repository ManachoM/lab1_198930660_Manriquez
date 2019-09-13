#lang racket
;;Este archivo contiene el Tipo de Dato Abstracto para el desarrollo
;;del laboratorio de paradigmas de programación. En particular, contendrá el TDA de
;;los personajes del equipo.


;;Funcion constructora de personajes
;;Dominio: Entero X Entero X Entero
;;Recorrido: personaje (entero)
(define (personaje X Y vida) (
                              if (and (> X 0)
                                      (> Y 0)
                                      (> vida 0)
                                      )
                                 (list X Y vida)
                                 0
                                 )
  )

;; Funcion de pertenencia de personajes
;; Dominio: cualquier cosa
;; Recorrido: booleano
(define (personaje? p) (
                        if(and
                          (> (car p) 0)
                          (> (car (cdr p)) 0)
                          (> (car (cdr (cdr p))) 0)
                          )
                          #t
                          #f)
  )


