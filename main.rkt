#lang racket
;; Este archivo contiene los requerimientos funcionales y las funciones asociadas.
(require "TDA_personajes.rkt")
(require "TDA_equipo.rkt")
(require "TDA_escena.rkt")

;; Funcion que inicializa lista de personajes del jugador para el desarrollo de una partida
;; Dominio: entero X entero X entero
;; Recorrido: lista de personajes
;; Recursion: natural o lineal
(define (initListaP1 cant fila col) (
                                       if(= cant 1)
                                         (cons (personaje fila cant 1) null)
                                         (cons (personaje fila col 1) (initListaP1 (- cant 1) fila (- col 1)))
                                         )
  )


;; Funcion que inicializa lista de equipo del jugador para el desarrollo de una partida
;; Dominio: string X entero X entero X entero
;; Recorrido: equipo
(define (initEquipoJ fl cant fila col) (
                               if(string? fl)
                                 (equipo fl (initListaP1 cant fila col))
                                 (equipo "invalid" (list (personaje 0 0 0))))
  )

;; Funcion que inicializa lista de personajes del computador para el desarrollo de una partida
;; Dominio: entero X entero X entero
;; Recorrido: lista de personajes
;; Recursion: natural o lineal
(define (initListaP2 ini fila col) (
                                       if(= ini col)
                                         (cons (personaje fila ini 1) null)
                                         (cons (personaje fila ini 1) (initListaP2 (+ ini 1) fila col))
                                         )
  )

;; Funcion que inicializa lista de equipo del computador para el desarrollo de una partida
;; Dominio: string X entero X entero X entero
;; Recorrido: equipo
(define (initEquipoC fl ini fila col) (
                               if(string? fl)
                                 (equipo fl (initListaP2 ini fila col))
                                 (equipo "invalid" (list (personaje 0 0 0))))
  )

;; Funcion que crea un escenario para el desarrollo de una partida
;; Dominio: entero X entero X entero X entero X entero
;; Recorrido: escena
(define (createScene N M E D seed) (
                                    if(and (number? N) (> N 0) (number? M) (> M 0) (number? E) (> E 0) (number? D) (> D 0) (number? seed) (> seed 0) (>= N seed))
                                      (escena N M "PLAYING" (initEquipoJ "Jugador" 3 (modulo seed N) 3) (initEquipoC "CPU" (- M (- E 1)) (modulo seed N) M))
                                      null
                                      )
  )

;; Funcion que agrega un elemento al final de una lista
;; Dominio: elemento X lista
;; Recorrido: lista
;; Recursion: natural o lineal
(define (addLast l list) (
                          if(empty? list)
                            (cons l null)
                            (cons (car list) (addLast l (cdr list)))
                            )
  )


;; Funcion que entrega todas las coordenadas X de personajes en un equipo
;; Dominio: lista de personajes X lista de enteros
;; Recorrido: lista de enteros
(define (getAllX eq list) (
                           if(empty? eq)
                             list
                             (getAllX (cdr eq) (addLast (getXpersonaje (car eq)) list))
                             )
  )
                           