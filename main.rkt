#lang racket
;; Este archivo contiene los requerimientos funcionales y las funciones asociadas.
(require "TDA_personajes.rkt")
(require "TDA_equipo.rkt")
(require "TDA_escena.rkt")




;; Funcion que retorna el enesimo elemento de una lista
;; Dominio: entero X lista
;; Recorrido: elemento
;; Recursion: de cola, no es necesario dejar estados pendientes
(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l))
      )
    )
  )


;; Funcion que inicializa lista de personajes del jugador para el desarrollo de una partida
;; Dominio: entero X entero X entero
;; Recorrido: lista de personajes
;; Recursion: natural o lineal, intuitiva, facil de implementar
(define (initListaP1 cant fila col) (
                                       if(= cant 1)
                                         (cons (personaje cant fila 1) null)
                                         (cons (personaje col fila 1) (initListaP1 (- cant 1)  fila (- col 1)))
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
;; Recursion: natural o lineal, facilidad de implementacion
(define (initListaP2 ini fila col) (
                                       if(= ini col)
                                         (cons (personaje ini fila 1) null)
                                         (cons (personaje ini fila 1) (initListaP2 (+ ini 1) fila col))
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
                                    if(and (number? N) (> N 0) (number? M) (> M 0) (number? E) (> E 0) (number? D) (> D 0) (number? seed) (> seed 0)) ;(>= N seed);)
                                      (escena N M "PLAYING" 0 (initEquipoJ "Jugador" 3 (modulo seed N) 3) (initEquipoC "CPU" (- M (- E 1)) (modulo seed N) M))
                                      null
                                      )
  )
;(createScene 10 10 1 4 1001)

;; Funcion que agrega un elemento al final de una lista
;; Dominio: elemento X lista
;; Recorrido: lista
;; Recursion: natural o lineal, facilidad de implementacion
(define (addLast l list) (
                          if(empty? list)
                            (cons l null)
                            (cons (car list) (addLast l (cdr list)))
                            )
  )


;; Funcion que entrega todas las coordenadas X de personajes en un equipo
;; Dominio: lista de personajes X lista de enteros
;; Recorrido: lista de enteros
;; Recursion: de cola, permite desarrollo versatil de la funcion. Permitira entregar una lista con elementos ya conocidos para seguir agregandole valores
(define (getAllX eq list) (
                           if(empty? eq)
                             list
                             (getAllX (cdr eq) (addLast (getXpersonaje (car eq)) list))
                             )
  )

;; Funcion que verifica si se repite algun elemento en una lista
;; Dominio: lista
;; Recorrido: boolean
;; Recursion: de cola, no deja estados pendientes. Permite avanzar por la lista de entrada sin procuparse por la cantidad de argumentos
(define (seRepite lista) (
                          if(empty? lista)
                          #f
                          (if(boolean? (member (car lista) (cdr lista)))
                             (seRepite (cdr lista))
                             #t
                             )
                          )
  )

;; Funcion que verifica que un escenario sea valido para el desarrollo de una partida
;; Dominio: escena
;; Recorrido: boolean
(define (checkScene scene) (
                            if(escena? scene);; Al preguntar si es escena verificamos que las dimensiones sean coherentes y los tipos de dato tambien
                               (if(and (or (eqv? (getEstescena scene) "PLAYING") (eqv? (getEstescena scene) "VICTORY") (eqv? (getEstescena scene) "DEFEAT") (eqv? (getEstescena scene) "DRAW"));; Luego verificamos si el estado del escenario es valido
                                       (>= (getMescena scene) (+ (length (getPequipo (getEq1escena scene))) (length (getPequipo (getEq2escena scene)))));; Vemos si las dimensiones son suficientes para contener a todos los personajes
                                       (not (seRepite (getAllX (getPequipo (getEq1escena scene)) (getAllX (getPequipo (getEq2escena scene)) '()))));; Vemos si hay posiciones repetidas. Se verifican solo los valores X porque se generan los personajes en una fila india
                                      )
                                  #t
                                  #f
                                  )
                               #f
                               )
  )

;; Funcion que mueve un personaje de una lista en el eje X
;; Dominio: lista de personajes X entero X entero
;; Recorrido: lista de personajes
(define (moveP eq n m) (
                        if(null? eq)
                          eq
                          (cons
                           (if(zero? n)
                              (setXpersonaje (car eq) (+ (getXpersonaje (car eq)) m))
                              (car eq))
                           (moveP (cdr eq) (- n 1) m)
                           )
                          )
  )

;; Funcion que modifica la vida de un personaje en una lista
;; Dominio: lista de personajes X entero
;; Recorrido: lista de personajes
;; Recursion: natural, es facil de implementar
(define (hitP1 eq n) (
                     if(null? eq)
                       eq
                       (cons
                        (if(zero? n)
                           (setVidapersonaje (car eq) 0)
                           (car eq))
                        (hitP1 (cdr eq) (- n 1))
                            )
                       )
  )

;; Funcion que describe trayectoria de un proyectil
;; Dominio: entero X entero X entero X entero
;; Recorrido: lista de numeros
;; Recursion: natural, facil de programar para generar las listas de trayectorias
(define (shoot1 angle x y M)
                              (define (generarLista1 n y) (if(zero? n)
                                                          (cons (+ n y) null)
                                                          (cons (+ n y) (generarLista1 (- n 1) y))
                                                          )
                                )
                              (define (generarLista2 n y) (if(zero? n)
                                                             (cons y null)
                                                             (cons y (generarLista2 (- n 1) y))
                                                             )
                                )
                              (define (generarLista3 n y) (if(zero? n)
                                                             (cons y null)
                                                             (cons y (generarLista3 (- n 1) (+ y 1)))
                                                             )
                                )
  (
                              if(and (number? angle) (number? x) (number? y) (number? M) (< 0 x) (< 0 y) (< 0 M) (< -1 angle))
                                (cond ((= (quotient angle 45) 1) (reverse (generarLista1 (- M x) y)))
                                      ((or (= (quotient angle 45) 0)  (= (quotient angle 45) 8)) (generarLista2 (- M x) y))
                                      ((= (quotient angle 45) 2) (cons (generarLista3 (- M x) y) null))
                                      ((= (quotient angle 45) 3) (generarLista1 x y))
                                      ((= (quotient angle 45) 4) (generarLista2 x y))
                                      (else '())
                                      )
                                '()
                                )
  )

;; Funcion que determina si un personaje esta en el camino de la trayectoria
;; Dominio: lista de enteros X lista de personajes X entero
;; Recorrido: lista de personajes
(define (chocarProyectil tr eq x angle) (define (aux1 member tr x angle) (
                                                                          cond [(or (< (quotient angle 45) 2) (= (quotient angle 45) 8)) (if(and (> (- (getXpersonaje member) x) -1) (< (- (getXpersonaje member) x) (length tr)))
                                                                                                 (if(and (= (nth (- (getXpersonaje member) x) tr) (getYpersonaje member)) (= (getVidapersonaje member) 1))
                                                                                                  #t
                                                                                                  #f
                                                                                              )
                                                                                               #f)]
                                                                                [(> (quotient angle 45) 2) (if(and (> (- x (getXpersonaje member)) -1) (< (- x (getXpersonaje member)) (length tr)))
                                                                                                     (if(= (nth (- x (getXpersonaje member)) tr) (getYpersonaje member))
                                                                                                        #t
                                                                                                        #f
                                                                                                        )
                                                                                                    #f)]
                                                                                [(= (quotient angle 45) 2) #f]
                                                                                )
                                                                          )
                                     (define (aux2 tr eq x n angle) (
                                                                     if(= (quotient angle 45) 2)
                                                                       -1
                                                                       (if(or (>= n (length eq)) (>= n (length tr)))
                                                                       -1
                                                                       (if(aux1 (nth n eq) tr x angle)
                                                                          n
                                                                          (aux2 tr eq x (+ n 1) angle)
                                                                          )
                                                                       )
                                        )
                                       )
                                    (
                                     if(empty? tr)
                                       eq
                                       (if(and (> (aux2 tr eq x 0 angle) -1) (> x -1))
                                          (hitP1 eq (aux2 tr eq x 0 angle))
                                          eq
                                          )
                                       )
  )
  
                                    
;;  (chocarProyectil (shoot1 46 2 2 5) (list (personaje 1 2 1) (personaje 3 2 1) (personaje 4 2 1)) 2 46)

;; Función que determina cuántos personajes de una lista quedan con vida
;; Dominio: lista de personajes
;; Recorrido: boolean
;; Recursión: de cola, no es necesario dejar estados pendientes
(define (quedanVivos eq) (
                           if(empty? eq)
                             #f
                             (if(= (getVidapersonaje (car eq)) 1)
                                #t
                                (quedanVivos (cdr eq))
                                )
                             )
  )

;; Funcion que calcula cuantos personajes muertos hay en un equipo
;; Dominio: lista de personajes
;; Recorrido: entero
;; Recursion: natural, implementacion sencilla y requiere menos parametros
(define (cuantosMuertos eq) (
                             if(empty? eq)
                               0
                               (if (= (getVidapersonaje (car eq)) 0)
                                   (+ 1 (cuantosMuertos (cdr eq)))
                                   (+ 0 (cuantosMuertos (cdr eq)))
                                   )
                               )
  )

;; Funcion que determina el ptje de una partida a partir de dos listas de personajes
;; Dominio: lista de personajes X lista de personajes
;; Recorrido: entero
(define (checkScore eq1 eq2) (
                              - (cuantosMuertos eq1) (cuantosMuertos eq2)
                              )
  )

;; Función que determina el estado de juego con dos equipos dados
;; Dominio:equipo X equipo
;; Recorrido: string
(define (checkState eq1 eq2) (
                              cond [(and (quedanVivos (getPequipo eq1)) (quedanVivos (getPequipo eq2))) "PLAYING"]
                                   [(and (quedanVivos (getPequipo eq1)) (not (quedanVivos (getPequipo eq2)))) "VICTORY"]
                                   [(and (not (quedanVivos (getPequipo eq1))) (quedanVivos (getPequipo eq2))) "DEFEAT"]
                                   [(and (not (quedanVivos (getPequipo eq1))) (not (quedanVivos (getPequipo eq2)))) "DRAW"]
                                   )
  )

;; Funcion que determina posicion de disparo enemigo
;; Dominio: lista de personajes
;; Recorrido: entero
;; Recursion: de cola, no es necesario dejar estados pendientes
(define (quienDispara eq) (
                          if(null? eq)
                            -1
                            (if(= (getVidapersonaje (car eq)) 1)
                               (getXpersonaje (car eq))
                               (quienDispara (cdr eq))
                               )
                            )
  )
                             


;; Funcion que permite realizar una jugada, que consiste en mover un miembro del equipo, setear un angulo y disparar
;; Luego, el computador dispara de vuelta y se verifica el estado de la partida
;; Dominio: escena X entero X entero X funcion X entero X entero
;; Recorrido: escena
(define (play scene) (
                      lambda (member) (
                                       lambda (move) (
                                                      lambda (tf) (
                                                                   lambda (angle) (
                                                                                   lambda (seed) (
                                                                                                  if(and (escena? scene) (number? member) (> member -1) (number? move) (number? angle) (number? seed))
                                                                                                    (escena
                                                                                                     (getNescena scene) ;; Heredamos las dimensiones de la escena de entrada
                                                                                                     (getMescena scene) 
                                                                                                     (checkState (equipo (getFlequipo (getEq1escena scene)) ;; Generamos el estado de juego verificando las listas de personajes resultantes
                                                                                                                                       (chocarProyectil
                                                                                                                                       (tf (modulo seed 360) (quienDispara (getPequipo (getEq2escena scene))) (getYpersonaje (car (getPequipo (getEq2escena scene)))) (getMescena scene))
                                                                                                                                       (moveP (getPequipo (getEq1escena scene)) member move)
                                                                                                                                       (quienDispara (getPequipo (getEq2escena scene)))
                                                                                                                                       (modulo seed 360)
                                                                                                                                       )
                                                                                                                                      )
                                                                                                                         
                                                                                                                 (equipo (getFlequipo (getEq2escena scene)) 
                                                                                                                         (chocarProyectil
                                                                                                                          (tf angle (+ (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) move) (getYpersonaje (nth member (getPequipo (getEq1escena scene)))) (getMescena scene))
                                                                                                                          (getPequipo (getEq2escena scene))
                                                                                                                          (+ (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) move)
                                                                                                                          angle
                                                                                                                          )   
                                                                                                                         )
                                                                                                                 )
                                                                                                     (checkScore (chocarProyectil
                                                                                                                          (tf angle (+ (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) move) (getYpersonaje (nth member (getPequipo (getEq1escena scene)))) (getMescena scene))
                                                                                                                          (getPequipo (getEq2escena scene))
                                                                                                                          (+ (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) move)
                                                                                                                          angle
                                                                                                                          )
                                                                                                                 (chocarProyectil
                                                                                                                                       (tf (modulo seed 360) (quienDispara (getPequipo (getEq2escena scene))) (getYpersonaje (car (getPequipo (getEq2escena scene)))) (getMescena scene))
                                                                                                                                       (moveP (getPequipo (getEq1escena scene)) member move)
                                                                                                                                       (quienDispara (getPequipo (getEq2escena scene)))
                                                                                                                                       (modulo seed 360)
                                                                                                                                       )
                                                                                                                 )
                                                                                                                 
                                                                                                     (equipo ;; Definimos el primer equipo con el flag del mismo de la escena de entrada
                                                                                                      (getFlequipo (getEq1escena scene))
                                                                                                      (chocarProyectil  ;; Definimos la lista de personajes como la lista resultante de intersectar el disparo enemigo con la lista del jugador con el miembro desplazado
                                                                                                       (tf (modulo seed 360) (quienDispara (getPequipo (getEq2escena scene))) (getYpersonaje (car (getPequipo (getEq2escena scene)))) (getMescena scene))
                                                                                                       (moveP (getPequipo (getEq1escena scene)) member move)
                                                                                                       (quienDispara (getPequipo (getEq2escena scene)))
                                                                                                       (modulo seed 360)
                                                                                                       )
                                                                                                      )
                                                                                                     (equipo  ;; Lo mismo para el equipo CPU,
                                                                                                      (getFlequipo (getEq2escena scene)) ;; Heredamos flag 
                                                                                                      (chocarProyectil ;; Definimos la lista de personajes como la interseccion del disparo del jugador desde la nueva posicion con la lista de personajes enemigos
                                                                                                       (tf angle (+ (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) move) (getYpersonaje (nth member (getPequipo (getEq1escena scene)))) (getMescena scene))
                                                                                                       (getPequipo (getEq2escena scene))
                                                                                                       (+ (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) move)
                                                                                                       angle
                                                                                                       )
                                                                                                      )
                                                                                                     )
                                                                                                    scene
                                                                                                    )
                                                                                    )
                                                                    )
                                                       )
                                        )
                       )
  )
  

;; Funcion que retorna un string de las dimensiones, el estado y el puntaje de un escenario
;; Dominio: escena
;; Recorrido: string
(define (headerStr scene) (
                           if(escena? scene)
                             (string-append "Filas: " (number->string (getNescena scene)) "\n" "Columnas: " (number->string (getMescena scene)) "\n" "Estado: " (getEstescena scene) "\n" "Puntaje: " (number->string (getPtjescena scene)) "\n")
                             ""
                             )
  )

;; Funcion que verifica que si existe un personaje en un punto
;; Dominio: entero X entero X lista de personajes
;; Recorrido: boolean
;; Recursion: de cola, no es necesario dejar estados pendientes y es fácil de implementar
(define (estaPersonaje x y eq) (
                                if(or (empty? eq) (<= 0 x) (<= 0 y))
                                  #f
                                  (if(and (= x (getXpersonaje (car eq))) (= y (getYpersonaje (car eq))) (> (getVidapersonaje (car eq)) 0))
                                     #t
                                     (estaPersonaje x y (cdr eq))
                                     )
                                  )
  )

;; Funcion que verifica si existe un proyectil en un punto
;; Domimio: entero X entero X lista con proyectil
;; Recorrido: boolean
;; Recursión: de cola, no es necesario dejar estados pendientes
(define (estaProy x y l) (
                          if(or (empty? l) (<= 0 x) (<= 0 y))
                            #f
                            (if (and (= x (car l)) (= y (cdr l)))
                                #t
                                #f
                                )
                            )
  )

;; Función que determina qué símbolo va en un punto
;; Dominio: entero X entero X lista con proyectil X lista con personajes X lista con personajes
;; Recorrido: string
(define (queLetra x y l eq1 eq2) (
                                  cond [(and (estaProy x y l) (not (estaPersonaje x y eq1)) (not (estaPersonaje x y eq2))) "*"]
                                       [(and (estaProy x y l) (or (estaPersonaje x y eq1) (estaPersonaje x y eq2))) "X"]
                                       [(and (not (estaProy x y l)) (estaPersonaje x y eq1) (not (estaPersonaje x y eq2))) "P"]
                                       [(and (not (estaProy x y l)) (not (estaPersonaje x y eq1)) (estaPersonaje x y eq2)) "E"]
                                       [else " "]
                                       )
  )
                                       
;; Funcion que genera string con suelo
;; Dominio: escena X entero
;; Recorrido: string
(define (getSuelo scene i) (
                            if(= i (getYpersonaje (car (getPequipo (getEq1escena scene)))))
                              (string-append (make-string (getMescena scene) #\-) "\n")
                              ""
                              )
  )



;; Funcion que transforma el tablero de juego a un string
;; Dominio: escena X entero X entero
;; Recorrido: string
(define (bodyStr scene i j) (
                             if(or (< (getNescena scene) i) (< (getMescena scene) j))
                               ""
                               (if(= (remainder j (getMescena scene)) 0)
                                  (string-append (queLetra j i (last scene) (getPequipo (getEq1escena scene)) (getPequipo (getEq2escena scene))) " \n" (getSuelo scene i) (bodyStr scene (+ i 1) 1))
                                  (string-append (queLetra j i (last scene) (getPequipo (getEq1escena scene)) (getPequipo (getEq2escena scene))) (bodyStr scene i (+ j 1)))
                                  )
                               )
  )


(define S1 (list 10 10 "VICTORY" 1 (list "Jugador" (list (personaje 3 8 1) (personaje 2 8 1) (personaje 4 8 1))) (list "CPU" (list (personaje 10 8 0))) null))
(bodyStr S1 1 1)
((((((play S1) 2) 3) shoot1) 20) 21390123)
                                                                               


                            