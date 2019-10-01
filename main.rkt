#lang racket
;; Este archivo contiene los requerimientos funcionales y las funciones asociadas.
(require "TDA_personajes.rkt")
(require "TDA_equipo.rkt")
(require "TDA_escena.rkt")
(require "aux.rkt")
(define null '())





;; Funcion que crea un escenario para el desarrollo de una partida
;; Dominio: entero X entero X entero X entero X entero
;; Recorrido: escena
(define (createScene N M E D seed) (
                                    if(and (number? N) (> N 0) (number? M) (> M 0) (number? E) (> E 0) (number? D) (> D 0) (number? seed) (> seed 0)) ;(>= N seed);)
                                      (escena N M "PLAYING" 0 (initEquipoJ "Jugador" 3 (modulo seed N) 3) (initEquipoC "CPU" (- M (- E 1)) (modulo seed N) M) '())
                                      null
                                      )
  )
;;(createScene 10 10 1 4 1001)


;; Funcion que verifica que un escenario sea valido para el desarrollo de una partida
;; Dominio: cualquier cosa
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
                                                                                                     '()
                                                                                                     )
                                                                                                    scene
                                                                                                    )
                                                                                    )
                                                                    )
                                                       )
                                        )
                       )
  )
  


;; Funcion que convierte un escenario a un string
;; Dominio: escena
;; Recorrido: string
;; Recursion: de cola, esta en funciones auxiliares
(define (scene->string scene) (
                               if(escena? scene)
                                 (string-append (headerStr scene) (make-string (* 2(getMescena scene)) #\#) (bodyStr scene 1 1) (make-string (* 2(getMescena scene)) #\#) "\n")
                                 ""
                                 )
  )

;; Funcion que convierte un escenario a una representacion en Json
;; Dominio: escena
;; Recorrido: string
;; Recursion: natural o lineal, implementacion intuitiva
(define (scene->json scene) (define (getPJson p) ( ;; Funcion que retorna string con formato Json de personaje
                                                  if(personaje? p)
                                                    (string-append "\n\t\t{" "\n\t\t\t" "\"X\"" ": " (number->string (getXpersonaje p)) ",\n\t\t\t" "\"Y\"" ": " (number->string (getYpersonaje p)) ",\n\t\t\t" "\"Vida\"" ": " (number->string (getVidapersonaje p)) "\n\t\t}")
                                                    ""
                                                    )
                              )
  (define (getLPJson l) ( ;; Funcion que retorna string con formato Json de lista de personajes, recursiva lineal
                         if(null? (cdr l))
                           (getPJson (car l))
                           (string-append (getPJson (car l)) ",\n" (getLPJson (cdr l)))
                           )
    )
  (
   if(escena? scene) ;; Aqui comienza procedimiento de scene->json
     (string-append "{\n\t" "\"Filas\"" ": " (number->string (getNescena scene)) ",\n\t" "\"Columnas\"" ": " (number->string (getMescena scene)) ",\n\t" "\"Estado\"" ": " "\"" (getEstescena scene) "\"" ",\n\t" "\"Puntaje\"" ": " (number->string (getPtjescena scene))
                    ",\n\t" "\"" (getFlequipo (getEq1escena scene)) "\"" ": [" (getLPJson (getPequipo (getEq1escena scene))) "\n\t]"  ",\n\t" "\"" (getFlequipo (getEq2escena scene)) "\"" ": [" (getLPJson (getPequipo (getEq2escena scene))) "\n\t]"
                    (if (and (pair? (last scene)) (not (empty? (last scene))))
                        (string-append ",\n\t" "\"Proyectil\"" ": [ "  "\"X\"" ": " (number->string (car (last scene))) ", " "\"Y\"" ": " (number->string (cdr (last scene))) "]")
                        "")
                    "\n}")
                               ""
                               )
  )

;; Funciom que convierte un escenario a una representacion en formato Xml
;; Dominio: escena
;; Recorrido: string
;; Recursion: natural, implementacion similar a funcion scene->json
(define (scene->xml scene) (define (perXml p) (if (personaje? p) ;; Funcion que entrega representacion de personaje en Xml
                                                  (string-append "\n\t\t\t<p>\n\t\t\t\t<x>" (number->string (getXpersonaje p)) "</x>\n\t\t\t\t<y>" (number->string (getYpersonaje p)) "</y>\n\t\t\t\t<vida>" (number->string (getVidapersonaje p)) "</vida>\n\t\t\t</p>")
                                                  "")
                             ) (define (listPerXml l) (if (sonPersonaje l) ;; Funcion que entrega representacion de lista de personajes en Xml, recursiva lineal
                                                          (if (null? (cdr l))
                                                              (string-append (perXml (car l)))
                                                              (string-append (perXml (car l)) (listPerXml (cdr l)))
                                                              )
                                                          ""
                                                          )
                                 )
  (if (escena? scene) ;; Aqui comienza el procedimiento de scene->xml
      (string-append "<escena>" "\n\t<N>" (number->string (getNescena scene)) "</N>\n\t<M>" (number->string (getMescena scene)) "</M>\n\t<Estado>" (getEstescena scene) "</Estado>\n\t<Puntaje>" (number->string (getPtjescena scene)) "</Puntaje>\n\t<Equipo1>\n\t\t<flag>"
                     (getFlequipo (getEq1escena scene)) "</flag>\n\t\t<ListaPersonajes>" (listPerXml (getPequipo (getEq1escena scene))) "\n\t\t</ListaPersonajes>\n\t</Equipo1>\n\t<Equipo2>\n\t\t<flag>" (getFlequipo (getEq2escena scene)) "</flag>\n\t\t<ListaPersonajes>" (listPerXml (getPequipo (getEq2escena scene))) "\n\t\t</ListaPersonajes>\n\t</Equipo2>"
                     (if(null? (last scene)) "" (string-append "\n\t<Proyectil>\n\t\t<x>" (number->string (car (last scene))) "</x>\n\t\t<y>" (number->string (cdr (last scene))) "</y>\n\t</Proyectil>"))"\n</escena>")
      ""
      )
  )


;; Funcion que entrega una lista infinita con actualizaciones de un escenario a lo largo de una jugada
;; Dominio: escena X entero X entero X funcion X entero X entero X entero
;; Recorrido: lista infinita con escenas
;; Recursion: natural, propio de la mayoria de las listas infinitas
(define (playLazy scene member move tf t angle seed) (define (playLazyAux scene member move tf t angle seed cont)(
                                                                                                                  if(escena? scene)
                                                                                                                    (cons (
                                                                                                                           escena (getNescena scene)
                                                                                                                                  (getMescena scene)
                                                                                                                                  (checkStateLazy scene member move tf (exact-floor cont) angle seed)
                                                                                                                                  (checkScoreLazy scene member move tf (exact-floor cont) angle seed)
                                                                                                                                  (checkEq1Lazy scene member move tf (exact-floor cont) angle seed)
                                                                                                                                  (checkEq2Lazy scene member move tf (exact-floor cont) angle seed)
                                                                                                                                  (cond [(and (> cont 0) (< cont (length (tf angle (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene)))))
                                                                                                                                         (cons (exact-floor cont) (nth (exact-floor cont) (tf angle (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene))))]
                                                                                                                                        [(and (> cont (length (tf angle (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene)))) (< cont (+ (length (tf (modulo seed 360) (quienDispara (getPequipo (getEq2escena scene))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene))) (length (tf angle (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene))))))
                                                                                                                                         (cons (+ (exact-floor cont) (quienDispara (getPequipo (getEq2escena scene)))) (nth (- (exact-floor cont) (length (tf angle (getXpersonaje (nth member (getPequipo (getEq1escena scene)))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene)))) (length (tf (modulo seed 360) (quienDispara (getPequipo (getEq2escena scene))) (getYpersonaje (car (getPequipo (getEq1escena scene)))) (getMescena scene)))))]
                                                                                                                                        [else '()]
                                                                                                                                        )
                                                                                                                                  )
                                                                                                                          (lazy (playLazyAux scene member move tf t angle seed (+ cont t))))
                                                                                                                          (cons null (lazy (playLazyAux scene member move tf t angle seed (+ cont t)))))
                                                                                                                          )
                                                                                                                    
                                                                                                                    
                                                       
  (
   playLazyAux scene member move tf t angle seed 0)
  )
                                                                      
  





;; Ejemplos de uso de createScene

;;(define S1 (createScene 10 10 2 4 7))
;;(createScene 3 5 6 7 8) --> retornara escena, pero al verificar con checkScene retornara falso
;;(createScene 10 10 5 4 234423)

;; Ejemplos de uso de checkScene
;;(checkScene (createScene 3 5 6 7 8)) --> retornara falso
;;(checkScene (createScene 10 10 5 4 234423))---> retorna verdarero
;;(checkScene (createScene 10 10 2 4 7))---> retorna verdadero

;; Ejemplos de uso de play
;((((((play (createScene 10 10 2 4 7)) 2) 3) shoot1) 20) 2312323)
;((((((play (createScene 10 10 2 4 7)) 2) 3) shoot1) 200) 2312323) --> dispara al suelo
;((((((play (createScene 10 10 2 4 7)) 2) 3) shoot1) 98) 2312323) --> dispara al aire

;; Ejemplos de uso de playLazy
;(playLazy S1 2 3 shoot1 8 20 12312313)
;(playLazy (createScene 10 10 5 4 234423) 2 3 shoot1 8 20 12312313)
;(playLazy (createScene 10 12 2 7 234423) 1 4 shoot2 5 32 13912398)

;; Ejemplos de uso de scene->string
;(scene-string (createScene 10 10 5 4 234423))
;(display (scene->string S1)) --> lo muestra por consola
;(write (createScene 10 10 5 4 234423))

;; Ejemplos de uso de scene->json
;(display (scene->json S1))---> muestra por consola
;(scene->json (createScene 10 10 5 4 234423))
;(write (scene->json (createScene 10 10 5 4 234423)))

;; Ejemplos de uso de scene->xml
;(display (scene->xml S1))---> muestra por consola
;(scene->xml (createScene 10 10 5 4 234423))
;(write (scene->xml (createScene 10 10 5 4 234423)))
                                                                               


                            