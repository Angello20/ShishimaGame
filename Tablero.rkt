#lang racket
(require (lib "graphics.ss" "graphics"))
(require racket/gui/base)

(open-graphics)


; Define the game state
(define (crear-estado-juego)
  (let ((estado-juego (make-hash)))
    (hash-set! estado-juego "filai" 0)       ; Initial row of the move
    (hash-set! estado-juego "columnai" 0)    ; Initial column of the move
    (hash-set! estado-juego "columnaf" 0)    ; Final column of the move
    (hash-set! estado-juego "filaf" 0)       ; Final row of the move
    (hash-set! estado-juego "click" 0)       ; Coordinates of the initial click
    (hash-set! estado-juego "click2" 0)      ; Coordinates of the final click
    (hash-set! estado-juego "nu" 0)          ; Variable to determine the player's turn
    (hash-set! estado-juego "ganadorR" 0)    ; Indicator for red pieces winner
    (hash-set! estado-juego "ganadorA" 0)    ; Indicator for blue pieces winner
    (hash-set! estado-juego "turnoInicial" 0); Indicator for initial turn
    estado-juego))

(define estado-juego (crear-estado-juego))




; Graphic window of size 750 x 750 pixels
(define ventanal (open-viewport "Shishima" 750 750))

; Pieces vector
(define piezas (vector (vector  1 2 3);1 rojas        
                       (vector  0 0 0);2
                       (vector  4 5 6);3 azules
                       )
  )



; Function to check blue pieces diagonals
(define (revisar-diagonalesA)
  (define valor-f0c0 (vector-ref (vector-ref piezas 0) 0))
  (define valor-f1c1 (vector-ref (vector-ref piezas 1) 1))
  (define valor-f2c2 (vector-ref (vector-ref piezas 2) 2))
  (define valor-f0c2 (vector-ref (vector-ref piezas 0) 2))
  (define valor-f2c0 (vector-ref (vector-ref piezas 2) 0))
  (if (or (and (or (= valor-f0c0 4) (= valor-f0c0 5) (= valor-f0c0 6))
               (or (= valor-f1c1 4) (= valor-f1c1 5) (= valor-f1c1 6))
               (or (= valor-f2c2 4) (= valor-f2c2 5) (= valor-f2c2 6)))
          (and (or (= valor-f0c2 4) (= valor-f0c2 5) (= valor-f0c2 6))
               (or (= valor-f1c1 4) (= valor-f1c1 5) (= valor-f1c1 6))
               (or (= valor-f2c0 4) (= valor-f2c0 5) (= valor-f2c0 6))))
      (begin
         (hash-set! estado-juego "ganadorA" 1)
        (display "Ganador"))
      (display "No ganador")))

; Function to check red pieces diagonals
(define (revisar-diagonalesR)
  (define valor-f0c0 (vector-ref (vector-ref piezas 0) 0))
  (define valor-f1c1 (vector-ref (vector-ref piezas 1) 1))
  (define valor-f2c2 (vector-ref (vector-ref piezas 2) 2))
  (define valor-f0c2 (vector-ref (vector-ref piezas 0) 2))
  (define valor-f2c0 (vector-ref (vector-ref piezas 2) 0))
  (if (or (and (or (= valor-f0c0 1) (= valor-f0c0 2) (= valor-f0c0 3))
               (or (= valor-f1c1 1) (= valor-f1c1 2) (= valor-f1c1 3))
               (or (= valor-f2c2 1) (= valor-f2c2 2) (= valor-f2c2 3)))
          (and (or (= valor-f0c2 1) (= valor-f0c2 2) (= valor-f0c2 3))
               (or (= valor-f1c1 1) (= valor-f1c1 2) (= valor-f1c1 3))
               (or (= valor-f2c0 1) (= valor-f2c0 2) (= valor-f2c0 3))))
      (begin
        (hash-set! estado-juego "ganadorR" 1)
        (display "Ganador"))
      (display "No ganador")))

; Function to check blue pieces horizontal lines
(define (revisar-lineasHA)
  (define valor-f0c0 (vector-ref (vector-ref piezas 0) 0))
  (define valor-f0c1 (vector-ref (vector-ref piezas 0) 1))
  (define valor-f0c2 (vector-ref (vector-ref piezas 0) 2))
  (define valor-f1c0 (vector-ref (vector-ref piezas 1) 0))
  (define valor-f1c1 (vector-ref (vector-ref piezas 1) 1))
  (define valor-f1c2 (vector-ref (vector-ref piezas 1) 2))
  (define valor-f2c0 (vector-ref (vector-ref piezas 2) 0))
  (define valor-f2c1 (vector-ref (vector-ref piezas 2) 1))
  (define valor-f2c2 (vector-ref (vector-ref piezas 2) 2))
  (if (or (and (or (= valor-f0c0 4) (= valor-f0c0 5) (= valor-f0c0 6))
               (or (= valor-f0c1 4) (= valor-f0c1 5) (= valor-f0c1 6))
               (or (= valor-f0c2 4) (= valor-f0c2 5) (= valor-f0c2 6)))
          (and (or (= valor-f1c0 4) (= valor-f1c0 5) (= valor-f1c0 6))
               (or (= valor-f1c1 4) (= valor-f1c1 5) (= valor-f1c1 6))
               (or (= valor-f1c2 4) (= valor-f1c2 5) (= valor-f1c2 6)))
          (and (or (= valor-f2c0 4) (= valor-f2c0 5) (= valor-f2c0 6))
               (or (= valor-f2c1 4) (= valor-f2c1 5) (= valor-f2c1 6))
               (or (= valor-f2c2 4) (= valor-f2c2 5) (= valor-f2c2 6))))
      (begin
        (hash-set! estado-juego "ganadorA" 1)
        (display "Ganador"))
      (display "No ganador")))


; Function to check red pieces horizontal lines
(define (revisar-lineasHR)
  (define valor-f0c0 (vector-ref (vector-ref piezas 0) 0))
  (define valor-f0c1 (vector-ref (vector-ref piezas 0) 1))
  (define valor-f0c2 (vector-ref (vector-ref piezas 0) 2))
  (define valor-f1c0 (vector-ref (vector-ref piezas 1) 0))
  (define valor-f1c1 (vector-ref (vector-ref piezas 1) 1))
  (define valor-f1c2 (vector-ref (vector-ref piezas 1) 2))
  (define valor-f2c0 (vector-ref (vector-ref piezas 2) 0))
  (define valor-f2c1 (vector-ref (vector-ref piezas 2) 1))
  (define valor-f2c2 (vector-ref (vector-ref piezas 2) 2))
  (if (or (and (or (= valor-f0c0 1) (= valor-f0c0 2) (= valor-f0c0 3))
               (or (= valor-f0c1 1) (= valor-f0c1 2) (= valor-f0c1 3))
               (or (= valor-f0c2 1) (= valor-f0c2 2) (= valor-f0c2 3)))
          (and (or (= valor-f1c0 1) (= valor-f1c0 2) (= valor-f1c0 3))
               (or (= valor-f1c1 1) (= valor-f1c1 2) (= valor-f1c1 3))
               (or (= valor-f1c2 1) (= valor-f1c2 2) (= valor-f1c2 3)))
          (and (or (= valor-f2c0 1) (= valor-f2c0 2) (= valor-f2c0 3))
               (or (= valor-f2c1 1) (= valor-f2c1 2) (= valor-f2c1 3))
               (or (= valor-f2c2 1) (= valor-f2c2 2) (= valor-f2c2 3))))
      (begin
        (hash-set! estado-juego "ganadorR" 1)
        (display "Ganado"))
      (display "No ganador")))

; Function to check blue pieces vertical lines
(define (revisar-lineasVA)
  (define valor-f0c0 (vector-ref (vector-ref piezas 0) 0))
  (define valor-f1c0 (vector-ref (vector-ref piezas 1) 0))
  (define valor-f2c0 (vector-ref (vector-ref piezas 2) 0))
  (define valor-f0c1 (vector-ref (vector-ref piezas 0) 1))
  (define valor-f1c1 (vector-ref (vector-ref piezas 1) 1))
  (define valor-f2c1 (vector-ref (vector-ref piezas 2) 1))
  (define valor-f0c2 (vector-ref (vector-ref piezas 0) 2))
  (define valor-f1c2 (vector-ref (vector-ref piezas 1) 2))
  (define valor-f2c2 (vector-ref (vector-ref piezas 2) 2))
  (if (or (and (or (= valor-f0c0 4) (= valor-f0c0 5) (= valor-f0c0 6))
               (or (= valor-f1c0 4) (= valor-f1c0 5) (= valor-f1c0 6))
               (or (= valor-f2c0 4) (= valor-f2c0 5) (= valor-f2c0 6)))
          (and (or (= valor-f0c1 4) (= valor-f0c1 5) (= valor-f0c1 6))
               (or (= valor-f1c1 4) (= valor-f1c1 5) (= valor-f1c1 6))
               (or (= valor-f2c1 4) (= valor-f2c1 5) (= valor-f2c1 6)))
          (and (or (= valor-f0c2 4) (= valor-f0c2 5) (= valor-f0c2 6))
               (or (= valor-f1c2 4) (= valor-f1c2 5) (= valor-f1c2 6))
               (or (= valor-f2c2 4) (= valor-f2c2 5) (= valor-f2c2 6))))
      (begin
        (hash-set! estado-juego "ganadorA" 1)
        (display "Ganador"))
      (display "No ganador")))

; Function to check red pieces vertical lines
(define (revisar-lineasVR)
  (define valor-f0c0 (vector-ref (vector-ref piezas 0) 0))
  (define valor-f1c0 (vector-ref (vector-ref piezas 1) 0))
  (define valor-f2c0 (vector-ref (vector-ref piezas 2) 0))
  (define valor-f0c1 (vector-ref (vector-ref piezas 0) 1))
  (define valor-f1c1 (vector-ref (vector-ref piezas 1) 1))
  (define valor-f2c1 (vector-ref (vector-ref piezas 2) 1))
  (define valor-f0c2 (vector-ref (vector-ref piezas 0) 2))
  (define valor-f1c2 (vector-ref (vector-ref piezas 1) 2))
  (define valor-f2c2 (vector-ref (vector-ref piezas 2) 2))
  (if (or (and (or (= valor-f0c0 1) (= valor-f0c0 2) (= valor-f0c0 3))
               (or (= valor-f1c0 1) (= valor-f1c0 2) (= valor-f1c0 3))
               (or (= valor-f2c0 1) (= valor-f2c0 2) (= valor-f2c0 3)))
          (and (or (= valor-f0c1 1) (= valor-f0c1 2) (= valor-f0c1 3))
               (or (= valor-f1c1 1) (= valor-f1c1 2) (= valor-f1c1 3))
               (or (= valor-f2c1 1) (= valor-f2c1 2) (= valor-f2c1 3)))
          (and (or (= valor-f0c2 1) (= valor-f0c2 2) (= valor-f0c2 3))
               (or (= valor-f1c2 1) (= valor-f1c2 2) (= valor-f1c2 3))
               (or (= valor-f2c2 1) (= valor-f2c2 2) (= valor-f2c2 3))))
      (begin
        (hash-set! estado-juego "ganadorR" 1)
        (display "Ganador"))
      (display "No ganador")))



(display (vector->list piezas))


  





; Function to draw the board
(define (dibujar x y i)
  (if (> i 3)
      ((draw-rectangle ventanal) (make-posn 0 0) 450 450 "black")
      (if (> x 300)
          (dibujar 0 (+ y 150) (+ i 1))
          (begin
            (if (odd? i)
                (if (odd?(/ x 150) )
                    (dibujar (+ x 150) y i)
                    (begin
                      ((draw-solid-rectangle ventanal) (make-posn x y) 150 150 "cyan")
                      (dibujar (+ x 150) y i))
                    )
                (if (odd? (/ x 150))
                    (begin
                      ((draw-solid-rectangle ventanal) (make-posn x y) 150 150 "cyan")
                      (dibujar (+ x 150) y i))
                    (dibujar (+ x 150) y i))
                )
            )
          )
      )
  )

(dibujar 0 0 1)


; Draw the initial pieces
(((draw-pixmap-posn "roja.png") ventanal)(make-posn 0 0 ))
(((draw-pixmap-posn "roja.png") ventanal)(make-posn 150 0 ))
(((draw-pixmap-posn "roja.png") ventanal)(make-posn 300 0 ))
(((draw-pixmap-posn "azul.png") ventanal)(make-posn 0 300 ))
(((draw-pixmap-posn "azul.png") ventanal)(make-posn 150 300 ))
(((draw-pixmap-posn "azul.png") ventanal)(make-posn 300 300 ))


; Function to get the mouse position at the start of a move
(define (inicio)
  (hash-set! estado-juego "click" (get-mouse-click ventanal))
  (hash-set! estado-juego "filai" (quotient (posn-y (mouse-click-posn (hash-ref estado-juego "click"))) 150))
  (hash-set! estado-juego "columnai" (quotient (posn-x (mouse-click-posn (hash-ref estado-juego "click"))) 150))
  (hash-ref estado-juego "filaf")
  (if (and (>= (hash-ref estado-juego "filai") 0) (< (hash-ref estado-juego "filai") 3) (>= (hash-ref estado-juego "columnai") 0) (< (hash-ref estado-juego "columnai") 3))
      (vector-ref (vector-ref piezas (hash-ref estado-juego "filai")) (hash-ref estado-juego "columnai"))
      (inicio))
  )

; Function to get the mouse position at the end of a move
(define (final)
  (hash-set! estado-juego "click2" (get-mouse-click ventanal))
  (hash-set! estado-juego "filaf" (quotient (posn-y (mouse-click-posn (hash-ref estado-juego "click2"))) 150))
  (hash-set! estado-juego "columnaf" (quotient (posn-x (mouse-click-posn (hash-ref estado-juego "click2"))) 150)))


; Function to perform a move
(define (llamar ficha)
 (print ficha)
  
  ; Set initial turn based on the piece
  (if (= (hash-ref estado-juego "turnoInicial") 0)
    (if (and (>= ficha 4) (<= ficha 6))
        (begin 
          (hash-set! estado-juego "nu" 0)
          (hash-set! estado-juego "turnoInicial" 1))
        (begin 
          (hash-set! estado-juego "nu" 1)
          (hash-set! estado-juego "turnoInicial" 1)))
    (void))


  ; Determine move based on the turn (red or blue piece)
  (if (odd? (hash-ref estado-juego "nu"))
      ; Red pieces move
      (if (and (>= ficha 1)(<= ficha 3))
          (begin
            (final)
            (verificar-movimiento (hash-ref estado-juego "filaf") (hash-ref estado-juego "columnaf") ficha (hash-ref estado-juego "nu"))
            (if (= (hash-ref estado-juego "ganadorR") 1)
                ((((draw-pixmap-posn "ganadorR.png") ventanal)(make-posn 50 475)) (sleep 3) (close-graphics) (exit 0))
                (void))
            (llamar (inicio))   
            )                        
          (begin
            (llamar (inicio))
            )
          )
      ; Blue pieces move
      (if (and (>= ficha 4)(<= ficha 6))
              (begin
                (final)
                (verificar-movimiento (hash-ref estado-juego "filaf") (hash-ref estado-juego "columnaf") ficha (hash-ref estado-juego "nu"))
                (if (= (hash-ref estado-juego "ganadorA") 1)
                    ((((draw-pixmap-posn "ganadorA.png") ventanal)(make-posn 50 475)) (sleep 3) (close-graphics) (exit 0))
                    (void))
                (llamar (inicio))   
                )    
              (begin
                (llamar (inicio))
                )
              )
          )
      )
  


; Function to check if a piece is blue or red, and verify a valid move
(define (verificar-movimiento ff cf ficha nu)
  (define ficha_actual (vector-ref (vector-ref piezas ff) cf))
  (if (and (odd? nu) (Roja? ff cf))
      (cond
        ((and (>= ficha 1) (<= ficha 3))
         (if (or (= ficha_actual 0) (Azul? ff cf))
             (rojaM ff cf ficha (hash-ref estado-juego "filai") (hash-ref estado-juego "columnai"))
             (display "No puedes mover una ficha a una posición ocupada por una ficha aliada o enemiga")))
        (else (display "Ficha inexistente en el juego"))
        )
      (if (and (even? nu) (Azul? ff cf))
          (cond
            ((and (>= ficha 4) (<= ficha 6))
             (if (or (= ficha_actual 0) (Roja? ff cf))
                 (azulM ff cf ficha (hash-ref estado-juego "filai") (hash-ref estado-juego "columnai"))
                 (display "No puedes mover una ficha a una posición ocupada por una ficha aliada o enemiga")))
            (else (display "Ficha inexistente en el juego"))
            )
          (display "No te puedes comer a tu equipo"))
      )
  )



; Function to check if a piece is red
(define (Roja? ff cf)
  (if (or (and (>= (vector-ref (vector-ref piezas ff) cf) 1)
               (<= (vector-ref (vector-ref piezas ff) cf) 3))
          (= (vector-ref (vector-ref piezas ff) cf) 0))
      #t
      #f
      )
    )

; Function to check if a piece is blue
(define (Azul? ff cf)
  (print ff)
  (print "")
  (print cf)
  (if (or (and (>= (vector-ref (vector-ref piezas ff) cf) 4)
               (<= (vector-ref (vector-ref piezas ff) cf) 6))
          (= (vector-ref (vector-ref piezas ff) cf) 0))
      #t
      #f
      )
    )




; Function to move a red piece
(define (rojaM ff cf ficha fi ci)

   (if (and (> ff -1)(< ff 3)
           (> cf -1)(< cf 3))
       
  ; Ensure pieces in positions f0 c1, f1 c0, f1 c2, and f2 c1 cannot move diagonally    
  (if (or (and (= ff 0) (= cf 1))
          (and (= ff 1) (= cf 0))
          (and (= ff 1) (= cf 2))
          (and (= ff 2) (= cf 1)))
      (if (or (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (- (hash-ref estado-juego "columnai") 1))))
          (begin
            (jugar ff cf ficha)
          )
          (displayln "Movimiento invalido"))
      (if (or (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (- (hash-ref estado-juego "columnai") 1)))
              (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (- (hash-ref estado-juego "columnai") 1)))
              (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (- (hash-ref estado-juego "columnai") 1))))
          (begin
            (jugar ff cf ficha)
          )
          (displayln "Movimiento invalido"))
      )
  (void)
  )
  )

; Function to move a blue piece
(define (azulM ff cf ficha fi ci)


   (if (and (> ff -1)(< ff 3)
           (> cf -1)(< cf 3))
       
  ; Ensure pieces in positions f0 c1, f1 c0, f1 c2, and f2 c1 cannot move diagonally     
  (if (or (and (= ff 0) (= cf 1))
          (and (= ff 1) (= cf 0))
          (and (= ff 1) (= cf 2))
          (and (= ff 2) (= cf 1)))
      (if (or (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (- (hash-ref estado-juego "columnai") 1))))
          (begin
            (jugar ff cf ficha)
          )
          (displayln "Movimiento invalido"))
      (if (or (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (- (hash-ref estado-juego "columnai") 1)))
              (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (- (hash-ref estado-juego "columnai") 1)))
              (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (+ (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (+ (hash-ref estado-juego "columnai") 1)))
              (and (= ff (- (hash-ref estado-juego "filai") 1)) (= cf (hash-ref estado-juego "columnai")))
              (and (= ff (hash-ref estado-juego "filai")) (= cf (- (hash-ref estado-juego "columnai") 1))))
          (begin
            (jugar ff cf ficha)
          )
          (displayln "Movimiento invalido"))
      )
  (void)
  )
  )





; Function to play a move graphically
(define (jugar ff cf ficha)
  
  ;(set! ficha2 (vector-ref (vector-ref piezas ff) cf))
  
  (vector-set! (vector-ref piezas (hash-ref estado-juego "filai")) (hash-ref estado-juego "columnai")  0)
  (vector-set! (vector-ref piezas ff) cf  ficha)
  (if (odd? (hash-ref estado-juego "nu"))
    ; true branch
    (begin
        ((draw-solid-rectangle ventanal) (make-posn 0 450) 450 150 "white")
        ((draw-solid-rectangle ventanal) (make-posn (* cf 150) (* ff 150)) 150 150 (color2))
        (((draw-pixmap-posn (usar ficha)) ventanal) (make-posn (* cf 150) (+ (* ff 150) 10)))
        ((draw-solid-rectangle ventanal) (make-posn (* (hash-ref estado-juego "columnai") 150) (* (hash-ref estado-juego "filai") 150)) 150 150 (color))
        (hash-set! estado-juego "nu" (+ (hash-ref estado-juego "nu") 1))
        (revisar-diagonalesR)
        (revisar-lineasHR)
        (revisar-lineasVR)
        (display (vector->list piezas))
    )
    ; false branch
    (begin
        ((draw-solid-rectangle ventanal) (make-posn 0 450) 450 150 "white")
        ((draw-solid-rectangle ventanal) (make-posn (* cf 150) (* ff 150)) 150 150 (color2))
        (((draw-pixmap-posn (usar ficha)) ventanal) (make-posn (* cf 150) (+ (* ff 150) 10)))
        ((draw-solid-rectangle ventanal) (make-posn (* (hash-ref estado-juego "columnai") 150) (* (hash-ref estado-juego "filai") 150)) 150 150 (color))
        (hash-set! estado-juego "nu" (+ (hash-ref estado-juego "nu") 1))
        (revisar-diagonalesA)
        (revisar-lineasHA)
        (revisar-lineasVA)
        (display (vector->list piezas))
    )
)

  )
  

      






; Function to determine the background color of the square
(define (color)
  (if (even? (+ (hash-ref estado-juego "filai") (hash-ref estado-juego "columnai")))
      "cyan"
      "White"
      )
  )

; Function to determine the background color of the square
(define (color2)
  (if (even? (+ (hash-ref estado-juego "columnaf") (hash-ref estado-juego "filaf")))
      "cyan"
      "White"
      )
  )

; Function to select the image of a piece
(define (usar ficha)
  (cond
    ((and (>= ficha 1) (<= ficha 3))  "roja.png")
    ((and (>= ficha 4) (<= ficha 6)) "azul.png")
    (else  (display "ficha inexistente en el juego"))
    )
  )


; Start the game by calling the `llamar` function with the first move
(llamar (inicio))


