;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname snakes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; Feeding Worm, ex 215 - 219
; Data Definitions
; Worm, Food
; a posn, the same definition of posn 

; Worms
; list of Worms
; - '()
; - (cons Worm Worms)

; To
; directions of Worms, one of the four Strings
; - "left", "right", "up", "down"

; WorldState
; Worms, To, Food
(define-struct ws (worms to food))


; Constant Definitions:
(define WIDTH 400)
(define HEIGHT 600)
(define BG (empty-scene WIDTH HEIGHT))

(define RATE 0.3) ; clock tick rate is seconds 

(define SIZE 10)
(define WORM (circle SIZE "solid" "red"))
(define FOOD (circle SIZE "solid" "green"))
(define STEP (* 2 SIZE))

(define START-X (/ WIDTH 2))
(define START-Y (/ HEIGHT 2))
(define START (make-posn START-X START-Y))

;;;;;;;;;;;;;;;;;;;;;;
; Main Program
;;;;;;;;;;;;;;;;;;;;;;

; WorldState -> WorldState
(define (main ws)
  (big-bang ws
            (stop-when gameover score)
            (on-tick tock RATE)
            (on-key keyh)
            (to-draw render)
            (name "For NOKIA")
            ))

;;;;;;;;;;;;;;;;;;;;;;
; Game Logic
;;;;;;;;;;;;;;;;;;;;;;

; WorldState -> Boolean 
(define (gameover ws)
  (not (string=? (worms-dead ws) "nothing")))

; WorldState -> String
(define (worms-dead ws)
  (cond
    ((hit-wall? ws) "Hit Wall. Game Over!")
    ((eat-self? ws) "Eat self. Game Over!")
    (else "nothing")
    ))

; WorldState -> Boolean
(define (hit-wall? ws)
  (or (<= (- (worm-x ws) SIZE) 0)
      (<= (- (worm-y ws) SIZE) 0)
      (>= (+ (worm-x ws) SIZE) WIDTH)
      (>= (+ (worm-y ws) SIZE) HEIGHT)))

; WorldState -> Boolean
(define (eat-self? ws)
  (and (member? (ws-to ws)
                (list "up" "down" "left" "right"))
       (member? (next-of ws)
                (ws-worms ws))
       (not (equal? (next-of ws)
                    (tail-of ws)))))

; WorldState -> Worm/Posn
(define (next-of ws)
  (cond
    ((string=? "up" (ws-to ws)) (up-of ws))
    ((string=? "down" (ws-to ws)) (down-of ws))
    ((string=? "left" (ws-to ws)) (left-of ws))
    ((string=? "right" (ws-to ws)) (right-of ws))
    (else (head-of ws))
    ))

; WorldState -> Posn
; get the head of worms
(define (head-of ws)
  (first (ws-worms ws)))

; WorldState -> Posn
; get the tail of worms
(define (tail-of ws)
  (ws-tail-of (ws-worms ws)))
; List -> Posn
; Auxiliary function to (tail-of ws)
(define (ws-tail-of worms)
  (cond
    ((empty? worms) '())
    ((empty? (rest worms)) (first worms))
    (else (ws-tail-of (rest worms)))))

; WorldState -> Worm/Posn
; get the neighbor positions of Worm's head
(define (up-of ws)
  (make-posn (worm-x ws) (- (worm-y ws) STEP)))

(define (down-of ws)
  (make-posn (worm-x ws) (+ (worm-y ws) STEP)))

(define (left-of ws)
  (make-posn (- (worm-x ws) STEP) (worm-y ws)))

(define (right-of ws)
  (make-posn (+ (worm-x ws) STEP) (worm-y ws)))

; WorldState -> Number
; get the worm's head x and y position
(define (worm-x ws)
  (posn-x (first (ws-worms ws))))

(define (worm-y ws)
  (posn-y (first (ws-worms ws))))


;;;;;;;;;;;;;;;;;;;;;;
; Render Image
;;;;;;;;;;;;;;;;;;;;;;
; WorldState -> Image
; render the current worldstate
(define (render ws)
  (ws-render (ws-worms ws) (ws-food ws)))
; Posn Posn -> Image
; place worms and food
; Auxiliary function for render
(define (ws-render worms food)
  (cond
    ((empty? worms) (init-bg food))
    (else (place-worm (first worms)
                      (ws-render (rest worms) food)))))

; WorldState -> Image
; create the current worldstate with food
(define (init-bg food)
  (place-image FOOD (posn-x food) (posn-y food) BG))

; WorldState -> Image
; place A worm in the BG
(define (place-worm w bg)
  (place-image WORM (posn-x w) (posn-y w) bg))

; WorldState -> Image
; place game over and sores in the BG
(define (score ws)
  (place-image (score-img ws) START-X START-Y (render ws)))

; WorldState -> Image
; create the score's image
(define (score-img ws)
  (above (text (worms-dead ws) 24 "blue")
         (text (get-score ws) 20 "black")))

; WorldState -> String
; return the score/length of the worms
(define (get-score ws)
  (string-append "Your Score: "
                 (number->string (length (ws-worms ws)))))
;;;;;;;;;;;;;;;;;;;;;;
; Run Game
;;;;;;;;;;;;;;;;;;;;;;
; WorldState -> WorldState
; clock ticking
(define (tock ws)
  (move-on ws))

; WorldState -> WorldState
; use arrow key to move
(define (keyh ws k)
  (move-on (ws-chdir k ws)))

; WorldState String -> WorldState
; change direction, reverse is speed, not eat self
(define (ws-chdir to ws)
  (if (reverse? to ws) ws
      (make-ws (ws-worms ws)
               to
               (ws-food ws))))

; String WorldState -> Boolean
(define (reverse? to ws)
  (or (and (string=? to "up")
           (string=? (ws-to ws) "down"))
      (and (string=? to "down")
           (string=? (ws-to ws) "up"))
      (and (string=? to "left")
           (string=? (ws-to ws) "right"))
      (and (string=? to "right")
           (string=? (ws-to ws) "left"))))

; WorldState -> WorldState
; under arrow key, or automatically move on to next
(define (move-on ws)
  (if (get-food? ws)
      (eat-food ws)
      (move-next ws)
      ))

; WorldState -> WorldState
; eat food, just add food to the head of worms
; and create a new food
(define (eat-food ws)
  (ws-new-food (add-head ws)))


; WorldState -> WorldState
; create a new food for world
(define (ws-new-food ws)
  (make-ws (ws-worms ws)
           (ws-to ws)
           (food-create (head-of ws))))


; WorldState -> WorldState
; move worms to next
; just add a head and cut the tail
(define (move-next ws)
  (cut-tail (add-head ws)))

; WorldState -> WorldState
; add next to the head of worms
(define (add-head ws)
  (make-ws (cons (next-of ws) (ws-worms ws))
           (ws-to ws)
           (ws-food ws)))

; WorldState -> WorldState
; cut the tail of worms
(define (cut-tail ws)
  (make-ws (do-cut-tail (ws-worms ws))
           (ws-to ws)
           (ws-food ws)))
; List -> List
; cut the tail of a list
; Auxiliary function for cut-tail
(define (do-cut-tail worms)
  (cond
    ((empty? worms) '())
    ((empty? (rest worms)) '())
    (else (cons (first worms)
                (do-cut-tail (rest worms))))))

; WorldState -> Boolean
; test if next move will eat food
(define (get-food? ws)
  (equal? (next-of ws) (ws-food ws)))

; Posn -> Posn
; create food, in the grids, not equal to head of worms
(define (food-create p)
  (food-check-create
   p (make-posn (* (random (/ WIDTH STEP)) STEP)
                (* (random (/ HEIGHT STEP)) STEP))))

; Posn Posn -> Posn 
; generative recursion 
; check-create? 
(define (food-check-create p candidate)
  (if (or (< (posn-x candidate) STEP)
          (< (posn-y candidate) STEP)
          (> (posn-x candidate) (- WIDTH STEP))
          (> (posn-y candidate) (- HEIGHT STEP))
          (equal? p candidate))
      (food-create p)
      candidate))

(main (make-ws (list START) "a" (make-posn 40 40)))
