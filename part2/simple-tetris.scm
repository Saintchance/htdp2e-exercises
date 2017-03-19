;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname simple-tetris) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define SIZE 20) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
(define HEIGHT 20) ; # of blocks, vertically
(define SCENE-HEIGHT (* HEIGHT SIZE))
(define BG (empty-scene SCENE-SIZE SCENE-HEIGHT))

(define BLOCK ; red squares with black rims
  (overlay
   (square (- SIZE 1) "solid" "red")
   (square SIZE "outline" "black")))

(define-struct tetris (block landscape))
(define-struct block (x y))

; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of:
; - '()
; - (cons Block Landscape)
; Block is a structure:
;   (make-block N N)
;
; interpretations:
; (make-block x y) depicts a block whose left
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

; some test data
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define landscape0 (list block-landed block-on-block))
(define block-dropping (make-block 1 1))
(define tetris0 (make-tetris block-dropping landscape0))
(define tetris0-drop (make-tetris (make-block 5 5) landscape0))

;;;;;;;;;;;;;;;;;;;;;;;;
; Game Render
;;;;;;;;;;;;;;;;;;;;;;;;

; Tetris -> Image
; render tetris into image
(define (tetris-render t)
  (place-block (tetris-block t)
               (place-landscape (tetris-landscape t))))

; Block Image -> Image
(define (place-block b bg)
  (place-image BLOCK (p-x b) (p-y b) bg))

; Block -> N
; get the real coordinate of the block
(define (p-x b) (* (+ (block-x b) 1/2) SIZE))
(define (p-y b) (* (+ (block-y b) 1/2) SIZE))

; Landscape -> Image
(define (place-landscape l)
  (cond
    ((empty? l) BG)
    (else (place-block (first l)
                       (place-landscape (rest l))))))

; T -> Image
(define (score t)
  (place-image (text (string-append
                      "Score: "
                      (number->string (length (tetris-landscape t))))
                     24 "black")
               (/ (image-width BG) 2)
               (/ (image-height BG) 2)
               (tetris-render t)))
;;;;;;;;;;;;;;;;;;;;;;;;
; Main Program
;;;;;;;;;;;;;;;;;;;;;;;;
; Tetris -> Tetris
(define (tetris-main t s)
  (big-bang t
            (on-tick tock s)
            (on-key keyh)
            (stop-when gameover score)
            (to-draw tetris-render)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Game Logic
;;;;;;;;;;;;;;;;;;;;;;;;
; T -> Boolean
(define (gameover t)
  (and (resting? (go-down t))
       (= 0 (block-y (tetris-block t)))))

; T -> T
(define (tock t)
  (if (or (resting? (go-down t))
          (under-floor? (go-down t)))
      (block-generate t)
      (go-down t)))

; T -> T
(define (block-generate t)
  (make-tetris
   (new-block (tetris-block t))
   (drop->rest t)))

; T -> Landscape
(define (drop->rest t)
  (append (list (tetris-block t))
          (tetris-landscape t)))

; Block -> Block
(define (new-block b)
  (new-check-block
   b (make-block (random WIDTH) -1)))

; Block Block -> Block
(define (new-check-block b candidate)
  (if (= (block-x b) (block-x candidate))
      (new-block b)
      candidate))

; T -> Boolean
(define (out-of-border? t)
  (or (< (block-x (tetris-block t)) 0)
      (>= (block-x (tetris-block t)) WIDTH)))

; T -> Boolean
(define (under-floor? t)
  (>= (block-y (tetris-block t)) HEIGHT))

; Block -> Boolean
(define (resting? t)
  (member? (tetris-block t) (tetris-landscape t)))

; T -> T
(define (go-left t)
  (make-tetris (left-block (tetris-block t))
               (tetris-landscape t)))

(define (go-right t)
  (make-tetris (right-block (tetris-block t))
               (tetris-landscape t)))

(define (go-down t)
  (make-tetris (down-block (tetris-block t))
               (tetris-landscape t)))

; Block -> Block
(define (left-block b)
  (make-block (- (block-x b) 1)
              (block-y b)))

(define (right-block b)
  (make-block (+ (block-x b) 1)
              (block-y b)))

(define (down-block b)
  (make-block (block-x b)
              (+ (block-y b) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Game Controller
;;;;;;;;;;;;;;;;;;;;;;;;
; T Key -> T
(define (keyh t k)
  (cond
    ((string=? k "left")
     (if (or (resting? (go-left t))
             (out-of-border? (go-left t)))
         t (go-left t)))
    ((string=? k "right")
     (if (or (resting? (go-right t))
             (out-of-border? (go-right t)))
         t (go-right t)))
    ((string=? k "down")
     (if (or (resting? (go-down t))
             (under-floor? (go-down t)))
         (block-generate t)
         (go-down t)))
    (else t)))


(tetris-main tetris0 0.3)