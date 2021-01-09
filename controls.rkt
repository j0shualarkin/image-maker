#lang racket

(require 2htdp/universe 2htdp/image)


#|

Params : [List Any]

Object : {Nat x Nat x [Params -> Object]
         x [Params - > Image]
         x [Wave x Params -> Object] x [Wave x Params -> Object]
         x Params}
|#

;; Color :: Racket Color
;; Fill :: Racket Mode 
;; ImageFunction :: [Nat Color Fill -> Image]

;; Object ::(Object Nat Nat ImageFunction Nat Color Fill)
(struct Object [x y shape size color fill] #:transparent)


(define Obj1 (Object 50 50 circle 30 "red" "solid"))
(define Obj2 (Object 50 60 circle 20 "brown" "outline" ))
(define Obj3 (Object 2 50 square 70 "black" "solid"))
(define Obj4 (Object 50 120 circle 20 "lemonchiffon" "outline"))
(define Obj5 (Object 130 50 triangle 30 "green" "solid"))


;; define an object that is a star
;; and a line 
;; special features for when it's a line
;; -- how many, rotation,
;; penrose as a function on the arguments 

;; so the object is plane ?

;; 


#|
Field : [List Obj]
all-layers : [List Field]


|#



(define init-layers `((,Obj1 ,Obj2 ,Obj3 ,Obj4 ,Obj5)))
(struct World [field all-layers clock] #:transparent)
(define init-world (World (car init-layers) init-layers 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-object o)
  (match o
    [(Object x y shape size color fill)
     (shape size fill color)]))

(define (draw-field f)
  (match f
    ['() empty-image]
    [`(,o . ,d)
     (place-image
      (draw-object o) (Object-x o) (Object-y o) (draw-field d))]))

(define (draw-world w)
  (match w
    [(World field all clock)
     (overlay (draw-field field)
              (empty-scene 500 500))]))

(define (key-handler w i)
  (match i
    [else w]))

(define (mouse-handler w x y m)
  (match m
    ["button-down"
     (match w
       [(World field all clock)
        (World '() (cons '() all) clock)])]
    [else w]))

(define (tick-handler w)
  (match w
    [(World field all clock)
     (World field all (add1 clock))]))

(define (main)
  (big-bang init-world
    [to-draw draw-world]
    [on-key key-handler]
    [on-mouse mouse-handler]
    [on-tick tick-handler]))

(main)
