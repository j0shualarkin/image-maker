#lang racket

(require 2htdp/universe 2htdp/image)

#|

Params : [List Any]

Object : {Nat x Nat x [Params -> Object]
         x [Params - > Image]
         x [Wave x Params -> Object] x [Wave x Params -> Object]
         x Params}
|#

(struct Object [x y shape size color fill] #:transparent)


(define Obj1 (Object 150 50 circle 10 "red" "solid"))
(define Obj2 (Object 150 160 circle 10 "brown" "outline" ))
(define Obj3 (Object 102 150 square 10 "black" "solid"))
(define Obj4 (Object 50 120 circle 10 "lemonchiffon" "outline"))
(define Obj5 (Object 70 50 triangle 10 "green" "solid"))

#|
Field : [List Obj]
all-layers : [List Field]
|#


(struct World [field all-layers clock] #:transparent)
(define init-layers `((,Obj1 ,Obj2 ,Obj3 ,Obj4 ,Obj5)))
(define init-world (World (car init-layers) init-layers 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-object o)
  (match o
    [(Object x y shape size color fill)
     (shape size fill color)]))

(define (draw-field f)
  (match f
    ['() (empty-scene 200 200)]
    [`(,o . ,d)
     (place-image
      (draw-object o)
      (Object-x o)
      (Object-y o)
      (draw-field d))]))

(define (draw-world w)
  (draw-field (World-field w)))

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