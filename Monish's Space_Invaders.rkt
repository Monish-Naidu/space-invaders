;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Monish's Space_Invaders|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define WIDTH 500)
(define HEIGHT 800)
(define CANVAS (empty-scene WIDTH HEIGHT))
(define invader-rows 4)
(define invader-columns 9)
(define INVADER-SPACE 35)
(define MAX-SPACESHIP-BULLETS 3)
(define MAX-INVADER-BULLETS 10)
(define BULLET-RADIUS 3)
(define BULLET-SPEED 10)
(define UP  "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")
(define SPACESHIP-BULLET-DIRECTION UP)
(define INVADERS-BULLET-DIRECTION DOWN)
(define INVADER-SIZE 20)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;MAKING SPACESHIP & INVADERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct spaceship [location speed direction])
;;INTERP: A Spaceship is a (make-spaceship posn nonNegInteger nonNegInteger)

;; Deconstructor Template
;; spaceship-fn: Spaceship -> ???
#; (define (spaceship-fn spaceship)
    ... (location-fn (spaceship-location invader))...
    ... (spaceship-speed spaceship) ...
     ... (spaceship-direction spaceship)... )


(define INIT-SPACESHIP (make-spaceship
                        (make-posn 250 780)
                        25
                        LEFT))


;; Location is a Posn
;; INTERP: represents the x and y coordinates of an object

;; Speed is a nonNegInteger
;; INTERP: represents how fast an object is moving

;; A direction is one of:
; -UP
; -DOWN
; -LEFT
; -RIGHT
;; INTERP: represents the various directions





(define-struct invader [location size])
;;INTERP: An invader is a (make-invader posn nonNegInteger)

;; Deconstructor Template
;; invader-fn: Invader -> ???
#; (define (invader-fn invader)
    ... (posn-fn (invader-location invader))...
    ... (invader-size invader) ...)

;; A ListOfInvaders (LoI) is one of
;; - empty
;; - (cons Invader Invaders)
;; INTERP: represents a list of invaders

;; Deconstructor
;; invaders-fn: Invaders -> ???
#;(define (invaders-fn invaders)
    (cond
      [(empty? invaders) ... ]
      [(cons? invaders) ... (invader-fn (first invaders)) ...
                        ... (invaders-fn (rest invaders)) ... ]))




(define ROW1-INVADER (make-invader
                      (make-posn 100 20)
                      INVADER-SIZE))
(define ROW2-INVADER (make-invader
                      (make-posn 100 50)
                      INVADER-SIZE))
(define ROW3-INVADER (make-invader
                      (make-posn 100 80)
                      INVADER-SIZE))
(define ROW4-INVADER (make-invader
                      (make-posn 100 110)
                      INVADER-SIZE))


;;;; Signature
;; invader-x-value : Invader => NonNegInteger

;;;; Purpose
;; GIVEN: an Invader 
;; RETURNS: Invaders x-value

;;;; Examples
;; (invader-x-value ROW1-INVADER) => 100

(define (invader-x-value invader)
  (posn-x (invader-location invader)))

;;;; Tests
(check-expect (invader-x-value ROW1-INVADER) 100)

;;;; Signature
;; invader-y-value : Invader => NonNegInteger

;;;; Purpose
;; GIVEN: an Invader 
;; RETURNS: Invaders y-value

;;;; Examples
;; (invader-y-value ROW1-INVADER) => 20

(define (invader-y-value invader)
  (posn-y (invader-location invader)))

;;;; Tests
(check-expect (invader-y-value ROW1-INVADER) 20)



;;;; Signature
;; invader-list-maker : Invader Column => ListofInvaders

;;;; Purpose
;; GIVEN: an Invader and a column
;; RETURNS: a list of invaders

;;;; Examples
;; (invader-list-maker ROW2-INVADER 3) =>
;; (cons (make-invader (make-posn 100 50) 20) (cons (make-invader (make-posn 135 50) 20)
;; (cons (make-invader (make-posn 170 50) 20) '())))

(define (invader-list-maker invader column)
  (cond
    [(= 0 column) empty]
    [else (cons invader
                (invader-list-maker
                 (make-invader
                      (make-posn (+ (invader-x-value invader) INVADER-SPACE)
                                 (invader-y-value invader))                      
                      INVADER-SIZE)
                 (- column 1)))]))

;;;; Tests
(check-expect (invader-list-maker ROW2-INVADER 3)
              (cons (make-invader (make-posn 100 50) 20)
                    (cons (make-invader (make-posn 135 50) 20)
                          (cons (make-invader (make-posn 170 50) 20) '()))))
(check-expect (invader-list-maker ROW2-INVADER 0) empty)

(define INVADER-ARMY
                      (append (invader-list-maker ROW1-INVADER invader-columns)
                              (invader-list-maker ROW2-INVADER invader-columns)
                              (invader-list-maker ROW3-INVADER invader-columns)
                              (invader-list-maker ROW4-INVADER invader-columns)))



;;;; Signature
;; invader-image : Invader => Image

;;;; Purpose
;; GIVEN: an Invader 
;; RETURNS: the image of the invader

;;;; Examples
;; (invader-image ROW1-INVADER) => (square 5 'solid 'red)

(define (invader-image invader)
  (square (invader-size invader) 'solid 'red))

;;;; Tests
(check-expect (invader-image ROW1-INVADER)
              (square 20 'solid 'red))


;;;; Signature
;; draw-invaders : ListofInvaders Image => Image

;;;; Purpose
;; GIVEN: a list of invaders and a background
;; RETURNS: the list of invaders drawn on the background

;;;; Examples
;; (draw-invaders (cons ROW1-INVADER empty) CANVAS) =>
;;(place-image (invader-image ROW1-INVADER) 100 20 CANVAS)
             
(define (draw-invaders invaders background)
  (cond
    [(empty? invaders) background]
    [(cons? invaders) (place-image (invader-image (first invaders))
                              (invader-x-value (first invaders))
                              (invader-y-value (first invaders))
                              (draw-invaders (rest invaders) background))]))


;;;; Tests
(check-expect (draw-invaders (cons ROW1-INVADER empty) CANVAS)
              (place-image (invader-image ROW1-INVADER) 100 20 CANVAS))

(define SPACESHIP-LENGTH 40)
(define SPACESHIP-WIDTH 20)


(define spaceship-image
  (rectangle SPACESHIP-LENGTH SPACESHIP-WIDTH 'solid 'black))


;;;; Signature
;; move-spaceship-left : Spaceship => Spaceship

;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: spaceship with its position going to the left, unless at
;; already at the left side of the game, which stops the spaceship

;;;; Examples
;; (move-spaceship-left d) => (make-spaceship (make-posn 225 780) 25 "left")
;; (move-spaceship-left (make-spaceship (make-posn 0 780) 25 "left")) =>
;; (make-spaceship (make-posn 0 780) 25 "left")

(define (move-spaceship-left spaceship)
  (cond
    [(<= (spaceship-x-value spaceship)  0) (make-spaceship
                                                 (make-posn 0
                                                            (spaceship-y-value spaceship))
                                                 (spaceship-speed spaceship)
                                                 (spaceship-direction spaceship))]
    [else (make-spaceship (make-posn (- (spaceship-x-value spaceship)
                                              (spaceship-speed spaceship))
                             (spaceship-y-value spaceship))
                  (spaceship-speed spaceship)
                  LEFT)]))

;;;; Tests
(check-expect (move-spaceship-left (make-spaceship (make-posn 0 780) 25 "left"))
              (make-spaceship (make-posn 0 780) 25 "left"))
(check-expect (move-spaceship-left INIT-SPACESHIP)
              (make-spaceship (make-posn 225 780) 25 "left"))


;;;; Signature
;; move-spaceship-right : Spaceship => Spaceship

;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: spaceship with its position going to the right, unless at
;; already at the right side of the game, which stops the spaceship

;;;; Examples
;; (move-spaceship-right INIT-SPACESHIP) => (make-spaceship (make-posn 275 780) 25 "right")
;; (move-spaceship-right (make-spaceship (make-posn WIDTH 780) 25 "right")) =>
;; (make-spaceship (make-posn WIDTH 780) 25 "right")

(define (move-spaceship-right spaceship)
  (cond
    [(>= (spaceship-x-value spaceship) WIDTH) (make-spaceship
                                                 (make-posn WIDTH
                                                            (spaceship-y-value spaceship))
                                                 (spaceship-speed spaceship)
                                                 (spaceship-direction spaceship))]
    [else (make-spaceship (make-posn (+ (spaceship-x-value spaceship)
                                              (spaceship-speed spaceship))
                             (spaceship-y-value spaceship))
                  (spaceship-speed spaceship)
                  RIGHT)]))

;;;; Tests
(check-expect (move-spaceship-right (make-spaceship (make-posn WIDTH 780) 25 "right"))
              (make-spaceship (make-posn WIDTH 780) 25 "right"))
(check-expect (move-spaceship-right INIT-SPACESHIP)
              (make-spaceship (make-posn 275 780) 25 "right"))



;;;; Signature
;; move-spaceship : Spaceship => Spaceship

;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: a moved spaceship to the left or to the right

;;;; Examples
;; (move-spaceship INIT-SPACESHIP) => (make-spaceship (make-posn 225 780) 25 "left")
;; (move-spaceship (make-spaceship(make-posn 275 780) 25 "right")) =>
;; (make-spaceship (make-posn 300 780) 25 "right")

(define (move-spaceship spaceship)
  (cond
    [(string=? LEFT (spaceship-direction spaceship)) (move-spaceship-left spaceship)]
    [(string=? RIGHT (spaceship-direction spaceship)) (move-spaceship-right spaceship)]))


;;;; Tests
(check-expect (move-spaceship INIT-SPACESHIP) (make-spaceship (make-posn 225 780) 25 "left"))
(check-expect (move-spaceship  (make-spaceship(make-posn 275 780) 25 "right"))
              (make-spaceship (make-posn 300 780) 25 "right"))



;;;; Signature
;; spaceship-x-value : Spaceship => NonNegInteger

;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: spaceships x coordinate

;;;; Examples
;; (spaceship-x-value INIT-SPACESHIP) => 250

(define (spaceship-x-value spaceship)
  (posn-x (spaceship-location spaceship)))

;;;; Tests
(check-expect (spaceship-x-value INIT-SPACESHIP) 250)


;;;; Signature
;; spaceship-y-value : Spaceship => NonNegInteger

;;;; Purpose
;; GIVEN: a spaceship
;; RETURNS: spaceships y coordinate

;;;; Examples
;; (spaceship-y-value INIT-SPACESHIP) => 780

;;;; Function Definition
(define (spaceship-y-value spaceship)
  (posn-y (spaceship-location spaceship)))

;;;; Tests
(check-expect (spaceship-y-value INIT-SPACESHIP) 780)

;; A Background is an Image

;;;; Signature
;; draw-spaceship : Spaceship background => NonNegInteger

;;;; Purpose
;; GIVEN: a spaceship and a background
;; RETURNS: spaceship drawon on background

;;;; Examples
;; (draw-spaceship INIT-SPACESHIP CANVAS) =>
;; (place-image spaceship-image 250 780 CANVAS)

;;;; Function Definition
(define (draw-spaceship spaceship background)
  (place-image
   spaceship-image
   (spaceship-x-value spaceship)
   (spaceship-y-value spaceship)
   background))

;;;; Tests
(check-expect (draw-spaceship INIT-SPACESHIP CANVAS)
              (place-image spaceship-image 250 780 CANVAS))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BULLETS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct bullet [location radius direction speed])
;; A Bullet is (make-bullet Posn PosInt Direction PosInt)
;; INTERP: represents a bullet with its current location,
;;         the bullet's radius, the bullet's direction axnd
;;         the bullet's speed

;;;; Deconstructor Template
;; bullet-fn: Bullet -> ???
#; (define (bullet-fn bullet)
     ... (posn-fn(bullet-location bullet)) ...
     ... (bullet-radius bullet) ...
     ... (bullet-direction bullet) ...
     ... (bullet-speed bullet) ... )

;; a ListofBullets (LoB) is one of:
;; -empty
;; (cons bullet LoB)
;; INTERP: represents a list of bullets

;;;; Deconstructor Template
;; lob-fn: LoB -> ???
#; (define (lob-fn lob)
     (cond
       [(empty? lob) ...]
       [(cons? lob) ... bullet-fn(first lob) ...
                    ... (lob-fn (rest lob)) ...]))
     
;; A sbullets is a LoB
;; INTERP: spaceship bullets is a list of bullets

;; An ibullets is a LoB
;; INTERP: invader bullets is a list of bullets


;;;; Signature
;; draw-bullets : LoB Background => IMage

;;;; Purpose
;; GIVEN: a list of bullets and a background
;; RETURNS: the list of bullets drawon on background

;;;; Examples
;; (draw-bullets BULLET-TEST CANVAS) =>
;; (place-image (bullet-image BULLET1) 30 30 (place-image (bullet-image BULLET2) 40 40 CANVAS)


(define BULLET1 (make-bullet (make-posn 30 30) 5 UP 5))
(define BULLET2 (make-bullet (make-posn 40 40) 5 UP 5))
(define BULLET-TEST (list BULLET1 BULLET2))


;;;; Function Definition
(define (draw-bullets lob background)
  (cond
    [(empty? lob) background]    
    [(cons? lob) (place-image (bullet-image (first lob)) 
                                  (bullet-x-value(first lob))
                                  (bullet-y-value(first lob))
                                  (draw-bullets (rest lob) background))]))
;;;; Tests

(check-expect (draw-bullets BULLET-TEST CANVAS)
              (place-image (bullet-image BULLET1)
                           30 30
                           (place-image (bullet-image BULLET2)
                                        40 40
                                        CANVAS)))


;;;; Signature
;; bullet-image : Bullet => Image

;;;; Purpose
;; GIVEN: a bullet
;; RETURNS: image of bullet

;;;; Examples
;; (bullet-image BULLET1) => (circle 5 'solid 'red)

(define (bullet-image bullet)
(circle (bullet-radius bullet) 'solid 'red))

;;;; Tests
(check-expect (bullet-image BULLET1) (circle 5 'solid 'red))



;;;; Signature
;; bullet-x-value : Bullet => NonNegInteger

;;;; Purpose
;; GIVEN: a bullet
;; RETURNS: x coordinate of bullet

;;;; Examples
;; (bullet-x-value BULLET1) => 30

;;;; Function Definition
(define (bullet-x-value bullet)
  (posn-x (bullet-location bullet)))

;;;; Tests
(check-expect (bullet-x-value BULLET1) 30)

;;;; Signature
;; bullet-y-value : Bullet => NonNegInteger

;;;; Purpose
;; GIVEN: a bullet
;; RETURNS: y coordinate of bullet

;;;; Examples
;; (bullet-y-value BULLET1) => 30

;;;; Function Definition
(define (bullet-y-value bullet)
  (posn-y (bullet-location bullet)))

;;;; Tests
(check-expect (bullet-y-value BULLET1) 30)

;; move-spaceship-bullets : move each spaceship bullet in the list upwards by SPEED units
;; RETURNS: list of spaceship bullets updated by speed units


;;;; Signature
;;move-spaceship-bullets : Sbullets => Sbullets

;;;; Purpose
;; GIVEN: a a list of spaceship bullets
;; RETURNS: list of spaceship bullets updated by speed

;;;; Examples
;; (move-spaceship-bulets SBULLET-TEST) =>(cons (make-bullet (make-posn 30 25) 5 "up" 5)
;; (cons (make-bullet (make-posn 40 35) 5 "up" 5) '()))

(define SBULLET1 (make-bullet (make-posn 30 30) 5 UP 5))
(define SBULLET2 (make-bullet (make-posn 40 40) 5 UP 5))
(define SBULLET-TEST (list SBULLET1 SBULLET2))


;;;; Function Definition
(define (move-spaceship-bullets sbullets)
  (cond
    [(empty? sbullets) empty]
    [(> 0 (bullet-y-value (first sbullets))) (move-spaceship-bullets (rest sbullets))]
    [(cons? sbullets) (cons
                       (make-bullet
                        (spaceship-bmover(first sbullets))
                        (bullet-radius (first sbullets))
                        (bullet-direction (first sbullets))
                        (bullet-speed (first sbullets)))
                       (move-spaceship-bullets (rest sbullets)))]))

;;;; Tests
(check-expect (move-spaceship-bullets SBULLET-TEST)
              (cons (make-bullet (make-posn 30 25) 5 "up" 5)
                    (cons (make-bullet (make-posn 40 35) 5 "up" 5) '())))


;;;; Signature
;;spaceship-bmover : Bullet => Bullet

;;;; Purpose
;; GIVEN: a spaceship bullet
;; RETURNS: a spaceship position updated by the speed unit

;;;; Examples
;; (spaceship-bmover SBULLET1) => (make-posn 30 25) 


;;;; Function Definition
(define (spaceship-bmover bullet)
  (make-posn
   (posn-x (bullet-location bullet))
   (- (bullet-y-value bullet)
      (bullet-speed bullet))))

;;;; Tests
(check-expect (spaceship-bmover SBULLET1) (make-posn 30 25)) 

;; move-invader-bullets : move each bullet in the list downwards by SPEED units

;;;; Signature
;;move-invader-bullets : iBullets => iBullets

;;;; Purpose
;; GIVEN: a list of invader bullets
;; RETURNS: a list of invader bullets moved down by speed units

;;;; Examples
;; (move-invader-bullets IBULLET-TEST) =>(cons (make-bullet (make-posn 60 65) 5 "down" 5)
;;(cons (make-bullet (make-posn 70 75) 5 "down" 5)
;;(cons (make-bullet (make-posn 80 85) 5 "down" 5) '()))))

(define IBULLET1 (make-bullet (make-posn 60 60) 5 DOWN 5))
(define IBULLET2 (make-bullet (make-posn 70 70) 5 DOWN 5))
(define IBULLET3 (make-bullet (make-posn 80 80) 5 DOWN 5))
(define IBULLET-TEST (list IBULLET1 IBULLET2 IBULLET3))

;;;; Function Definition
(define (move-invader-bullets ibullets)
  (cond
    [(empty? ibullets) empty]
    [(< HEIGHT  (bullet-y-value (first ibullets))) (move-invader-bullets (rest ibullets))]
    [(cons? ibullets) (cons
                       (make-bullet
                        (invader-bmover(first ibullets))
                        (bullet-radius (first ibullets))
                         INVADERS-BULLET-DIRECTION
                        (bullet-speed (first ibullets)))
                       (move-invader-bullets (rest ibullets)))]))

;;;;Tests
(check-expect (move-invader-bullets IBULLET-TEST)
              (cons (make-bullet (make-posn 60 65) 5 "down" 5)
                    (cons (make-bullet (make-posn 70 75) 5 "down" 5)
                          (cons (make-bullet (make-posn 80 85) 5 "down" 5) '()))))


;;;; Signature
;;invader-bmover : Bullet => Posn

;;;; Purpose
;; GIVEN: a bullet
;; RETURNS: a bullet's posn moving down by speed units

;;;; Examples
;; (invader-bmover (make-bullet (make-posn 170 20) 3 "down" 10)) => (make-posn 170 30)

;;;; Function Definition
(define (invader-bmover bullet)
  (make-posn
   (posn-x (bullet-location bullet))
   (+ (posn-y (bullet-location bullet))
      (bullet-speed bullet))))

;;;; Tests
(check-expect (invader-bmover (make-bullet (make-posn 170 20) 3 "down" 10))
              (make-posn 170 30))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor Template:
(define-struct world [invaders spaceship ibullets sbullets])
;; a World is a (make-world ListOfInvaders Spaceship spaceship-bullets invader-bullets)
;; INTERP: represents a world with a invaders, a spaceship, and bullets

;; Deconstructor Template:
#;(define (world-fn world)
    ... (invaders-fn (world-invaders world)) ...
    ... (spaceship-fn(world-spaceship world)) ...
    ... (bullets-fn(world-ibullets world)) ...
    ... (bullets-fn(world-sbullets world)) ...)


;;;; Signature
;; draw-world : World -> Image

;;;; Purpose
;; GIVEN: a world 
;; RETURNS: an image representation of the given world 

;;;; Examples
;; (draw-world INIT-WORLD) => (draw-invaders INVADER-ARMY
;;(draw-spaceship INIT-SPACESHIP (draw-bullets empty (draw-bullets empty CANVAS))))

(define (draw-world world)
  (draw-invaders (world-invaders world)
    (draw-spaceship (world-spaceship world)
      (draw-bullets (world-ibullets world) 
        (draw-bullets (world-sbullets world) CANVAS)))))         


;;;; Tests
(check-expect (draw-world INIT-WORLD)
              (draw-invaders INVADER-ARMY
                             (draw-spaceship INIT-SPACESHIP
                                             (draw-bullets empty
                                                           (draw-bullets empty CANVAS)))))

(define INIT-WORLD (make-world
                      INVADER-ARMY
                      INIT-SPACESHIP
                      empty
                      empty))


(define DEAD-SPACESHIP (make-spaceship
                        (make-posn 0 0)
                        0
                        LEFT))



(define END-WORLD (make-world
                   INVADER-ARMY
                   DEAD-SPACESHIP
                   empty
                   empty))

    


;;;;;;;;;;;;;;;;;;;;;;;;; part of big bang key events;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; A KeyEvent is one of
;; "left"
;; "right"
;; " "
;; INTERP: Represents directional input from the keyboard


;;;; Signature
;; spaceship-key-vents: World KeyEvent -> Image

;;;; Purpose
;; GIVEN: a World and a KeyEvent
;; RETURNS: an image representation of a world representing the key event

;;;; Examples
;; (spaceship-key-events INIT-WORLD LEFT) =>(make-world INVADER-ARMY INIT-SPACESHIP empty empty))
;; (spaceship-key-events INIT-WORLD RIGHT) =>(make-world INVADER-ARMY
;;                                                      (make-spaceship (make-posn 250 780) 25 RIGHT)
;;                                                      empty empty))

;; (spaceship-key-events INIT-WORLD LEFT) =>(make-world INVADER-ARMY INIT-SPACESHIP empty
;;                                                      (add-spaceship-bullets INIT-SPACESHIP empty)))



;;;; Function Definition
(define (spaceship-key-events world key-event)
  (cond
    [(key=? key-event LEFT) (world-left-spaceship world)]
    [(key=? key-event RIGHT) (world-right-spaceship world)]
    [(key=? key-event " ") (fire-spaceship-bullet world)]
    [else world]))


;;;; Tests
(check-expect(spaceship-key-events INIT-WORLD LEFT)
             (make-world INVADER-ARMY
                         INIT-SPACESHIP
                         empty
                         empty))
(check-expect(spaceship-key-events INIT-WORLD RIGHT)
             (make-world INVADER-ARMY
                         (make-spaceship (make-posn 250 780) 25 RIGHT)
                         empty
                         empty))
(check-expect(spaceship-key-events INIT-WORLD " ")
             (make-world INVADER-ARMY
                         INIT-SPACESHIP
                         empty
                         (add-spaceship-bullets
                          INIT-SPACESHIP
                          empty)))


;;;; Signature
;; world-left-spaceship: World -> Image

;;;; Purpose
;; GIVEN: a World 
;; RETURNS: an image representation of the world 

;;;; Examples
;;  (world-left-spaceship INIT-WORLD) =>(make-world INVADER-ARMY INIT-SPACESHIP empty empty))


;;;; Function Definition
(define (world-left-spaceship world)
  (make-world (world-invaders world)
              (make-spaceship
               (spaceship-location (world-spaceship world))
               (spaceship-speed (world-spaceship world))
               LEFT)
              (world-ibullets world)
              (world-sbullets world)))

;;;; Tests
(check-expect (world-left-spaceship INIT-WORLD)
              (make-world INVADER-ARMY INIT-SPACESHIP empty empty))


;;;; Signature
;; world-right-spaceship: World -> Image

;;;; Purpose
;; GIVEN: a World 
;; RETURNS: an image representation of the world 

;;;; Examples
;;  (world-right-spaceship INIT-WORLD) =>(make-world INVADER-ARMY
;;                                                      (make-spaceship (make-posn 250 780) 25 RIGHT)
;;                                                      empty empty))


;;;; Function Definition
(define (world-right-spaceship world)
  (make-world (world-invaders world)
              (make-spaceship
               (spaceship-location (world-spaceship world))
               (spaceship-speed (world-spaceship world))
               RIGHT)
              (world-ibullets world)
              (world-sbullets world)))

;;;; Tests

(check-expect(world-right-spaceship INIT-WORLD)
             (make-world INVADER-ARMY
                         (make-spaceship (make-posn 250 780) 25 RIGHT)
                         empty
                         empty))


;;;; Signature
;; fire-spaceship-bullet: World -> Image

;;;; Purpose
;; GIVEN: a World 
;; RETURNS: an image representation of the world with a spaceship bullet fired

;;;; Examples
;;  (fire-spaceship-bullet INIT-WORLD) =>(make-world INVADER-ARMY INIT-SPACESHIP empty
;;                                                      (add-spaceship-bullets INIT-SPACESHIP empty)))

;;;; Function Definition
(define (fire-spaceship-bullet world)
  (make-world (world-invaders world)
              (world-spaceship world)
              (world-ibullets world)
              (add-spaceship-bullets
               (world-spaceship world)
               (world-sbullets world))))

;;;; Tests
(check-expect(fire-spaceship-bullet INIT-WORLD)
             (make-world INVADER-ARMY
                         INIT-SPACESHIP
                         empty
                         (add-spaceship-bullets
                          INIT-SPACESHIP
                          empty)))
             

;;;; Signature
;; add-spaceship-bullets: Spaceship Sbullets -> Sbullets

;;;; Purpose
;; GIVEN: a Spaceship and a list of bullets
;; RETURNS: an updated list of bullets with a new bullet from the location of the spaceship

;;;; Examples
;;(add-spaceship-bullets INIT-SPACESHIP empty) => (cons (make-bullet (make-posn 250 780)
;;                                                                   BULLET-RADIUS
;;                                                                   SPACESHIP-BULLET-DIRECTION
;;                                                                   BULLET-SPEED)
;;                                                       empty)


;;;; Function Definition
(define (add-spaceship-bullets spaceship sbullets)
  (cond
    [(>= (list-length sbullets) MAX-SPACESHIP-BULLETS) sbullets]
    [else (cons (make-bullet
                 (make-posn (spaceship-x-value spaceship)
                            (spaceship-y-value spaceship))
                 BULLET-RADIUS
                 SPACESHIP-BULLET-DIRECTION
                 BULLET-SPEED)
                sbullets)]))

;;;; Tests
(check-expect (add-spaceship-bullets INIT-SPACESHIP empty) (cons (make-bullet
                                                                  (make-posn 250 780)
                                                                  BULLET-RADIUS
                                                                  SPACESHIP-BULLET-DIRECTION
                                                                  BULLET-SPEED)
                                                                 empty))


;;;; Signature
;; add-invader-bullets: Invaders Ibullets -> Ibullets


;;;; Purpose
;; Given: a list of invaders and a list of bullets
;; Returns: the list of bullets updated with a new bullet

;;;; Exampless
;; can't use examples due to random function


(define (add-invader-bullets invaders ibullets)
  (cond
    [(= (list-length ibullets) MAX-INVADER-BULLETS) ibullets]
    [(= 1(random 10)) (cons (make-bullet
                 (random-invader-posn invaders)              
                 BULLET-RADIUS
                 INVADERS-BULLET-DIRECTION
                 BULLET-SPEED)
                ibullets)]
    [else ibullets]))



;;;; Tests
;; cannot test because we're using a random helper function

;;;; Signature
;; random-invader-posn: Invaders -> Posn

;;;; Purpose
;;Given: a list of invaders 
;;Returns: the position of a random invader

;;;; Examples
;; can't use examples due to random function


;;;; Function Definition
(define (random-invader-posn invaders)
  (invader-location
   (list-ref invaders (random (list-length invaders)))))

;;;; Tests
;; can't use tests due to random function


;;;; Signature
;; list-length: List -> NonNegInteger

;;;; Purpose
;;Given: a list 
;;Returns: the size of the list

;;;; Examples
;; (list-length example-list) => 3
(define example-list (list 2 "hello" #true))

;;;; Function Definition
(define (list-length list)
  (cond
    [(empty? list) 0]
    [(cons? list) (+ 1 (list-length (rest list)))]))


;;;; Tests
(check-expect (list-length example-list) 3)


;;;;;;;;;;;;;;;;;;; removing bullets & invaders ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;; Signature
;;remove-hit-invaders: Sbullets Invaders -> Invaders


;;;; Purpose
;;Given: a list of spaceship bullets and invaders
;;Returns: updated list of hit invaders removed

;;;; Examples
;; (remove-hit-invaders BULLET-TEST2 4INVADER-ROWS) =>
;; (cons (make-invader (make-posn 100 50) 20)
;; (cons (make-invader (make-posn 100 80) 20)
;; (cons (make-invader (make-posn 100 110) 20) '())))
;; (remove-hit-invaders BULLET-TEST3 4INVADER-ROWS) =>
;; (cons (make-invader (make-posn 100 30) 20)
;; (cons (make-invader (make-posn 100 50) 20)
;; (cons (make-invader (make-posn 100 80) 20)
;; (cons (make-invader (make-posn 100 110) 20) '())))

(define 4INVADER-ROWS (list ROW1-INVADER ROW2-INVADER ROW3-INVADER ROW4-INVADER))

(define BULLET3 (make-bullet (make-posn 105 20) 5 UP 5))
(define BULLET4 (make-bullet (make-posn 200 90) 5 UP 5))
(define BULLET5 (make-bullet (make-posn 0 0) 5 UP 5))
(define BULLET-TEST2 (list BULLET3 BULLET4))
(define BULLET-TEST3 (cons BULLET4(cons BULLET5 empty)))



;;;; Function Definition
(define (remove-hit-invaders sbullets invaders)
  (cond
    [(empty? sbullets) invaders]
    [(empty? invaders) empty]
    [(invader-hit? sbullets (first invaders))
     (remove-hit-invaders sbullets (rest invaders))]
    [else (cons (first invaders) (remove-hit-invaders sbullets (rest invaders)))]))

;;;; Tests

(check-expect (remove-hit-invaders BULLET-TEST2 4INVADER-ROWS)
              (list ROW2-INVADER ROW3-INVADER ROW4-INVADER))
(check-expect (remove-hit-invaders BULLET-TEST3 4INVADER-ROWS)
              (list ROW1-INVADER ROW2-INVADER ROW3-INVADER ROW4-INVADER))


;;;; Signature
;;hit-left-side?: Sbullet Invader -> Boolean


;;;; Purpose
;;Given: an invader and a spaceship bullet
;;Returns: true if bullet hits left side of spaceship, false otherwise

;;;; Examples
;; (hit-left-side? BULLET3 ROW1-INVADER) => #true
;; (hit-left-side? BULLET4 ROW1-INVADER) => #false


;;;; Function Definition
(define (hit-left-side? sbullet invader)
  (and (<= (- (invader-x-value invader) (/ (invader-size invader) 2))
           (posn-x (bullet-location sbullet)))           
       (<= (- (invader-y-value invader) (/ (invader-size invader) 2))
           (posn-y (bullet-location sbullet)))))

;;;; Tests
(check-expect (hit-left-side? BULLET3 ROW1-INVADER) #true)
(check-expect (hit-left-side? BULLET5 ROW1-INVADER) #false)


;;;; Signature
;;hit-right-side?: Sbullet Invader -> Boolean


;;;; Purpose
;;Given: an invader and a spaceship bullet
;;Returns: true if bullet hits right side of spaceship, false otherwise

;;;; Examples
;; (hit-right-side? BULLET3 ROW1-INVADER) => #true
;; (hit-right-side? BULLET4 ROW1-INVADER) => #false


;;;; Function Definition
(define (hit-right-side? sbullet invader)
  (and (>= (+ (invader-x-value invader) (/ (invader-size invader) 2))
           (posn-x (bullet-location sbullet)))           
       (>= (+ (invader-y-value invader) (/ (invader-size invader) 2))
           (posn-y (bullet-location sbullet)))))

;;;; Tests
(check-expect (hit-right-side? BULLET3 ROW1-INVADER) #true)
(check-expect (hit-right-side? BULLET4 ROW1-INVADER) #false)


;;;; Signature
;;invader-hit?: Sbullets Invader -> Boolean


;;;; Purpose
;;Given: an invader and a spaceship bullet list
;;Returns: true if bullet hits spaceship, false otherwise

;;;; Examples
;; (invader-hit? BULLET-TEST2 ROW1-INVADER) => #true
;; (invader-hit? BULLET-TEST3 ROW1-INVADER) => #false


;;;; Function Definition
(define (invader-hit? sbullets invader)
  (cond
  [(empty? sbullets) #false]  
  [(and (hit-left-side? (first sbullets) invader)
      (hit-right-side? (first sbullets) invader)) #true]
  [else (invader-hit? (rest sbullets) invader)]))

;;;; Tests
(check-expect (invader-hit? BULLET-TEST2 ROW1-INVADER) #true)
(check-expect (invader-hit? BULLET-TEST3 ROW1-INVADER) #false)



;;;; Signature
;;remove-hit-bullet: Sbullets Invaders -> Sbullets

;;;; Purpose
;;Given: a list of spaceship bullets and a list of invaders
;;Returns: updated spaceship bullet list with hit bullet removed

;;;; Examples
;; (remove-hit-bullet BULLET-TEST2 4INVADER-ROWS) =>
;; (cons BULLET4 empty)
;; (remove-hit-bullet BULLET-TEST3 4INVADER-ROWS) =>
;; BULLET-TEST3

;;;; Function Definition
(define (remove-hit-bullet sbullets invaders)
  (cond
    [(empty? sbullets) empty]
    [(cons? sbullets) (cond
           [(= (list-length invaders)
               (list-length (remove-hit-invaders sbullets invaders)))
            (cons (first sbullets) (remove-hit-bullet (rest sbullets) invaders))]
           [else (remove-hit-bullet (rest sbullets) invaders)])]))

;;;; Tests
(check-expect (remove-hit-bullet BULLET-TEST2 4INVADER-ROWS)(cons BULLET4 empty))
(check-expect (remove-hit-bullet BULLET-TEST3 4INVADER-ROWS) BULLET-TEST3)


;;;; Signature
;; hit-spacehship-left-side?: Ibullet Spaceship -> Boolean

;;;; Purpose
;;Given: an invader bullet and a spaceship
;;Returns: true if bullet hits spaceship left side, false otherwise

;;;; Examples
;; (hit-spaceship-left-side?


(define INIT-SPACESHIP (make-spaceship
                        (make-posn 250 780)
                        25
                        LEFT))

;;;; Function Definition
(define (hit-spaceship-left-side? ibullet spaceship)
  (and (<= (- (spaceship-x-value spaceship) (/ SPACESHIP-LENGTH 2))
           (posn-x (bullet-location ibullet)))           
       (<= (- (spaceship-y-value spaceship) (/ SPACESHIP-WIDTH 2))
           (posn-y (bullet-location ibullet)))))
           
(define (hit-spaceship-right-side? ibullet spaceship)
  (and (>= (+ (spaceship-x-value spaceship) (/ SPACESHIP-LENGTH 2))
           (posn-x (bullet-location ibullet)))           
       (>= (+ (spaceship-y-value spaceship) (/ SPACESHIP-WIDTH 2))
           (posn-y (bullet-location ibullet)))))


(define (spaceship-hit? ibullets spaceship)
   (cond
  [(empty? ibullets) #false]  
  [(and (hit-spaceship-left-side? (first ibullets) spaceship)
      (hit-spaceship-right-side? (first ibullets) spaceship)) #true]
  [else (spaceship-hit? (rest ibullets) spaceship)]))

(define (invaders-dead? invaders)
  (cond
    [(empty? invaders) #true]
    [else #false]))


(define (game-over world)
  (or (spaceship-hit? (world-ibullets world)
                      (world-spaceship world))
      (invaders-dead? (world-invaders world))))



;;;;;;;;;;;;;;;;;;;;;;;;;;; part of big bang on tick events;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Signature
;; update-world: World -> World

;;;; Purpose
;;Given: a World 
;;Returns: an updated world with the spaceship moving, invader bullets being fired and moved,
;; and spaceship bullets moving

;;;; Examples
;; can't run examples because of random function

;;;; Function Definition
(define (update-world world)
   (make-world
   (remove-hit-invaders
    (world-sbullets world)
    (world-invaders world))   
   (move-spaceship(world-spaceship world))
   (move-invader-bullets
    (add-invader-bullets
     (world-invaders world)
     (world-ibullets world)))
   (move-spaceship-bullets (remove-hit-bullet (world-sbullets world)
                                              (world-invaders world)))))
                                

;;;; Tests
;; can't run tests because of random function in add-invader-bullets


;(big-bang INIT-WORLD
;          (to-draw draw-world)
;          (on-key spaceship-key-events)
;          (on-tick update-world .1)
;          (stop-when game-over))
          


