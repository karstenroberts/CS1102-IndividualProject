;Karsten Roberts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PROJECT REPORT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
1. To run an animation, type (run-animation animation-name) into the interactions window, where animation-name is the name of the animation. (ex. run-animation animation1)
 - animation1 (first example animation)
 - animation2 (second example animation)
 - animation3 (third example animation)
 - animation4 (my animation looping through a block of commands)

2. I have implemented all features outlined in the assignment instructions, except for specifying which side a shape should collide with to end the animation. As it stands,
if the shape collides with any side of the frame the program counts that as a collision, and moves on to the next command.

3.The one change that I made to my program was to split up the loop that takes a single command and the loop that takes a block of commands. The reason I did this was that
I felt like it made writing the animation more intuitive, and it simplified the code in the interpreter. Everything else remained the same

4.In general, I would say that I am pleased with both my interpreter and language. I feel like what could use the most improvement is my implementation of macros.
The macros I created do improve the readability of the code, but not as much as I would like. The programmer still has to have a decent understanding of how the code works
to create a new animation that functions appropriatly. 

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;LANGUAGE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 2htdp/image)
(require "world-cs1102.rkt")

;;An animation is (make-animation list[cmds])
(define-struct animation (cmds))

;;A command is a
;; - (make-movecmd string)
;; - (make-jumpcmd string)
;; - (make-deletecmd string)
;; - (make-addelementcmd element)
;; - (make-changevelocitycmd string velocity)
;; - (make-loopuntilcollisioncmd struct struct)
;; - (make-loopuntilendcmd list[struct] number)

(define-struct movecmd (a-name));;moves an element according to its velocity vector
(define-struct jumpcmd (a-name));;moves an element to a random position
(define-struct deletecmd (a-name));;deletes an element from the list of elements
(define-struct addelementcmd (element));; adds and element to the list of elements drawn to the scene
(define-struct changevelocitycmd (a-name velocity));;takes the name of an element and returns a new element with the velocity changed to resemble a bounce, and deletes the old element from the list
(define-struct loopuntilcollisioncmd (a-cmd a-collision));;loops a command until the loop condition is met. If loopcondition is string, collision with wall is assumed to be the end condition.
(define-struct loopuntilendcmd(a-loc loop-for));;loops a list of commands a given number of times

;; A list-of-commands is either
 ; - empty or
 ; - (cons a-loc empty)
  
;;An element is a (make-element posn velocity string shape)
(define-struct element (coords velocity name shape));;boolean determines if object should be removed if hit

;;a velocity is a (make-velocity num num)
(define-struct velocity (delta-x delta-y))

;;a loopcondition is either
;;- a collision or
;;- a number

;;a collision is a (make-collision string string symbol)
(define-struct collision (collision-cause collision-target collision-direction))

;;a collision-direction is either
;;- 'vertical (sides)
;;- 'horizontal (top or bottom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MACROS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax changevelocity
  (syntax-rules ()
    [(changevelocity name delta-x delta-y)
     (make-changevelocitycmd 'name (make-velocity 'delta-x 'delta-y))]))

(define-syntax addelement
  (syntax-rules (: position velocity)
    [(addelement (position : x y) (velocity : d-x d-y) name shape)
     (make-addelementcmd (make-element (make-posn 'x 'y) (make-velocity 'd-x 'd-y) 'name shape))]))

(define-syntax delete
  (syntax-rules ()
    [(delete name)
     (make-deletecmd 'name)]))

(define-syntax move
  (syntax-rules ()
    [(move name)
     (make-movecmd 'name)]))

(define-syntax collision
  (syntax-rules ()
    [(collision collision-cause collision-target collision-side)
     (make-collision collision-cause collision-target collision-side)]))

(define-syntax collisionloop
  (syntax-rules (collision-target collision-cause collision-side until :)
    [(collisionloop move-shape until (collision-cause : cause) (collision-target : target) (collision-side : side))
     (make-loopuntilcollisioncmd move-shape (collision 'cause 'target 'side))]
    [(collisionloop move-shape name)
     (make-loopuntilcollisioncmd move-shape 'name)]))

(define-syntax new-animation
  (syntax-rules ()
    [(new-animation cmds ...)
       (make-animation (list cmds ...))]))

(define-syntax jump
  (syntax-rules ()
  [(jump name)
   (make-jumpcmd 'name)]))

(define-syntax loopcmds
  (syntax-rules (cycles :)
    [(loopcmds (cmd ...) (cycles : num))
     (make-loopuntilendcmd (list cmd ...) 'num)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXAMPLES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define animation1
  (new-animation 
    (addelement (position : 50 50) (velocity : 5 1) "red circle" (circle 10 "solid" "red"))
    (addelement (position : 750 500) (velocity : 0 0) "blue rectangle" (rectangle 15 900 "solid" "blue"))
    (collisionloop (move "red circle") until (collision-cause : "red circle") (collision-target : "blue rectangle") (collision-side : 'vertical))
    (delete "blue rectangle")
    (changevelocity "red circle" -5 1)
    (collisionloop (move "red circle") "red circle")))

(define animation2
  (new-animation
    (addelement (position : 750 750) (velocity : 0 0) "purple circle" (circle 30 "solid" "purple"))
    (collisionloop (jump "purple circle") "purple circle")))
    
(define animation3
  (new-animation
    (addelement (position : 300 250) (velocity : 0 25) "orange circle" (circle 25 "solid" "orange"))
    (addelement (position : 100 750) (velocity : 0 0) "green rectangle" (rectangle 800 10 "solid" "green"))
    (collisionloop (move "orange circle") until (collision-cause : "orange circle") (collision-target : "green rectangle") (collision-side : 'horizontal))
    (addelement (position : 750 300) (velocity : 0 0) "red rectangle" (rectangle 10 1000 "solid" "red"))
    (changevelocity "orange circle" 5 0)
    (collisionloop (move "orange circle") until (collision-cause : "orange circle") (collision-target : "red rectangle") (collision-side : 'vertical))
    (jump "orange circle")))

(define animation4 ;;this animation should repeat 4 times, then stop
  (new-animation  
     (loopcmds
      (
       (addelement (position : 500 500) (velocity : 10 0) "purple circle" (circle 5 "solid" "purple"))
       (addelement (position : 50 500) (velocity : 0 0) "green rectangle" (rectangle 5 80 "solid" "green"))
       (addelement (position : 900 500) (velocity : 0 0) "blue rectangle" (rectangle 5 80 "solid" "blue"))
       (collisionloop (move "purple circle") until (collision-cause : "purple circle") (collision-target : "blue rectangle") (collision-side : 'vertical))
       (delete "blue rectangle")
       (changevelocity "purple circle" -10 0)
       (collisionloop (move "purple circle") until (collision-cause : "purple circle") (collision-target : "green rectangle") (collision-side : 'vertical))
       (delete "green rectangle")
       (addelement (position : 900 500) (velocity : 0 0) "blue rectangle" (rectangle 5 80 "solid" "blue"))
       (addelement (position : 500 500) (velocity : 0 0) "white rectangle" (rectangle 1 10000 "solid" "white"))
       (changevelocity "purple circle" 10 0)
       (collisionloop (move "purple circle") until (collision-cause : "purple circle") (collision-target : "white rectangle") (collision-side : 'vertical)))
      (cycles : 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;INTERPRETER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ELEMENTS empty)
(define WIDTH 1000)
(define HEIGHT 800)

;;run-animation: animation ->
;;Runs an animation
(define (run-animation an-animation)
  (run-cmdlist (animation-cmds an-animation))
  (set! ELEMENTS empty))

;;run-cmdlist: list[cmd] ->
;;Runs each individual command in a list of commands
(define (run-cmdlist a-loc)
  (for-each run-cmd a-loc))

;;run-cmd: cmd ->
;;Determines the type of command, and calls the appropriate helper function
(define (run-cmd a-cmd)
  (cond
    [(movecmd? a-cmd) (move-element (movecmd-a-name a-cmd))]
    [(jumpcmd? a-cmd) (jump-element (jumpcmd-a-name a-cmd))]
    [(deletecmd? a-cmd) (delete-element (deletecmd-a-name a-cmd))]
    [(addelementcmd? a-cmd) (add-element (addelementcmd-element a-cmd))]
    [(changevelocitycmd? a-cmd) (change-velocity (changevelocitycmd-a-name a-cmd) (changevelocitycmd-velocity a-cmd))]
    [(loopuntilcollisioncmd? a-cmd) (loop-until-collision (loopuntilcollisioncmd-a-cmd a-cmd) (loopuntilcollisioncmd-a-collision a-cmd))]
    [(loopuntilendcmd? a-cmd) (loop-until-end (loopuntilendcmd-a-loc a-cmd) (loopuntilendcmd-loop-for a-cmd))]))

;;add-element: element ->
;;Adds an element to the global list ELEMENTS
(define (add-element an-element)
  (begin
    (set! ELEMENTS (cons an-element ELEMENTS))
    (draw-animation)))

;;delete-element: string ->
;;Deletes a specific element from the global list ELEMENTS
(define (delete-element element-label)
  (begin
    (set! ELEMENTS (filter (lambda (element) (not (string=? (element-name element) element-label))) ELEMENTS))
    (draw-animation)))

;;jump-element: string ->
;;Changes the position of a specific element in the global list ELEMENTS to a random position within the draw panel
(define (jump-element element-label)
  (begin
    (let ([found-element (find-element element-label)]);;Conditional to see if this returns empty?
      (begin
        (delete-element element-label)
        (add-element (make-element (make-posn (random WIDTH) (random HEIGHT)) (element-velocity found-element) (element-name found-element) (element-shape found-element)))))
    (draw-animation-w-extra-pause)))

;;move-element: string ->
;;Changes the position of a specific element in the global list ELEMENTS by its velocity
(define (move-element element-label)
  (begin
    (let ([found-element (find-element element-label)])
      (begin
        (delete-element element-label)
        (add-element (make-element
                      (make-posn (+ (posn-x (element-coords found-element)) (velocity-delta-x (element-velocity found-element)))
                                 (+ (posn-y (element-coords found-element)) (velocity-delta-y (element-velocity found-element))))
                      (element-velocity found-element)
                      (element-name found-element)
                      (element-shape found-element)))))
    (draw-animation)))

;;find-element: string -> element
;;Finds the element with the name equal to the given string
(define (find-element name)
  (first (filter (λ (element) (string=? (element-name element) element-label)) ELEMENTS)))
                     
;;change-velocity: string velocity ->
;;Changes the velocity of a specific element in the global list ELEMENTS to the given velocity 
(define (change-velocity element-label new-velocity)
  (begin
    (let ([found-element (find-element element-label)])
      (begin
        (delete-element element-label)
        (add-element (make-element
                      (element-coords found-element)
                      new-velocity
                      (element-name found-element)
                      (element-shape found-element)))))
    (draw-animation)))

;;loop-until-collision: cmd collision ->
;;Loops a given command until the given element collides with the target element
(define (loop-until-collision a-cmd a-collision)
  (cond
    [(not (collision-occurrence? a-collision))
     (begin
       (run-cmd a-cmd)
       (loop-until-collision a-cmd a-collision))]))

;;collision-occurrence?: collision -> boolean
;;Returns true if there was a collision, false if not
(define (collision-occurrence? a-collision)
  (cond
    [(string? a-collision)
     (let ([shape (find-element a-collision)])
       (wall-collision? shape))]
    [else
     (let ([target (find-element (collision-collision-target a-collision))]
           [cause (find-element (collision-collision-cause a-collision))])
       (or
        (and
         (dimension-collision? cause target 'vertical)
         (dimension-collision? cause target 'horizontal))
        (wall-collision? cause)))]))

;;dimension-collision? : element element symbol -> boolean
;;Checks if two elements are colliding in a given dimension
(define (dimension-collision? cause target dimension)

  (cond
    [(symbol=? dimension 'vertical)
     (and
      (> (+ (posn-x (element-coords cause)) (find-hitbox cause dimension)) (- (posn-x (element-coords target)) (find-hitbox target dimension)))
      (< (- (posn-x (element-coords cause)) (find-hitbox cause dimension)) (+ (posn-x (element-coords target)) (find-hitbox target dimension))))]
    [(symbol=? dimension 'horizontal)
     (and
      (> (+ (posn-y (element-coords cause)) (find-hitbox cause dimension)) (- (posn-y (element-coords target)) (find-hitbox target dimension)))
      (< (- (posn-y (element-coords cause)) (find-hitbox cause dimension)) (+ (posn-y (element-coords target)) (find-hitbox target dimension))))]))

;;wall-collision? : collision -> boolean
;;Determines if an element is colliding with a wall
(define (wall-collision? an-element)
  (or
   (> (+ (posn-x (element-coords an-element)) (find-hitbox an-element 'vertical)) WIDTH)
   (< (- (posn-x (element-coords an-element)) (find-hitbox an-element 'vertical)) 0)
   (> (+ (posn-y (element-coords an-element)) (find-hitbox an-element 'horizontal))HEIGHT)
   (< (- (posn-y (element-coords an-element)) (find-hitbox an-element 'horizontal)) 0)))

;;find-hitbox: shape symbol -> number
;;Returns the distance from the center of the target edge
(define (find-hitbox a-shape a-direction)   
  (cond
    [(symbol=? a-direction 'vertical)
     (/ (image-width (element-shape a-shape)) 2)]
    [(symbol=? a-direction 'horizontal)
     (/ (image-height (element-shape a-shape)) 2)]))

;;find-element: string -> element
;;returns the element with the name equal to the given string
(define (find-element element-label)
  (first (filter (lambda (element) (string=? (element-name element) element-label)) ELEMENTS)))

;;loop-until-end: list[cmd] number ->
;;Loops a given list of commands for a given number of times
(define (loop-until-end a-loc repeat-for)
  (cond
    [(not (< repeat-for 1))
     (begin
       (run-cmdlist a-loc)
       (loop-until-end a-loc (- repeat-for 1)))]))

;;draw-animation-w-extra-pause: ->
;;Makes an extra pause before the scene is updated, used to slow down jumpcmd
(define (draw-animation-w-extra-pause)
  (sleep/yield .25)
  (draw-animation))

;; draw-animation: ->
;;Updates the scene, and draws all elements in the global ELEMENTS
(define (draw-animation)
  (sleep/yield .01)
  (let ([a-loe ELEMENTS])
      (update-frame (draw-elements a-loe))))

;;draw-elements: ->
;;Draws each element on the same scene
(define (draw-elements a-loe)
  (cond
    [(empty? a-loe) (empty-scene WIDTH HEIGHT)]
    [(cons? a-loe) 
       (place-image (element-shape (first a-loe))
                    (posn-x (element-coords (first a-loe)))
                    (posn-y (element-coords (first a-loe)))
                    (draw-elements (rest a-loe)))]))


(big-bang WIDTH HEIGHT .00003 true)

;(run-animation animation1)
;(run-animation animation2)
;(run-animation animation3)
;(run-animation animation4)