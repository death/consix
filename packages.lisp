;;;; +----------------------------------------------------------------+
;;;; | CONSIX                                             DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(defpackage #:gob
  (:use #:cl #:alexandria)
  (:export
   #:draw-circle
   #:draw-star
   #:draw-cubic-curve

   #:define-symmetric
   
   #:*frames-per-second*
   #:*tick-duration*
   #:*draw-collision-shape-for-type*
   #:*draw-tick*
   
   #:vec
   #:x
   #:y
   #:copy-vec
   #:vec-assign
   #:unit
   #:vec-clear
   #:vec-mul
   #:vec+
   #:vec+=
   #:vec-
   #:vec-=
   #:vec*
   #:vec*=
   #:vec/
   #:vec/=
   #:vec-mag
   #:vec-distance
   #:vec-distance-sq
   #:vec-contains
   #:vec-contains-xy
   #:with-vec
   #:vec=~
   #:vel-vec
   #:vec-angle
   
   #:collidable-object
   #:collide-p
   #:point-collidable-object
   #:circle-collidable-object
   #:line-segment-collidable-object
   #:box-collidable-object
   #:pos
   #:collision-radius

   #:pickable-object
   #:draggable-object
   #:selectable-object
   #:clickable-object
   #:update
   #:render
   #:select
   
   #:*world*
   #:*tick*
   #:*keys*
   #:world
   #:tick
   #:add-object
   #:object-got-removed
   #:object-got-added
   #:remove-object
   #:do-objects
   #:render-collision-shape
   
   #:mouse
   #:game-window
   #:find-game-window
   #:next-world
   #:outer-world
   #:this-world-again
   #:display-text
   #:define-level))

(defpackage #:consix
  (:use #:cl #:alexandria #:gob)
  (:export))
