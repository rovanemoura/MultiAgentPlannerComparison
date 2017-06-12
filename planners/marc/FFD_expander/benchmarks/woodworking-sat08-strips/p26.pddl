; woodworking task with 18 parts and 100% wood
; Machines:
;   1 grinder
;   1 glazer
;   1 immersion-varnisher
;   1 planer
;   1 highspeed-saw
;   1 spray-varnisher
;   1 saw
; random seed: 518346

(define (problem wood-prob)
  (:domain woodworking)
  (:objects
    grinder0 - grinder
    glazer0 - glazer
    immersion-varnisher0 - immersion-varnisher
    planer0 - planer
    highspeed-saw0 - highspeed-saw
    spray-varnisher0 - spray-varnisher
    saw0 - saw
    white red mauve black blue green - acolour
    walnut beech cherry mahogany pine - awood
    p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 - part
    b0 b1 b2 b3 b4 b5 b6 - board
    s0 s1 s2 s3 s4 s5 s6 s7 s8 - aboardsize
  )
  (:init
    (grind-treatment-change varnished colourfragments)
    (grind-treatment-change glazed untreated)
    (grind-treatment-change untreated untreated)
    (grind-treatment-change colourfragments untreated)
    (is-smooth smooth)
    (is-smooth verysmooth)
    (= (total-cost) 0)
    (boardsize-successor s0 s1)
    (boardsize-successor s1 s2)
    (boardsize-successor s2 s3)
    (boardsize-successor s3 s4)
    (boardsize-successor s4 s5)
    (boardsize-successor s5 s6)
    (boardsize-successor s6 s7)
    (boardsize-successor s7 s8)
    (has-colour glazer0 blue)
    (has-colour glazer0 black)
    (has-colour glazer0 natural)
    (has-colour glazer0 red)
    (has-colour immersion-varnisher0 blue)
    (has-colour immersion-varnisher0 mauve)
    (has-colour immersion-varnisher0 white)
    (has-colour immersion-varnisher0 red)
    (empty highspeed-saw0)
    (has-colour spray-varnisher0 blue)
    (has-colour spray-varnisher0 mauve)
    (has-colour spray-varnisher0 white)
    (has-colour spray-varnisher0 red)
    (available p0)
    (colour p0 black)
    (wood p0 pine)
    (surface-condition p0 rough)
    (treatment p0 varnished)
    (goalsize p0 medium)
    (= (spray-varnish-cost p0) 10)
    (= (glaze-cost p0) 15)
    (= (grind-cost p0) 30)
    (= (plane-cost p0) 20)
    (unused p1)
    (goalsize p1 small)
    (= (spray-varnish-cost p1) 5)
    (= (glaze-cost p1) 10)
    (= (grind-cost p1) 15)
    (= (plane-cost p1) 10)
    (unused p2)
    (goalsize p2 medium)
    (= (spray-varnish-cost p2) 10)
    (= (glaze-cost p2) 15)
    (= (grind-cost p2) 30)
    (= (plane-cost p2) 20)
    (available p3)
    (colour p3 black)
    (wood p3 beech)
    (surface-condition p3 verysmooth)
    (treatment p3 glazed)
    (goalsize p3 small)
    (= (spray-varnish-cost p3) 5)
    (= (glaze-cost p3) 10)
    (= (grind-cost p3) 15)
    (= (plane-cost p3) 10)
    (unused p4)
    (goalsize p4 small)
    (= (spray-varnish-cost p4) 5)
    (= (glaze-cost p4) 10)
    (= (grind-cost p4) 15)
    (= (plane-cost p4) 10)
    (unused p5)
    (goalsize p5 large)
    (= (spray-varnish-cost p5) 15)
    (= (glaze-cost p5) 20)
    (= (grind-cost p5) 45)
    (= (plane-cost p5) 30)
    (unused p6)
    (goalsize p6 large)
    (= (spray-varnish-cost p6) 15)
    (= (glaze-cost p6) 20)
    (= (grind-cost p6) 45)
    (= (plane-cost p6) 30)
    (unused p7)
    (goalsize p7 medium)
    (= (spray-varnish-cost p7) 10)
    (= (glaze-cost p7) 15)
    (= (grind-cost p7) 30)
    (= (plane-cost p7) 20)
    (unused p8)
    (goalsize p8 small)
    (= (spray-varnish-cost p8) 5)
    (= (glaze-cost p8) 10)
    (= (grind-cost p8) 15)
    (= (plane-cost p8) 10)
    (unused p9)
    (goalsize p9 small)
    (= (spray-varnish-cost p9) 5)
    (= (glaze-cost p9) 10)
    (= (grind-cost p9) 15)
    (= (plane-cost p9) 10)
    (unused p10)
    (goalsize p10 small)
    (= (spray-varnish-cost p10) 5)
    (= (glaze-cost p10) 10)
    (= (grind-cost p10) 15)
    (= (plane-cost p10) 10)
    (unused p11)
    (goalsize p11 medium)
    (= (spray-varnish-cost p11) 10)
    (= (glaze-cost p11) 15)
    (= (grind-cost p11) 30)
    (= (plane-cost p11) 20)
    (available p12)
    (colour p12 blue)
    (wood p12 pine)
    (surface-condition p12 verysmooth)
    (treatment p12 colourfragments)
    (goalsize p12 small)
    (= (spray-varnish-cost p12) 5)
    (= (glaze-cost p12) 10)
    (= (grind-cost p12) 15)
    (= (plane-cost p12) 10)
    (unused p13)
    (goalsize p13 small)
    (= (spray-varnish-cost p13) 5)
    (= (glaze-cost p13) 10)
    (= (grind-cost p13) 15)
    (= (plane-cost p13) 10)
    (unused p14)
    (goalsize p14 small)
    (= (spray-varnish-cost p14) 5)
    (= (glaze-cost p14) 10)
    (= (grind-cost p14) 15)
    (= (plane-cost p14) 10)
    (unused p15)
    (goalsize p15 medium)
    (= (spray-varnish-cost p15) 10)
    (= (glaze-cost p15) 15)
    (= (grind-cost p15) 30)
    (= (plane-cost p15) 20)
    (unused p16)
    (goalsize p16 large)
    (= (spray-varnish-cost p16) 15)
    (= (glaze-cost p16) 20)
    (= (grind-cost p16) 45)
    (= (plane-cost p16) 30)
    (available p17)
    (colour p17 black)
    (wood p17 mahogany)
    (surface-condition p17 smooth)
    (treatment p17 varnished)
    (goalsize p17 medium)
    (= (spray-varnish-cost p17) 10)
    (= (glaze-cost p17) 15)
    (= (grind-cost p17) 30)
    (= (plane-cost p17) 20)
    (boardsize b0 s5)
    (wood b0 beech)
    (surface-condition b0 rough)
    (available b0)
    (boardsize b1 s3)
    (wood b1 cherry)
    (surface-condition b1 rough)
    (available b1)
    (boardsize b2 s1)
    (wood b2 mahogany)
    (surface-condition b2 rough)
    (available b2)
    (boardsize b3 s8)
    (wood b3 pine)
    (surface-condition b3 rough)
    (available b3)
    (boardsize b4 s0)
    (wood b4 pine)
    (surface-condition b4 rough)
    (available b4)
    (boardsize b5 s3)
    (wood b5 walnut)
    (surface-condition b5 rough)
    (available b5)
    (boardsize b6 s4)
    (wood b6 walnut)
    (surface-condition b6 rough)
    (available b6)
  )
  (:goal
    (and
      (available p0)
      (surface-condition p0 verysmooth)
      (treatment p0 glazed)
      (available p1)
      (colour p1 red)
      (surface-condition p1 verysmooth)
      (available p2)
      (wood p2 pine)
      (surface-condition p2 verysmooth)
      (available p3)
      (surface-condition p3 smooth)
      (treatment p3 glazed)
      (available p4)
      (colour p4 blue)
      (surface-condition p4 smooth)
      (available p5)
      (wood p5 cherry)
      (surface-condition p5 verysmooth)
      (available p6)
      (wood p6 pine)
      (surface-condition p6 smooth)
      (available p7)
      (colour p7 blue)
      (wood p7 walnut)
      (surface-condition p7 verysmooth)
      (available p8)
      (colour p8 mauve)
      (wood p8 walnut)
      (surface-condition p8 verysmooth)
      (treatment p8 varnished)
      (available p9)
      (wood p9 walnut)
      (treatment p9 glazed)
      (available p10)
      (colour p10 mauve)
      (surface-condition p10 verysmooth)
      (treatment p10 varnished)
      (available p11)
      (colour p11 black)
      (wood p11 pine)
      (surface-condition p11 verysmooth)
      (treatment p11 glazed)
      (available p12)
      (colour p12 white)
      (treatment p12 varnished)
      (available p13)
      (wood p13 walnut)
      (surface-condition p13 verysmooth)
      (treatment p13 varnished)
      (available p14)
      (colour p14 white)
      (treatment p14 varnished)
      (available p15)
      (colour p15 red)
      (wood p15 beech)
      (available p16)
      (colour p16 blue)
      (wood p16 beech)
      (surface-condition p16 smooth)
      (treatment p16 glazed)
      (available p17)
      (colour p17 natural)
      (wood p17 mahogany)
      (surface-condition p17 verysmooth)
      (treatment p17 glazed)
    )
  )
  (:metric minimize (total-cost))
)