(define (problem logistics-8-0) (:domain logistics)
  (:objects apn1 - airplane
             apt3 apt2 apt1 - airport
             pos3 pos2 pos1 - location
             cit3 cit2 cit1 - city
             tru3 tru2 tru1 - truck
             obj33 obj32 obj31 obj23 obj22 obj21 obj13 obj12 obj11 - package
            )
  (:init
       (at tru3 pos3)
       (at tru2 pos2)
       (at tru1 pos1)
       (in-city pos1 cit1)
       (in-city pos2 cit2)
       (in-city pos3 cit3)
       (in-city apt1 cit1)
       (in-city apt2 cit2)
       (in-city apt3 cit3)
       (at-pkg obj11 pos1)
       (at-pkg obj12 pos1)
       (at-pkg obj13 pos1)
       (at-pkg obj21 pos2)
       (at-pkg obj22 pos2)
       (at-pkg obj23 pos2)
       (at-pkg obj31 pos3)
       (at-pkg obj32 pos3)
       (at-pkg obj33 pos3)
       (at apn1 apt1))
  (:goal (and 
       (at-pkg obj32 apt1)
       (at-pkg obj13 apt2)
       (at-pkg obj23 apt2)
       (at-pkg obj12 pos1)
       (at-pkg obj22 pos3)
       (at-pkg obj31 apt3)
       (at-pkg obj21 pos2)
       (at-pkg obj11 pos3))))
