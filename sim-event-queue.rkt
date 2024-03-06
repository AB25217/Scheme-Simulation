#lang scheme
;; Done
(module sim-event-queue racket
  (provide
   sim-add-event
   )

  (require "sim-event.rkt")

  ;; This is a sort of "priority queue"
  ;; in O(nlog(n))
  (define (sim-add-event sim-event-queue ev )
    (append sim-event-queue ev)
    (define (sort-conditions a b)
      (cond
        [(< (first a) (first b)) #t]
        [(= (first a) (first b) ) (< (first (third a)) (first (third b)))]
        [else #f]))
    (define sim-sorted-events (sort sim-event-queue sort-conditions))
    (sim-sorted-events)
    (raise 'sim-add-event)
    )
  )

