;; done
(module sim-lane-list racket
  (provide lane-list?
           less-crowded
           )

  (require "sim-lane.rkt")

  
  
  (define (lane-list? lst) ; recursion
    (define (check-for-customers x)
      (cond
        [(empty? x) #t]
        [else (or (number? (first x)) (null? (first x))) (check-for-customers (rest x))]))
    (cond
      [(empty? lst) #t]
      [else (and (number? (first (first lst))) (and (or (number? (first (second (first lst)))) (null? (first (second (first lst))))) (check-for-customers (second(second (first lst)))))) (lane-list? (rest lst))]))
    
    
  ;; This is a sort of "priority queue"
  ;; in O(nlog(n))
  (define (less-crowded sim-lanes )
    (first (sort sim-lanes
                 (lambda (x y)
                    (<
                     (+ (if (lane-bussy? x) 1 0) (lane-length x))
                     (+ (if (lane-bussy? y) 1 0) (lane-length y))
                    )
                    ))))
    )