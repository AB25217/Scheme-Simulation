(module event racket
  (provide event
           event?
           event-time
           event-type
           event-user
           event-lane
           event-params
           ENTER_EVENT
           CHECKOUT_EVENT
           LEAVE_EVENT
           event->string
           )

  (define ENTER_EVENT 1)
  (define CHECKOUT_EVENT 2)
  (define LEAVE_EVENT 3)

  (define event-dict
    (list (cons ENTER_EVENT "ENTER_EVENT")
          (cons CHECKOUT_EVENT "CHECKOUT_EVENT")
          (cons LEAVE_EVENT "LEAVE_EVENT")))
          

  ;; constructor.
  ;; ( time . ( id . (user . id-lane)))
  (define (event time id params)
    (cond
      [(< time 0)("Time has to be a non negative integer")]
      [(or (>= 0 id) (> id 3)) ("ID needs to be between 1 and 3") ]
      [(not (pair? params)) ("List needs to be a pair")]
      [(not (or (number? (first params)) (number?(rest params)))) ("Both the line ID and the customer ID need to be numbers")]
      [else (display (list time id params))])
    
    (raise 'event)
    )
    

  ;; P: True
(define (event? val)
  (cond
    [(not(= (length val) 3)) #f]
    [(and (pair? (third val)) (and (number? (first val)) (number? (second val)))) #t ]
    [else #f]
    )
  (raise 'event?))
  
  

  ;; P: valid event
  (define (event-time ev)
    (first ev)
    (raise 'event-time))

  ;; P: valid event
  (define (event-type ev)
    (second ev)
    (raise 'event-type))

  

  ;; P: event-params
  (define (event-params ev)
    (third ev)
    (raise 'event-params))

  (define (event-user ev)
    (first (third ev))
    (raise 'event-user))

  (define (event-lane ev)
    (second (third ev))
    (raise 'event-lane))



  (require racket/dict)
  (define (event->string ev)
    (format "~a"
            (dict-ref event-dict (event-type ev))
            ))
  )

