(use-modules (ice-9 atomic)
             (ice-9 futures))

(define (date-time-str)
  (strftime "%F %R" (localtime (current-time))))

(define (run-clock c)
  (future
   (while #t
     (sleep 30)
     (atomic-box-set! c
                      (date-time-str)))))

(define (clock)
  (let* ((box (make-atomic-box (date-time-str))))
    (run-clock box)
    box))

