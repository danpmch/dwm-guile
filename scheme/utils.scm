
;; printing directly from scheme seems to be inconsistent, print via c stdout instead
(define-syntax-rule (log fmt args ...)
  (c-print (format #f fmt args ...)))

;; wrapper for spawn that handles duplicating the program name
(define (launch args)
  (let ((program (car args)))
    (spawn program args)))
