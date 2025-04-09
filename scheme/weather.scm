(use-modules (web client)
             (web http)
             (web response)
             (ice-9 atomic)
             (ice-9 receive)
             (ice-9 iconv)
             (ice-9 format)
             (ice-9 futures)
             (json)
             (srfi srfi-1))

(define (weather-get uri)
  (receive (_ body)
      (http-get uri
                #:headers '((Content-Type . "application/json")
                            (User-Agent . "guile-web-client")))
    body))

(define (bytevector->json bytes)
  (let* ((json-str (bytevector->string bytes "utf8"))
         (json (json-string->scm json-str)))
    json))


(define (get-point lat lon)
  (let* ((response (weather-get (format #f "https://api.weather.gov/points/~a,~a" lat lon)))
         (json (bytevector->json response)))
    (assoc-ref json "properties")))

(define (get-forecast point)
  (let* ((response (weather-get (assoc-ref point "forecastHourly")))
         (json (bytevector->json response)))
    json))

(define (get-periods forecast)
  (array-ref
   (fold (lambda (k alist) (assoc-ref alist k))
         forecast
         '("properties" "periods"))
   0))

(define (temperature point)
  (let* ((forecast (get-forecast point))
         (periods (get-periods forecast)))
    (values (assoc-ref periods "temperature")
            (assoc-ref periods "temperatureUnit"))))

(define (temperature-str point)
  (receive (temp unit)
      (temperature point)
    (format #f "~a~a" temp unit)))

(define (run-weather box point)
  (future (begin
            (atomic-box-set! box (temperature-str (touch point)))
            (sleep (* 60 60))
            (run-weather box point))))

(define (weather lat lon)
  (let* ((point (future (get-point lat lon)))
         (box (make-atomic-box "Temp...")))
    (run-weather box point)
    box))


