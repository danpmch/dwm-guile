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
  (receive (response body)
      (http-get uri
                #:headers '((Content-Type . "application/json")
                            (User-Agent . "guile-web-client")))
    (if (<= 200 (response-code response) 299)
        body
        #f)))

(define (bytevector->json bytes)
  (let* ((json-str (bytevector->string bytes "utf8"))
         (json (json-string->scm json-str)))
    json))

(define (weather-get-json uri)
  (let ((response (weather-get uri)))
    (if response
        (bytevector->json response)
        #f)))

(define (get-point lat lon)
  (let* ((response (weather-get-json (format #f "https://api.weather.gov/points/~a,~a" lat lon))))
    (if response
        (assoc-ref response "properties")
        #f)))

(define (get-forecast point)
  (weather-get-json (assoc-ref point "forecastHourly")))

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

(define (run box lat lon)
  (let ((point #f))
    (while (not point)
      (sleep 60)
      (set! point (get-point lat lon)))
    (while #t
      (atomic-box-set! box (temperature-str point))
      (sleep (* 60 60)))))

(define (weather lat lon)
  (let ((box (make-atomic-box "Temp")))
    (future (run box lat lon))
    box))


