
(use-modules (json)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1))

(load "./utils.scm")

(define (read-dirs dir)
  (let ((f (readdir dir)))
    (if (eof-object? f)
        '()
        (cons f (read-dirs dir)))))

(define (ls-dir dir-path)
  (let* ((dir (opendir dir-path))
         (fs (read-dirs dir)))
    (closedir dir)
    fs))

(define (read path)
  (call-with-input-file path
    (lambda (port) (read-line port))))

(define cpu-path "/sys/devices/system/cpu")

(define (ls-cpus)
  (filter (lambda (s) (string-match "cpu[0-9]+" s))
          (ls-dir cpu-path)))

;; return CPU frequency in GHz
(define (read-cpu-freq cpu)
  (let* ((path (format #f
                       "/sys/devices/system/cpu/~a/cpufreq/scaling_cur_freq"
                       cpu))
         (freq-str (read path))
         (freq (string->number freq-str)))
    (/ freq 1e6)))

(define (read-cpu-freqs)
  (map (lambda (cpu)
         `(,cpu . ,(read-cpu-freq cpu)))
       (ls-cpus)))

(define thermal-zone-path "/sys/class/thermal")

(define (ls-thermal-zones)
  (filter (lambda (s) (string-contains s "thermal_zone"))
          (ls-dir thermal-zone-path)))

(define (read-thermal-zone zone)
  (let* ((zone-path (format #f
                            "~a/~a"
                            thermal-zone-path
                            zone))
         (type-path (string-append zone-path "/type"))
         (type (read type-path))
         (temp-path (string-append zone-path "/temp"))
         (temp-str (read temp-path))
         (temp (string->number temp-str)))
    `(,type . ,(/ temp 1e3))))

(define (read-thermal-zones)
  (let* ((zones (ls-thermal-zones)))
    (map read-thermal-zone zones)))

;; (json-keys json "key1" "key2" ... "keyN") is peforms the equivalent of
;; json.key1.key2...keyN in javascript
(define (json-keys json . keys)
  (fold (lambda (k alist) (assoc-ref alist k))
       json
       keys))

;; (define (cpu-temp)
;;   (let* ((pipe (open-input-pipe "sensors -j nct6796-isa-0290"))
;;          (json (json->scm pipe))
;;          (temp (json-keys json
;;                           "nct6796-isa-0290"
;;                           "CPUTIN"
;;                           "temp2_input")))
;;     (with-exception-handler
;;         (lambda (e) (log "CPU Temp: Error closing pipe: ~a\n" e))
;;       (lambda () (close-pipe pipe))
;;       #:unwind? #t)
;;     (format #f "~dC" (inexact->exact (round temp)))))



