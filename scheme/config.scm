
(use-modules (srfi srfi-9)
             (ice-9 match)
             (ice-9 format)
             (ice-9 atomic))

(load "./clock.scm")
(load "./weather.scm")

;; printing directly from scheme seems to be inconsistent, print via c stdout instead
(define-syntax-rule (log fmt args ...)
  (c-print (format #f fmt args ...)))

(log "Scheme working directory: ~a\n" (getcwd))

(define-record-type <view>
  (make-view num layouts)
  view?
  (num view-num)
  ;; list of two layouts, the first is current and second is previous (for toggling)
  (layouts view-layouts set-view-layouts!))

(define (view-layout view) (car (view-layouts view)))
(define (swap-layouts view)
  (set-view-layouts! view
                     (reverse (view-layouts view))))
(define (push-view-layout view layout)
  (let* ((current-layout (view-layout view))
         (new-layouts (list layout current-layout)))
    (set-view-layouts! view new-layouts)))

(define (view-flag view)
  (let* ((n (view-num view))
         (zero-based-n (- n 1))
         (flag (ash 1 zero-based-n)))
    flag))

(define FLOATING "><>")
(define TILE "[]=")
(define MONOCLE "[M]")

(define scm-floating-layout (make-layout FLOATING #f))
(define scm-tile-layout (make-layout TILE tile-layout))
(define scm-monocle-layout (make-layout MONOCLE monocle-layout))

;; create a default layout to prevent the underlying C code from
;; blowing up during initialization
(add-layout scm-tile-layout)

(define views
  (map (lambda (n)
         (make-view n (list scm-tile-layout scm-monocle-layout)))
       '(1 2 3 4 5 6 7 8 9)))

(define current-view (list-ref views 0))
(define previous-view current-view)

(define (push-view view)
  (set! previous-view current-view)
  (set! current-view view))

(define (switch-to-view v)
  (let ((layout (view-layout v))
        (flag (view-flag v)))
    (push-view v)
    (set-layout-noarrange layout)
    (view flag)))

(define font "monospace")
(define fontsize 12)
(define dmenu-font (string-append font ":size=" (number->string fontsize)))
(define fonts (list dmenu-font))

(define (dmenu-command cmd)
  (let ((col-gray1 "#222222")
        (col-gray3 "#bbbbbb")
        (col-gray4 "#eeeeee")
        (col-cyan "#005577"))
    (list cmd
          "-m" (string (integer->char (dmenu-monitor)))
          "-fn" dmenu-font
          "-nb" col-gray1
          "-nf" col-gray3
          "-sb" col-cyan
          "-sf" col-gray4)))

;; wrapper for spawn that handles duplicating the program name
(define (launch args)
  (let ((program (car args)))
    (spawn program args)))

(define modmask (get-modmask 4))
(define shiftmask (get-shiftmask))

(define (xkey c)
  (get-xkey (char->integer c)))

(define keys '())

(define (on-key keylist f)
  (let* ((total (length keylist))
         (modifiers (list-head keylist (- total 1)))
         (key (car (last-pair keylist)))
         (xkey (get-xkey key)))
    (set! keys (acons (list modifiers xkey) f keys))
    ;; still need to add-key so that the C code will register to grab
    ;; the keys. Need to replace this with key grabbing in scheme.
    (add-key (apply logior modifiers)
             (get-xkey key)
             f)
    ))

(define-syntax-rule (when-key keylist body body* ...)
  (on-key keylist (lambda () body body* ...)))

(on-key `(,modmask #\p) (lambda () (launch (dmenu-command "dmenu_run"))))
(on-key `(,modmask ,shiftmask #\p)
         (lambda () (launch (dmenu-command "passmenu"))))

(on-key `(,modmask ,shiftmask #\newline)
         (lambda () (launch '("alacritty"))))
(on-key `(,modmask #\b) toggle-bar)
(on-key `(,modmask #\j) (lambda () (focus-stack 1)))
(on-key `(,modmask #\k) (lambda () (focus-stack -1)))
(on-key `(,modmask #\i) (lambda () (increase-num-master 1)))
(on-key `(,modmask #\d) (lambda () (increase-num-master -1)))
(on-key `(,modmask #\h) (lambda () (adjust-master-factor -0.05)))
(on-key `(,modmask #\l) (lambda () (adjust-master-factor 0.05)))
(on-key `(,modmask #\newline) zoom)
(on-key `(,modmask #\tab) (lambda () (switch-to-view previous-view)))
(on-key `(,modmask ,shiftmask #\c) kill-client)

(define (switch-view-layout view layout)
  (push-view-layout view layout)
  (set-layout-noarrange layout)
  (arrange (selected-monitor)))

(on-key `(,modmask #\t) (lambda () (switch-view-layout current-view scm-tile-layout)))
(on-key `(,modmask #\f) (lambda () (switch-view-layout current-view scm-floating-layout)))
(on-key `(,modmask #\m)
         (lambda ()
           (switch-view-layout current-view scm-monocle-layout)))
(on-key `(,modmask #\space) (lambda ()
                              (swap-layouts current-view)
                              (set-layout-noarrange (view-layout current-view))
                              (arrange (selected-monitor))))

(on-key `(,modmask ,shiftmask #\space) toggle-floating)

(on-key `(,modmask #\0) (lambda () (view (lognot 0))))
(on-key `(,modmask ,shiftmask #\0)
        (lambda () (view (lognot 0))))

(on-key `(,modmask #\,) (lambda () (focus-monitor -1)))
(on-key `(,modmask #\.) (lambda () (focus-monitor 1)))

(on-key `(,modmask ,shiftmask #\,) (lambda () (tag-monitor -1)))
(on-key `(,modmask ,shiftmask #\.) (lambda () (tag-monitor 1)))

(on-key `(,modmask ,shiftmask #\q) quit)

(on-key `(,modmask ,shiftmask #\r) load-config)

(define (tag-keys key n)
  (let ((flag (ash 1 n)))
    (on-key (list modmask key) (lambda () (switch-to-view (list-ref views n))))
    (on-key (list modmask (get-controlmask) key)
            (lambda () (toggle-view flag)))
    (on-key (list modmask shiftmask key)
            (lambda () (tag flag)))
    (on-key (list modmask (get-controlmask) shiftmask key)
            (lambda () (toggle-tag flag)))))

(tag-keys #\1 0)
(tag-keys #\2 1)
(tag-keys #\3 2)
(tag-keys #\4 3)
(tag-keys #\5 4)
(tag-keys #\6 5)
(tag-keys #\7 6)
(tag-keys #\8 7)
(tag-keys #\9 8)


;; power commands
(on-key `(,modmask ,shiftmask #\s)
         (lambda () (launch '("systemctl" "suspend"))))

(define (keymap . mappings)
  (if (null? mappings)
      '()
      (let ((mapping (car mappings)))
        (let* ((keylist (car mapping))
               (modifiers (list-head keylist (- (length keylist) 1)))
               (key (car (last-pair keylist)))
               (xkey (get-xkey key))
               (f (cadr mapping))
               )
          (acons (list modifiers xkey) f
                 (apply keymap (list-tail mappings 1)))))))

;; (keymap
;;  ((list modmask shiftmask #\s) (lambda () (launch '("systemctl" "suspend")))))

;; (keymap
;;  ((list modmask shiftmask #\s) (lambda () (launch '("systemctl" "suspend"))))
;;  ((list modmask shiftmask #\r) (lambda () (launch '("systemctl" "reboow"))))
;;  ((list modmask shiftmask #\q) quit))

(define current-keys keys)

;; must be defined so that C code can call out to it
(define (keypress modifier-flags xkey)
  (let ((modifiers '()))
    (when (equal? (get-controlmask)
                  (logand modifier-flags (get-controlmask)))
      (set! modifiers (cons (get-controlmask) modifiers)))
    (when (equal? shiftmask
                  (logand modifier-flags shiftmask))
      (set! modifiers (cons shiftmask modifiers)))
    (when (equal? modmask
                  (logand modifier-flags modmask))
      (set! modifiers (cons modmask modifiers)))
    (log "keypress modifiers: ~a\n" modifiers)
    (let ((f (assoc-ref current-keys (list modifiers xkey))))
      (cond
       ((list? f) (begin
                    (log "New keys\n")
                    (set! current-keys f)))
       ((procedure? f) (begin
                         (log "Found f: ~a, resetting keys\n" f)
                         (set! current-keys keys)
                         (f)))
       (else (begin
               (log "No mapping for ~a ~a, resetting keys\n" modifiers xkey)
               (set! current-keys keys)))))))

(define (match-n n rule)
  (let ((matched 0))
    (lambda (class instance name)
      (if (< matched n)
          (let ((result (rule class instance name)))
            (when result (set! matched (+ 1 matched)))
            result)
          #f))))

(define (match-once rule) (match-n 1 rule))

(define (match-class target-class f)
  (lambda (class instance name)
    (if (equal? class target-class)
     (f)
     #f)))

(define (set-tag n)
  (make-rule n #f -1))

(define rules
  (list
   (match-once
    (match-class "Alacritty" (lambda () (set-tag 4))))
   (match-once
    (match-class "Emacs" (lambda () (set-tag 4))))
   (match-n 2
            (match-class "firefox" (lambda () (set-tag 3))))
   (match-class "discord" (lambda () (set-tag 1)))))

(define (match-first rules class instance name)
  (match rules
    (() #f)
    ((head tail ...) (let ((result (head class instance name)))
                       (if result
                           result
                           (match-first tail class instance name))))))

;; must be defined so that C code can call out to it
(define (match class instance name)
  (match-first rules class instance name))

(define main-clock (clock))
(define main-weather (weather 43 -76))

(define (bar)
  (let* ((current-clock (atomic-box-ref main-clock))
         (current-weather (atomic-box-ref main-weather)))
    (format #f " ~a | ~a "
            current-weather
            current-clock)))

(future (while #t
          ;; must sleep first to prevent accessing unintialized C members
          (sleep 15)
          (log "Scheme updating status\n")
          (update-status (bar))))

