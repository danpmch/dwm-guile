
(use-modules (srfi srfi-9))

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

(define (dmenu-command)
  (let ((col-gray1 "#222222")
        (col-gray3 "#bbbbbb")
        (col-gray4 "#eeeeee")
        (col-cyan "#005577"))
    (list "dmenu_run"
          "-m" (string (integer->char (dmenu-monitor)))
          "-fn" dmenu-font
          "-nb" col-gray1
          "-nf" col-gray3
          "-sb" col-cyan
          "-sf" col-gray4)))

(define modmask (get-modmask 4))

(define (xkey c)
  (get-xkey (char->integer c)))

(add-key modmask (get-xkey #\p) (lambda () (spawn "dmenu_run" (dmenu-command))))
(add-key (logior modmask (get-shiftmask))
         (get-xkey #\newline)
         (lambda () (spawn "alacritty" '("alacritty"))))
(add-key modmask (get-xkey #\b) toggle-bar)
(add-key modmask (get-xkey #\j) (lambda () (focus-stack 1)))
(add-key modmask (get-xkey #\k) (lambda () (focus-stack -1)))
(add-key modmask (get-xkey #\i) (lambda () (increase-num-master 1)))
(add-key modmask (get-xkey #\d) (lambda () (increase-num-master -1)))
(add-key modmask (get-xkey #\h) (lambda () (adjust-master-factor -0.05)))
(add-key modmask (get-xkey #\l) (lambda () (adjust-master-factor 0.05)))
(add-key modmask (get-xkey #\newline) zoom)
(add-key modmask (get-xkey #\tab) (lambda () (switch-to-view previous-view)))
(add-key (logior modmask (get-shiftmask))
         (get-xkey #\c)
         kill-client)


(define (switch-view-layout view layout)
  (push-view-layout view layout)
  (set-layout-noarrange layout)
  (arrange (selected-monitor)))

(add-key modmask (get-xkey #\t) (lambda () (switch-view-layout current-view scm-tile-layout)))
(add-key modmask (get-xkey #\f) (lambda () (switch-view-layout current-view scm-floating-layout)))
(add-key modmask (get-xkey #\m) (lambda () (switch-view-layout current-view scm-monocle-layout)))
(add-key modmask (get-xkey #\space) (lambda ()
                                      (swap-layouts current-view)
                                      (set-layout-noarrange (view-layout current-view))
                                      (arrange (selected-monitor))))

(add-key (logior modmask (get-shiftmask))
         (get-xkey #\space)
         toggle-floating)

(add-key modmask (get-xkey #\0) (lambda () (view (lognot 0))))
(add-key (logior modmask (get-shiftmask))
         (get-xkey #\0)
         (lambda () (view (lognot 0))))

(add-key modmask (get-xkey #\,) (lambda () (focus-monitor -1)))
(add-key modmask (get-xkey #\.) (lambda () (focus-monitor 1)))

(add-key (logior modmask (get-shiftmask))
         (get-xkey #\,)
         (lambda () (tag-monitor -1)))
(add-key (logior modmask (get-shiftmask))
         (get-xkey #\.)
         (lambda () (tag-monitor 1)))

(add-key (logior modmask (get-shiftmask))
         (get-xkey #\q)
         quit)

(add-key (logior modmask (get-shiftmask)) (get-xkey #\r) load-config)

(define (tag-keys key n)
  (let ((xkey (get-xkey key))
        (flag (ash 1 n)))
    (add-key modmask xkey (lambda () (switch-to-view (list-ref views n))))
    (add-key (logior modmask (get-controlmask))
             xkey
             (lambda () (toggle-view flag)))
    (add-key (logior modmask (get-shiftmask))
             xkey
             (lambda () (tag flag)))
    (add-key (logior modmask (get-controlmask) (get-shiftmask))
             xkey
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

