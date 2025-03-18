
(use-modules (srfi srfi-9))

(define-record-type <view>
  (make-view num layouts)
  view?
  (num view-num)
  (layouts view-layouts set-view-layouts!))

(define (view-layout view) (car (view-layouts view)))
(define (swap-layouts view)
  (set-view-layouts! (reverse (view-layouts view))))

(define FLOATING "><>")
(define TILE "[]=")
(define MONOCLE "[M]")

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

(add-layout FLOATING #f)
(add-layout MONOCLE monocle-layout)
(add-layout TILE tile-layout)

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
(add-key modmask (get-xkey #\tab) (lambda () (view 0)))
(add-key (logior modmask (get-shiftmask))
         (get-xkey #\c)
         kill-client)
(add-key modmask (get-xkey #\t) (lambda () (set-layout TILE)))
(add-key modmask (get-xkey #\f) (lambda () (set-layout FLOATING)))
(add-key modmask (get-xkey #\m) (lambda () (set-layout MONOCLE)))

(add-key modmask (get-xkey #\space) (lambda () (set-layout "")))

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
    (add-key modmask xkey (lambda () (view flag)))
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

