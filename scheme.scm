(import (rnrs arithmetic bitwise (6)))

; Emit 

(define (emit . args) (display (apply format #f args)) (newline))

; Binary Op

(define shift-r bitwise-arithmetic-shift-right)
(define shift-l bitwise-arithmetic-shift-left )
(define (booltoi x) (if x 1 0))

; Predicates

(define (immediate? x)
  (or (integer? x) (char? x) (boolean? x) (null? x)))

(define (primitive? x)
  (and (symbol? x)
   (case x
     [(add1)         #t]
     [(sub1)         #t]
     [(char->fixnum) #t]
     [(fixnum->char) #t]
     [(fxzero?)      #t]
     [(null?)        #t]
     [(not)          #t]
     [(fixnum?)      #t]
     [(boolean?)     #t]
     [ else          #f])))

(define (primcall-emitter x)
  (case x
    [(add1)         add1-primcall-emitter]
    [(sub1)         sub1-primcall-emitter]
    [(char->fixnum) char->fixnum-primcall-emitter]
    [(fixnum->char) fixnum->char-primcall-emitter]
    [(fxzero?)      fxzero?-primcall-emitter]
    [(null?)        null?-primcall-emitter]
    [(fixnum?)      fixnum?-primcall-emitter]
    [(boolean?)     boolean?-primcall-emitter]
    [(not)          not-primcall-emitter]
    ))

;;  not,  boolean?,
;; and char?.

(define (primcall? x) (and (pair? x) (primitive? (car x))))

(define glb-label 10)

(define (get-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "var~a" (- glb-label 1))
)

(define (add1-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = add i32 %~a, 4"      label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label2)))

(define (sub1-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = sub i32 %~a, 4"      label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label2)))

(define (char->fixnum-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = ashr i32 %~a, 4"     label2 label1)
   (emit "%~a = shl  i32 %~a, 2"     label3 label2)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (fixnum->char-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = shl i32 %~a, 2"      label2 label1)
   (emit "%~a = or  i32 %~a, 15"     label3 label2)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (fxzero?-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = icmp eq i32 %~a, 0"  label2 label1)
   (emit "%~a = zext i1 %~a to i32"  label3 label2)
   (emit "%~a = shl i32 %~a, 8"      label4 label3)
   (emit "%~a = or i32 %~a, 31"      label5 label4)
   (emit "store i32 %~a, i32* %tmp"  label5)))


(define (null?-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = icmp eq i32 %~a, 47" label2 label1)
   (emit "%~a = zext i1 %~a to i32"  label3 label2)
   (emit "%~a = shl i32 %~a, 8"      label4 label3)
   (emit "%~a = or i32 %~a, 31"      label5 label4)
   (emit "store i32 %~a, i32* %tmp"  label5)))

(define (fixnum?-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = and i32 %~a, 3"      label2 label1)
   (emit "%~a = icmp eq i32 %~a, 0"  label3 label2)
   (emit "%~a = zext i1 %~a to i32"  label4 label3)
   (emit "%~a = shl i32 %~a, 8"      label5 label4)
   (emit "%~a = or i32 %~a, 31"      label6 label5)
   (emit "store i32 %~a, i32* %tmp"  label6)))

(define (boolean?-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = and i32 %~a, 127"    label2 label1)
   (emit "%~a = icmp eq i32 %~a, 31" label3 label2)
   (emit "%~a = zext i1 %~a to i32"  label4 label3)
   (emit "%~a = shl i32 %~a, 8"      label5 label4)
   (emit "%~a = or i32 %~a, 31"      label6 label5)
   (emit "store i32 %~a, i32* %tmp"  label6)))


(define (not-primcall-emitter arg)
  (let ((label1 (get-label)) (label2 (get-label)))
   (emit-expr arg)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = xor i32 %~a, 128"    label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label2)))

(define (primcall-oper x) (car (cdr x)))


(define (emit-primcall expr)
  (let ((p (car expr))
	(a (cdr expr)))
       (apply (primcall-emitter p) a)))

(define (immediate-rep x)
    (cond
     ((integer? x) (shift-l x 2))
     ((char?    x) (logior (shift-l (char->integer x) 8) #x0F))
     ((boolean? x) (logior (shift-l (booltoi x)       7) #x1F))
     ((null?    x)  47)
     ))

;; Emit Expression into a register
(define (emit-expr x)
  (cond
   ((immediate? x) (emit "store i32 ~a, i32* %tmp" (immediate-rep x) ))
   (( primcall? x) (emit-primcall x))
   )
  )

;; Compile program

(define (compile-program expr)
  (emit-header)
  (emit-expr expr)
  (emit-footer))

(define (emit-header)
  (emit "define i32 @scheme_entry()")
  (emit "{")
  (emit "%tmp = alloca i32"))

(define (emit-footer)
  (emit "%ret = load i32, i32* %tmp")
  (emit "ret i32 %ret")
  (emit "}"))


;; fixnum - last two bits 0, mask 11b
;; charac - taggeed with 00001111 (8 bits)
;; boolns - tagged with 0011111 (7 bits)
;; emplst - 00101111b


