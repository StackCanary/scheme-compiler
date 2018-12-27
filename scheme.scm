(import (rnrs arithmetic bitwise (6)))

; Emit 

(define (emit . args) (display (apply format #f args)) (newline))

; Binary Op -- TODO use logior, logiand

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
     [(add)          #t]
     [(sub)          #t]
     [(mul)          #t]
     [(div)          #t]
     [(if)           #t]
     [ else          #f])))


(define (let? x) (and (pair? x) (eqv? (car x) 'let))) 
(define (let*? x) (and (pair? x) (eqv? (car x) 'let*)))
(define (begin? x) (and (pair? x) (eqv? (car x) 'begin)))
(define (code? x) (and (pair? x) (eqv? (car x) 'code)))
(define (labels? x) (and (pair? x) (eqv? (car x) 'labels)))
(define (labelcall? x) (and (pair? x) (eqv? (car x) 'labecall)))


(define variable? symbol?)

(define (bindings x) (car (cdr x)))
(define (body x) (cddr x))

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
    [(add)          add-primcall-emitter]
    [(sub)          sub-primcall-emitter]
    [(mul)          mul-primcall-emitter]
    [(div)          div-primcall-emitter]
    [(if)           if-primcall-emitter]
    ))

;;  not,  boolean?,
;; and char?.

(define (primcall? x) (and (pair? x) (primitive? (car x))))

(define glb-label 10)

(define (get-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "var~a" (- glb-label 1))
  )

(define (lab-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "lab~a" (- glb-label 1))
  )

(define (fun-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "fun~a" (- glb-label 1))
  )

(define (add1-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = add i32 %~a, 4"      label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label2)))

(define (sub1-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = sub i32 %~a, 4"      label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label2)))

(define (char->fixnum-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = ashr i32 %~a, 4"     label2 label1)
   (emit "%~a = shl  i32 %~a, 2"     label3 label2)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (fixnum->char-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = shl i32 %~a, 2"      label2 label1)
   (emit "%~a = or  i32 %~a, 15"     label3 label2)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (fxzero?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = icmp eq i32 %~a, 0"  label2 label1)
   (emit "%~a = zext i1 %~a to i32"  label3 label2)
   (emit "%~a = shl i32 %~a, 8"      label4 label3)
   (emit "%~a = or i32 %~a, 31"      label5 label4)
   (emit "store i32 %~a, i32* %tmp"  label5)))


(define (null?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = icmp eq i32 %~a, 47" label2 label1)
   (emit "%~a = zext i1 %~a to i32"  label3 label2)
   (emit "%~a = shl i32 %~a, 8"      label4 label3)
   (emit "%~a = or i32 %~a, 31"      label5 label4)
   (emit "store i32 %~a, i32* %tmp"  label5)))

(define (fixnum?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = and i32 %~a, 3"      label2 label1)
   (emit "%~a = icmp eq i32 %~a, 0"  label3 label2)
   (emit "%~a = zext i1 %~a to i32"  label4 label3)
   (emit "%~a = shl i32 %~a, 8"      label5 label4)
   (emit "%~a = or i32 %~a, 31"      label6 label5)
   (emit "store i32 %~a, i32* %tmp"  label6)))

(define (boolean?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = and i32 %~a, 127"    label2 label1)
   (emit "%~a = icmp eq i32 %~a, 31" label3 label2)
   (emit "%~a = zext i1 %~a to i32"  label4 label3)
   (emit "%~a = shl i32 %~a, 8"      label5 label4)
   (emit "%~a = or i32 %~a, 31"      label6 label5)
   (emit "store i32 %~a, i32* %tmp"  label6)))


(define (not-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
   (emit-expr arg env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit "%~a = xor i32 %~a, 128"    label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label2)))

(define (add-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
   (emit-expr arg2 env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit-expr arg1 env)
   (emit "%~a = load i32, i32* %tmp" label2)
   (emit "%~a = add i32 %~a, %~a"    label3 label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (sub-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
   (emit-expr arg2 env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit-expr arg1 env)
   (emit "%~a = load i32, i32* %tmp" label2)
   (emit "%~a = sub i32 %~a, %~a"    label3 label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (mul-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))
   (emit-expr arg2 env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit-expr arg1 env)
   (emit "%~a = load i32, i32* %tmp" label2)
   (emit "%~a = mul  i32 %~a, %~a"    label3 label2 label1)
   (emit "%~a = sdiv i32 %~a,   4"    label4 label3)
   (emit "store i32 %~a, i32* %tmp"  label4)))

(define (div-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
   (emit-expr arg2 env)
   (emit "%~a = load i32, i32* %tmp" label1)
   (emit-expr arg1 env)
   (emit "%~a = load i32, i32* %tmp" label2)
   (emit "%~a = sdiv i32 %~a, %~a"    label3 label2 label1)
   (emit "store i32 %~a, i32* %tmp"  label3)))

(define (emit-label label)
  (emit "~a:" label))

(define (if-primcall-emitter env test conseq altern)
  (let ((L1 (lab-label))
	(L2 (lab-label))
	(L3 (lab-label))
	(label1 (get-label))
	(label2 (get-label)))
    (emit-expr test   env) ;
    (emit "%~a = load i32, i32* %tmp"  label1)
    (emit "%~a = icmp eq i32 %~a, ~a"  label2 label1 (immediate-rep #t))
    (emit "br i1 %~a, label %~a, label %~a " label2 L1 L2)
    (emit-label L1)
    (emit-expr conseq env)
    (emit "br label %~a " L3)
    (emit-label L2)
    (emit-expr altern env)
    (emit "br label %~a " L3)
    (emit-label L3)
    )
  )

(define (emit-begin x env)
  (cond
   ((null? x) '())
   ( else (begin (emit-expr (car x) env) (emit-begin (cdr x) env)) )
   ))

 
(define (emit-let bindings body env)
  (let f ((b* bindings) (new-env env))
    (cond ((null? b*) (emit-begin body new-env))
	  ( else	 (let ((b (car b*)) (label (get-label)))
			   (emit-expr (cadr b) env)
			   (emit "%~a = load i32, i32* %tmp" label)
			   (f (cdr b*)
			      (cons (list (car b) label) new-env))
			   )
			 ))))


(define (rewrite-let* bindings body env)
  (cond
   ((null? bindings) body)
   ( else (let ((b (car bindings)) (b* (cdr bindings)))
	    (if (null? b*)
		(cons 'let (cons (cons b '()) body))
		(cons 'let (cons (cons b '()) (cons (rewrite-let* b* body env) '())))
		)))
   ))


;; TODO Implement Lambdas


; Emit label expression (label ([lvar code] ..) expr)
(define (emit-lbls bindings expr)
  
					; Create a new environment mapping function names (lvars) to unique labels
  (define (make-env b)
    (cond
     ((null? b) '())
     ( else      (cons (list (caar b) (fun-label)) (make-env (cdr b)) ))
     ))

  (define (code-var c)
    (cadr c))

  (define (code-exp c)
    (caddr c))
  
  (let ((env (make-env bindings)))
					; Emit functions for each binding [lvar code]
    (for-each 					
     (lambda (binding)
       (let* ((lvar (car binding)) (code (cadr binding)) (var (code-var code)) (expr (code-exp code)) (labl (lookup lvar env)))
	 (emit-code labl var expr env)
	 )) bindings)
    
					; Emit Scheme Entry for expr
    (emit-code "scheme_entry" '() expr env) 
    )
  )

(define (emit-code labl var expr env)   ; labl - func name, var - list of func args, expr - body
  (emit-header labl (length var))	; Emit Function Header
					; Extend env to map symbols to variable locations
  (let f ((var var) (arg 0) (env env))
    (cond
     ((null? var) (emit-expr expr env)) ; Emit Function Body with new-env
     (else (f (cdr var) (+ arg 1) (cons (list (car var) (format #f "arg~a" arg)) env)))
     ))
  
  (emit-footer))			; Emit Function Footer


(define (comma-interpersed-list list)
  (cond
   ((eqv? (length list) 0) "0")
   ((eqv? (length list) 1) (format #f "~a" (car list)))
   ( else (string-append (format #f "~a, " (car list)) (comma-interpersed-list (cdr list))))
   )
  )

(define (emit-labelcall fname exprs env)
  (define (emit-args exprs arg-vars)
    (cond 
     ((null? exprs) (emit "call i32 @~a(~a)" fname (comma-interpersed-list (reverse arg-vars))))
     ( else
       (let ((label (get-label)))
	 (emit-expr (car exprs) env)
	 (emit "%~a = load i32, i32* %tmp" label)
	 (emit-args (cdr exprs) (cons label arg-vars))
	 )
       )
     )
    )

  (emit-args exprs '())
  
  )

(define (emit-primcall expr env)
  (let ((p (car expr))
	(a (cdr expr)))
       (apply (primcall-emitter p) (cons env a))))

(define (immediate-rep x)
    (cond
     ((integer? x) (shift-l x 2))
     ((char?    x) (logior (shift-l (char->integer x) 8) #x0F))
     ((boolean? x) (logior (shift-l (booltoi x)       7) #x1F))
     ((null?    x)  47)
     ))

(define (lookup x env) (cadr (assv x env)))

;; Emit Expression into a register
(define (emit-expr x env)
  (cond
   ((immediate? x) (emit "store i32 ~a, i32* %tmp" (immediate-rep x) ))
   (( primcall? x) (emit-primcall x env))
   (( variable? x) (emit "store i32 %~a, i32* %tmp" (lookup x env))) 
   ((      let? x) (emit-let  (bindings x) (body x) env))
   ((     let*? x) (emit-expr (rewrite-let* (bindings x) (body x) env) env))
   ((    begin? x) (emit-begin (cdr x) env))
   ((   labels? x) '()) ; (labels ([fname code] ...] expr)
   ((     code? x) '()) ; (  code (var ...) expr)
   ((labelcall? x) '()) ; (labelcall fname expr ...)
   )
  )

;; Compile program

(define (compile-program expr)
  (emit-header "scheme_entry" 0)
  (emit-expr expr '())
  (emit-footer))


(define (emit-argspc count_a count_b)
  (cond
   ((zero? count_a) "")
   ((eqv? 1 count_a) (format #f "i32 arg~a" count_b)) 
   ( else (string-append (format #f "i32 arg~a, " count_b) (emit-argspc (- count_a 1) (+ count_b 1))))
   ))

(define (emit-header fname count)
  (emit "define i32 @~a(~a)" fname (emit-argspc count 0))
  (emit "{")
  (emit "entry: ")
  (emit "%tmp = alloca i32"))

(define (emit-footer)
  (emit "%ret = load i32, i32* %tmp")
  (emit "ret i32 %ret")
  (emit "}"))


;; fixnum - last two bits 0, mask 11b
;; charac - taggeed with 00001111 (8 bits)
;; boolns - tagged with 0011111 (7 bits)
;; emplst - 00101111b


