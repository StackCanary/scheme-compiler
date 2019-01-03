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
	 [(pair?)        #t]
	 [(cons)         #t]
	 [(car)          #t]
	 [(cdr)          #t]
	 [ else          #f])))

(define (is-labelled? expr tag) (and (pair? expr) (eqv? (car expr) tag)))

(define (let? x) (is-labelled? x 'let)) 
(define (let*? x) (is-labelled? x 'let*))
(define (begin? x) (is-labelled? x 'begin))
(define (lambda? x) (is-labelled? x 'lambda))
(define (code? x) (is-labelled? x 'code))
(define (labels? x) (is-labelled? x 'labels))
(define (labelcall? x) (is-labelled? x 'labelcall))
(define (closure? x) (is-labelled? x 'closure))


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
    [(pair?)        pair?-primcall-emitter]
    [(closure?)     closure?-primcall-emitter]
    [(symbol?)      symbol?-primcall-emitter]
    [(vector?)      vector?-primcall-emitter]
    [(string?)      string?-primcall-emitter]
    [(boolean?)     boolean?-primcall-emitter]
    [(not)          not-primcall-emitter]
    [(add)          add-primcall-emitter]
    [(sub)          sub-primcall-emitter]
    [(mul)          mul-primcall-emitter]
    [(div)          div-primcall-emitter]
    [(if)           if-primcall-emitter]
    [(cons)         cons-primcall-emitter]
    [(car)          car-primcall-emitter]
    [(cdr)          cdr-primcall-emitter]
    ))

;;  not,  boolean?,
;; and char?.

(define (primcall? x) (and (pair? x) (primitive? (car x))))

(define glb-label 10)

(define (get-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "var~a" (- glb-label 1)))

(define (lab-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "lab~a" (- glb-label 1)))

(define (fun-label)
  (set! glb-label (+ 1 glb-label))
  (format #f "fun~a" (- glb-label 1)))

(define (add1-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = add i64 %~a, 4"      label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (sub1-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = sub i64 %~a, 4"      label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (char->fixnum-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = ashr i64 %~a, 4"     label2 label1)
    (emit "%~a = shl  i64 %~a, 2"     label3 label2)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (fixnum->char-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)) (label3 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = shl i64 %~a, 2"      label2 label1)
    (emit "%~a = or  i64 %~a, 15"     label3 label2)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (fxzero?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = icmp eq i64 %~a, 0"  label2 label1)
    (emit "%~a = zext i1 %~a to i64"  label3 label2)
    (emit "%~a = shl i64 %~a, 8"      label4 label3)
    (emit "%~a = or i64 %~a, 31"      label5 label4)
    (emit "store i64 %~a, i64* %tmp"  label5)))


(define (null?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = icmp eq i64 %~a, 47" label2 label1)
    (emit "%~a = zext i1 %~a to i64"  label3 label2)
    (emit "%~a = shl i64 %~a, 8"      label4 label3)
    (emit "%~a = or i64 %~a, 31"      label5 label4)
    (emit "store i64 %~a, i64* %tmp"  label5)))

(define (last-3-bits-eq env arg bits)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = and i64 %~a, 3"      label2 label1)
    (emit "%~a = icmp eq i64 %~a, ~a" label3 label2 bits)
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 8"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)))

(define (fixnum?-primcall-emitter env arg)
  (last-3-bits-eq env arg 0))

(define (pair?-primcall-emitter env arg)
  (last-3-bits-eq env arg 1))

(define (closure?-primcall-emitter env arg)
  (last-3-bits-eq env arg 2))

(define (symbol?-primcall-emitter env arg)
  (last-3-bits-eq env arg 3))

(define (vector?-primcall-emitter env arg)
  (last-3-bits-eq env arg 5))

(define (string?-primcall-emitter env arg)
  (last-3-bits-eq env arg 6))

(define (boolean?-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label))
	(label5 (get-label)) (label6 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = and i64 %~a, 127"    label2 label1)
    (emit "%~a = icmp eq i64 %~a, 31" label3 label2)
    (emit "%~a = zext i1 %~a to i64"  label4 label3)
    (emit "%~a = shl i64 %~a, 8"      label5 label4)
    (emit "%~a = or i64 %~a, 31"      label6 label5)
    (emit "store i64 %~a, i64* %tmp"  label6)))

(define (not-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = xor i64 %~a, 128"    label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (add-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = add i64 %~a, %~a"    label3 label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (sub-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = sub i64 %~a, %~a"    label3 label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (mul-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = mul  i64 %~a, %~a"    label3 label2 label1)
    (emit "%~a = sdiv i64 %~a,   4"    label4 label3)
    (emit "store i64 %~a, i64* %tmp"  label4)))

(define (div-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)))
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = sdiv i64 %~a, %~a"    label3 label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label3)))

(define (emit-label label)
  (emit "~a:" label))

(define (if-primcall-emitter env test conseq altern)
  (let ((L1 (lab-label))
	(L2 (lab-label))
	(L3 (lab-label))
	(label1 (get-label))
	(label2 (get-label)))
    (emit-expr test   env) ;
    (emit "%~a = load i64, i64* %tmp"  label1)
    (emit "%~a = icmp eq i64 %~a, ~a"  label2 label1 (immediate-rep #t))
    (emit "br i1 %~a, label %~a, label %~a " label2 L1 L2)
    (emit-label L1)
    (emit-expr conseq env)
    (emit "br label %~a " L3)
    (emit-label L2)
    (emit-expr altern env)
    (emit "br label %~a " L3)
    (emit-label L3)
    ))

(define (emit-begin x env)
  (cond
   ((null? x) '())
   ( else (begin (emit-expr (car x) env) (emit-begin (cdr x) env)))))


(define (emit-let-help b* new-env body e)
  (cond ((null? b*) (emit-begin body new-env))
	( else	 (let ((b (car b*)) (label (get-label)))
		   (emit-expr (cadr b) e)
		   (emit "%~a = load i64, i64* %tmp" label)
		   (emit-let-help (cdr b*) (cons (list (car b) label) new-env) body e)
		   ))))

(define (emit-let bindings body env)
  (emit-let-help bindings env body env))


(define (transform-let* bindings body env)
  (cond
   ((null? bindings) body)
   ( else (let ((b (car bindings)) (b* (cdr bindings)))
	    (if (null? b*)
		(cons 'let (cons (cons b '()) body))
		(cons 'let (cons (cons b '()) (cons (transform-let* b* body env) '())))
		)))
   ))


;; TODO Implement Lambdas


					; Emit label expression (label ([lvar code] ..) expr)
(define (emit-lbls expr)
  
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

  (define (expr-bin e)
    (cadr e)
    )
  
  (let ((bindings (expr-bin expr)) (env (make-env bindings)))
					; Emit functions for each binding [lvar code]
    (for-each 					
     (lambda (binding)
       (let* ((lvar (car binding)) (code (cadr binding)) (var (code-var code)) (expr (code-exp code)) (labl (lookup lvar env)))
	 (emit-code labl var expr env)
	 )) bindings)
    
    ;; Emit Scheme Entry for expr
    (emit-code "scheme_entry" '() expr env)
    
    )
  )

;; TODO 
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
   ((eqv? (length list) 0) "")
   ((eqv? (length list) 1) (format #f "~a" (car list)))
   ( else (string-append (format #f "~a, " (car list)) (comma-interpersed-list (cdr list))))
   ))

(define (emit-labelcall lvar exprs env)
  (define (emit-args exprs arg-vars)
    (cond 
     ((null? exprs) (emit "call i64 @~a(~a)" (lookup lvar env) (comma-interpersed-list (reverse arg-vars))))
     ( else
       (let ((label (get-label)))
	 (emit-expr (car exprs) env)
	 (emit "%~a = load i64, i64* %tmp" label)
	 (emit-args (cdr exprs) (cons label arg-vars))
	 )
       )
     )
    )

  (emit-args exprs '()))

(define (emit-fcnptr count)

  (define (f c)
    (cond
     ((eqv? c 0) "")
     ((eqv? c 1) (string-append "i64" (f (- c 1))))
     ( else (string-append "i64, " (f (- c 1))))))
  
  (format #f "i64 (~a)*" (f count)))

;; TODO
;; Emit Closure Object on heap, I think I need to store the function ptr and then the values of the free variables
(define (emit-closure lvar fvar env)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))

    ;; Save hptr 
    (emit "%~a = call i64 @hptr_ptr(i64 2)" label1) ;; Get Heap Ptr for Storage on Stack
    ;; Store Function Ptr

    (emit "call void @hptr_inc(i64 ptrtoint (~a @~a to i64))" (lookup lvar env) (emit-fcnptr 42)) ;; TODO change 42 to actual function arg count
 
    (for-each
     (lambda (fv)
       (let ((fvar-label (get-label)))
	 (emit-variable fv env)
	 (emit "%~a = load i64, i64* %tmp" fvar-label)
	 (emit "call void @hptr_inc(i64 %~a)" fvar-label)     
	 )
       )
     fvar)

    (emit "store i64 %~a, i64* %tmp" label1)         ;; 
    ))

;; TODO
;; Given a lambda return the free variables
(define (free-vars expr)
  '())

;; Annotate lambdas with their free variables
;; TODO
(define (transform_a expr)
  (cond
   [(null? expr) expr]
   ))

;; Transform lambdas into labels, code and closure forms
;; TODO
(define (transform_b expr)
  '())

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

(define (emit-variable expr env)
  (emit "store i64 %~a, i64* %tmp" (lookup expr env)))

;; Emit Expression into a register
(define (emit-expr x env)
  (cond
   ((immediate? x) (emit "store i64 ~a, i64* %tmp" (immediate-rep x) ))
   (( primcall? x) (emit-primcall x env))
   (( variable? x) (emit-variable x env)) 
   ((      let? x) (emit-let  (bindings x) (body x) env))
   ((     let*? x) (emit-expr (transform-let* (bindings x) (body x) env) env))
   ((    begin? x) (emit-begin (cdr x) env))
   ((   labels? x) '()) ; (labels ([fname code] ...] expr)
   ((labelcall? x) '()) ; (labelcall lvar expr ...)
   ((  closure? x) (emit-closure (cadr x) (cddr x) env)) ; (closure lvar var ...)
   )
  )

;; Compile program

(define (compile-program expr)
  (emit-prolog)
  (if (labelcall? expr) (emit-lbls expr) (emit-code "scheme_entry" '() expr '()))
  (emit-epilog)
  )

(define (emit-argspc count_a count_b)
  (cond
   ((zero? count_a) "")
   ((eqv? 1 count_a) (format #f "i64 arg~a" count_b)) 
   ( else (string-append (format #f "i64 arg~a, " count_b) (emit-argspc (- count_a 1) (+ count_b 1))))
   ))

(define (emit-blank)
  (emit ""))

(define (emit-header fname count)
  (emit "define i64 @~a(~a)" fname (emit-argspc count 0))
  (emit "{")
  (emit "entry: ")
  (emit "%tmp = alloca i64"))

(define (emit-footer)
  (emit "%ret = load i64, i64* %tmp")
  (emit "ret i64 %ret")
  (emit "}"))

(define (emit-prolog)
  (emit "@hptr = external global i8*, align 8")
  (emit-blank))


(define (emit-epilog)
  (emit-blank)
  (emit "declare i64  @hptr_con(i64, i64) #1")
  (emit "declare void @hptr_inc(i64)      #1")
  (emit "declare i64  @hptr_ptr(i64)      #1")
  (emit "declare i64  @hptr_car(i64)      #1")
  (emit "declare i64  @hptr_cdr(i64)      #1")
  )

;; fixnum - last two bits 0, mask 11b
;; charac - taggeed with 00001111 (8 bits)
;; boolns - tagged with 0011111 (7 bits)
;; emplst - 00101111b


;; Heap Allocation

;; Tags

;; 001 - Pairs
;; 010 - Closures
;; 011 - Symbols
;; 101 - Vectors
;; 110 - Strings


;; The aim right now is to implement the datastructures Pairs, Vectors and Strings


;; In addition to the primitives required to construct them and access their data


;; The Pair and its primitives pair? [x], car[x], cdr[x] and cons[x]

(define (cons-primcall-emitter env arg1 arg2)
  (let ((label1 (get-label)) (label2 (get-label))
	(label3 (get-label)) (label4 (get-label)))
    (emit-expr arg1 env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit-expr arg2 env)
    (emit "%~a = load i64, i64* %tmp" label2)
    (emit "%~a = call i64 @hptr_con(i64 %~a, i64 %~a)" label3 label1 label2)
    (emit "store i64 %~a, i64* %tmp" label3)
))

(define (car-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = call i64 @hptr_car(i64 %~a)" label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

(define (cdr-primcall-emitter env arg)
  (let ((label1 (get-label)) (label2 (get-label)))
    (emit-expr arg env)
    (emit "%~a = load i64, i64* %tmp" label1)
    (emit "%~a = call i64 @hptr_cdr(i64 %~a)" label2 label1)
    (emit "store i64 %~a, i64* %tmp"  label2)))

;; The String and its primitives



;; The Vector and its primitives









