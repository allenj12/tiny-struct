(library (tiny-struct tiny-struct)
    (export define-tiny-struct)
    (import (chezscheme))

(meta define ts-backend
  (lambda (stx)
    (syntax-case stx ()
      [(k total-bits struct-name a type bits len field-name rest ...)
       (and (eq? (syntax->datum #'a) 'array)
            (number? (syntax->datum #'bits))
            (number? (syntax->datum #'len)))
       (if (fx> (fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) 60)
        (syntax-violation 'define-tiny-struct "struct too large!" stx)
        (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          (cond 
              ((and (eq? (syntax->datum #'type) 'u)
                    (= 0 (syntax->datum #'total-bits))
                    (null? (syntax->datum #'(rest ...))))
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                          [(_ x 0)
                          #`(fxand x #,(sub1 (expt 2 (syntax->datum #'bits))))]
                          [(_ x #,(fx1- (syntax->datum #'len)))
                          #`(fxarithmetic-shift-right x #,(fx* (syntax->datum #'bits) (fx1- (syntax->datum #'len))))]
                          [(_ x idx)
                           (number? (syntax->datum #'idx))
                          #`(fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits))))]
                          [(_ x idx)
                          #`(fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx)
                        (syntax-case stx ()
                        [(_ x 0 new)
                         #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) (sub1 (expt 2 (syntax->datum #'bits))))) new)]
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                    #,(fxnot (fxarithmetic-shift-left 
                                                (sub1 (expt 2 (syntax->datum #'bits)))
                                                (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                  (fxarithmetic-shift-left new #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                      (fxnot (fxarithmetic-shift-left 
                                                  #,(sub1 (expt 2 (syntax->datum #'bits)))
                                                  (fx+ (fx* idx bits) total-bits))))
                                  (fxarithmetic-shift-left new (fx+ (fx* idx bits) total-bits)))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))
              ((and (eq? (syntax->datum #'type) 'u)
                    (= 0 (syntax->datum #'total-bits)))
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                          [(_ x 0)
                          #`(fxand x #,(sub1 (expt 2 (syntax->datum #'bits))))]
                          [(_ x idx)
                           (number? (syntax->datum #'idx))
                          #`(fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits))))]
                          [(_ x idx)
                          #`(fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx)
                        (syntax-case stx ()
                        [(_ x 0 new)
                         #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) (sub1 (expt 2 (syntax->datum #'bits))))) new)]
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                    #,(fxnot (fxarithmetic-shift-left 
                                                (sub1 (expt 2 (syntax->datum #'bits)))
                                                (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                  (fxarithmetic-shift-left new #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                      (fxnot (fxarithmetic-shift-left 
                                                  #,(sub1 (expt 2 (syntax->datum #'bits)))
                                                  (fx+ (fx* idx bits) total-bits))))
                                  (fxarithmetic-shift-left new (fx+ (fx* idx bits) total-bits)))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))
              ((and (eq? (syntax->datum #'type) 'u)
                    (null? (syntax->datum #'(rest ...))))
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                          [(_ x #,(fx1- (syntax->datum #'len)))
                           #`(fxarithmetic-shift-right x #,(fx+ (fx* (fx1- (syntax->datum #'len)) (syntax->datum #'bits)) (syntax->datum #'total-bits)))]
                          [(_ x idx)
                           (number? (syntax->datum #'idx))
                          #`(fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits))))]
                          [(_ x idx)
                          #`(fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx)
                        (syntax-case stx ()
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                    #,(fxnot (fxarithmetic-shift-left 
                                                (sub1 (expt 2 (syntax->datum #'bits)))
                                                (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                  (fxarithmetic-shift-left new #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                      (fxnot (fxarithmetic-shift-left 
                                                  #,(sub1 (expt 2 (syntax->datum #'bits)))
                                                  (fx+ (fx* idx bits) total-bits))))
                                  (fxarithmetic-shift-left new (fx+ (fx* idx bits) total-bits)))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))
              ((eq? (syntax->datum #'type) 'u)
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                          [(_ x idx)
                           (number? (syntax->datum #'idx))
                          #`(fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits))))]
                          [(_ x idx)
                          #`(fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx)
                        (syntax-case stx ()
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                    #,(fxnot (fxarithmetic-shift-left 
                                                (sub1 (expt 2 (syntax->datum #'bits)))
                                                (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                  (fxarithmetic-shift-left new #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                      (fxnot (fxarithmetic-shift-left 
                                                  #,(sub1 (expt 2 (syntax->datum #'bits)))
                                                  (fx+ (fx* idx bits) total-bits))))
                                  (fxarithmetic-shift-left new (fx+ (fx* idx bits) total-bits)))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))

              ((and (eq? (syntax->datum #'type) 's)
                    (= 0 (syntax->datum #'total-bits))
                    (null? (syntax->datum #'(rest ...))))
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                         [(_ x 0)
                          #`(fx- (fxand x #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                         [(_ x #,(fx1- (syntax->datum #'len)))
                          #`(fx- (fxarithmetic-shift-right x #,(fx+ (fx* (fx1- (syntax->datum #'len)) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                         [(_ x idx)
                          (number? (syntax->datum #'idx))
                          #`(fx- (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                        [(_ x idx)
                         #`(fx- (fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx) 
                        (syntax-case stx ()
                        [(_ x 0 new)
                         #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) (sub1 (expt 2 (syntax->datum #'bits))))) 
                                    (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))))]
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                      #,(fxnot 
                                          (fxarithmetic-shift-left 
                                              (sub1 (expt 2 (syntax->datum #'bits))) 
                                              (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                      (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                        (fxnot 
                                          (fxarithmetic-shift-left 
                                            #,(sub1 (expt 2 (syntax->datum #'bits))) 
                                           (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits)))))
                                        (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits))))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))

              ((and (eq? (syntax->datum #'type) 's)
                    (= 0 (syntax->datum #'total-bits)))

                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                         [(_ x 0)
                          #`(fx- (fxand x #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                         [(_ x idx)
                          (number? (syntax->datum #'idx))
                          #`(fx- (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                        [(_ x idx)
                         #`(fx- (fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx) 
                        (syntax-case stx ()
                        #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) (sub1 (expt 2 (syntax->datum #'bits))))) 
                                    (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))))
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                      #,(fxnot 
                                          (fxarithmetic-shift-left 
                                              (sub1 (expt 2 (syntax->datum #'bits))) 
                                              (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                      (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                        (fxnot 
                                          (fxarithmetic-shift-left 
                                            #,(sub1 (expt 2 (syntax->datum #'bits))) 
                                           (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits)))))
                                        (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits))))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))
              
              ((and (eq? (syntax->datum #'type) 's)
                    (null? (syntax->datum #'(rest ...))))
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                         [(_ x #,(fx1- (syntax->datum #'len)))
                          #`(fx- (fxarithmetic-shift-right x #,(fx+ (fx* (fx1- (syntax->datum #'len)) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                         [(_ x idx)
                          (number? (syntax->datum #'idx))
                          #`(fx- (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                        [(_ x idx)
                         #`(fx- (fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx) 
                        (syntax-case stx ()
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                      #,(fxnot 
                                          (fxarithmetic-shift-left 
                                              (sub1 (expt 2 (syntax->datum #'bits))) 
                                              (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                      (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                        (fxnot 
                                          (fxarithmetic-shift-left 
                                            #,(sub1 (expt 2 (syntax->datum #'bits))) 
                                           (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits)))))
                                        (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits))))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))

              ((eq? (syntax->datum #'type) 's)
                    
                #`(begin
                    (define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                         [(_ x idx)
                          (number? (syntax->datum #'idx))
                          #`(fx- (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]
                        [(_ x idx)
                         #`(fx- (fxand (fxarithmetic-shift-right x (fx+ (fx* idx bits) total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))])))
                    (define-syntax setter-name
                      (lambda (stx) 
                        (syntax-case stx ()
                        [(_ x idx new)
                         (number? (syntax->datum #'idx))
                         #`(fxlogor (fxand x 
                                      #,(fxnot 
                                          (fxarithmetic-shift-left 
                                              (sub1 (expt 2 (syntax->datum #'bits))) 
                                              (fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits)))))
                                      (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) #,(fx+ (fx* (syntax->datum #'idx) (syntax->datum #'bits)) (syntax->datum #'total-bits))))]
                        [(_ x idx new)
                         #`(fxlogor (fxand x 
                                        (fxnot 
                                          (fxarithmetic-shift-left 
                                            #,(sub1 (expt 2 (syntax->datum #'bits))) 
                                           (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits)))))
                                        (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) (fx+ (fx* idx #,(syntax->datum #'bits)) #,(syntax->datum #'total-bits))))])))
                    #,(ts-backend #`(k #,(fx+ (fx* (syntax->datum #'bits) (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))))))]
      [(k total-bits struct-name a type len field-name rest ...)
       (and (eq? (syntax->datum #'a) 'array)
            (eq? (syntax->datum #'type) 'bool)
            (number? (syntax->datum #'len)))
       (if (fx> (fx+ (syntax->datum #'len) (syntax->datum #'total-bits)) 60)
        (syntax-violation 'define-tiny-struct "struct too large!" stx)
        (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          #`(begin
              (define-syntax getter-name
                (lambda (stx) 
                  (syntax-case stx ()
                    [(_ x idx)
                     (number? (syntax->datum #'idx))
                     #`(fxbit-set? x #,(fx+ (syntax->datum #'idx) (syntax->datum #'total-bits)))]
                    [(_ x idx)
                     #`(fxbit-set? x (fx+ idx total-bits))])))
              (define-syntax setter-name
                (lambda (stx) 
                  (syntax-case stx ()
                  [(_ x idx new)
                   (number? (syntax->datum #'idx))
                   #`(if new
                      (fxlogbit1 #,(fx+ (syntax->datum #'idx) (syntax->datum #'total-bits)) x)
                      (fxlogbit0 #,(fx+ (syntax->datum #'idx) (syntax->datum #'total-bits)) x))]
                  [(_ x idx new)
                   #`(if new
                      (fxlogbit1 (fx+ idx #,(syntax->datum #'total-bits)) x)
                      (fxlogbit0 (fx+ idx #,(syntax->datum #'total-bits)) x))])))
            #,(ts-backend #`(k #,(fx+ (syntax->datum #'len) (syntax->datum #'total-bits)) struct-name rest ...))))))]
      [(k total-bits struct-name a type len field-name rest ...)
       (and (eq? (syntax->datum #'a) 'array)
            (eq? (syntax->datum #'type) 'char)
            (number? (syntax->datum #'len)))
       (if (fx> (fx+ (fx* 8 (syntax->datum #'len)) (syntax->datum #'total-bits)) 60)
        (syntax-violation 'define-tiny-struct "struct too large!" stx)
        (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          #`(begin
            #,(cond
                ((and (= 0 (syntax->datum #'total-bits))
                      (null? (syntax->datum #'(rest ...))))
                #`(define-syntax getter-name
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ x 0)
                        #`(integer->char (fxand x 255))]
                        [(_ x #,(fx1- (syntax->datum #'len)))
                        #`(integer->char (fxarithmetic-shift-right x #,(fx+ (fx* (fx1- (syntax->datum #'len)) 8) (syntax->datum #'total-bits))))]
                        [(_ x idx)
                        (number? (syntax->datum #'idx))
                        #`(integer->char (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits))) 255))]
                        [(_ x idx)
                        #`(integer->char (fxand (fxarithmetic-shift-right x (fx+ (fx* idx 8) #,(syntax->datum #'total-bits))) 255))]))))
                ((= 0 (syntax->datum #'total-bits))
                 #`(define-syntax getter-name
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ x 0)
                        #`(integer->char (fxand x 255))]
                        [(_ x idx)
                        (number? (syntax->datum #'idx))
                        #`(integer->char (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits))) 255))]
                        [(_ x idx)
                        #`(integer->char (fxand (fxarithmetic-shift-right x (fx+ (fx* idx 8) #,(syntax->datum #'total-bits))) 255))]))))
                ((null? (syntax->datum #'(rest ...)))
                 #`(define-syntax getter-name
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ x #,(fx1- (syntax->datum #'len)))
                        #`(integer->char (fxarithmetic-shift-right x #,(fx+ (fx* (fx1- (syntax->datum #'len)) 8) (syntax->datum #'total-bits))))]
                        [(_ x idx)
                        (number? (syntax->datum #'idx))
                        #`(integer->char (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits))) 255))]
                        [(_ x idx)
                        #`(integer->char (fxand (fxarithmetic-shift-right x (fx+ (fx* idx 8) #,(syntax->datum #'total-bits))) 255))]))))
                (else
                 #`(define-syntax getter-name
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ x idx)
                        (number? (syntax->datum #'idx))
                        #`(integer->char (fxand (fxarithmetic-shift-right x #,(fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits))) 255))]
                        [(_ x idx)
                        #`(integer->char (fxand (fxarithmetic-shift-right x (fx+ (fx* idx 8) #,(syntax->datum #'total-bits))) 255))])))))
            #,(cond
                ((= 0 (syntax->datum #'total-bits))
                #`(define-syntax setter-name
                    (lambda (stx)
                      (syntax-case stx ()
                      [(_ x 0 new)
                       #`(fxlogor (fxand x 
                                      #,(fxnot (fxarithmetic-shift-left 
                                                255
                                                (syntax->datum #'total-bits))))
                                 (char->integer new))]
                      [(_ x idx new)
                        (number? (syntax->datum #'idx))
                        #`(fxlogor (fxand x 
                                      #,(fxnot (fxarithmetic-shift-left 
                                                255
                                                (fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits)))))
                                (fxarithmetic-shift-left (char->integer new) #,(fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits))))]
                      [(_ x idx new)
                        #`(let ([calc-total-bits (fx+ (fx* idx 8) #,(syntax->datum #'total-bits))])
                            (fxlogor (fxand x 
                                          (fxnot (fxarithmetic-shift-left 
                                                    255
                                                    calc-total-bits)))
                                    (fxarithmetic-shift-left (char->integer new) calc-total-bits)))]))))
                  (else
                   #`(define-syntax setter-name
                    (lambda (stx)
                      (syntax-case stx ()
                      [(_ x idx new)
                        (number? (syntax->datum #'idx))
                        #`(fxlogor (fxand x 
                                      #,(fxnot (fxarithmetic-shift-left 
                                                255
                                                (fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits)))))
                                (fxarithmetic-shift-left (char->integer new) #,(fx+ (fx* (syntax->datum #'idx) 8) (syntax->datum #'total-bits))))]
                      [(_ x idx new)
                        #`(let ([calc-total-bits (fx+ (fx* idx 8) #,(syntax->datum #'total-bits))])
                            (fxlogor (fxand x 
                                          (fxnot (fxarithmetic-shift-left 
                                                    255
                                                    calc-total-bits)))
                                    (fxarithmetic-shift-left (char->integer new) calc-total-bits)))])))))
            #,(ts-backend #`(k #,(fx+ (fx* 8 (syntax->datum #'len)) (syntax->datum #'total-bits)) struct-name rest ...))))))]
      [(k total-bits struct-name type field-name rest ...)
       (eq? (syntax->datum #'type) 'bool)
       (if (fx> (fx1+ (syntax->datum #'total-bits)) 60)
        (syntax-violation 'define-tiny-struct "struct too large!" stx)
        (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          #`(begin
              (define-syntax getter-name
                (lambda (stx) 
                  (syntax-case stx ()
                  [(_ x)
                   #`(fxbit-set? x #,(syntax->datum #'total-bits))])))
              (define-syntax setter-name
                (lambda (stx)
                  (syntax-case stx ()
                  [(_ x new)
                   #`(if new
                      (fxlogbit1 total-bits x)
                      (fxlogbit0 total-bits x))])))
            #,(ts-backend #`(k #,(fx1+ (syntax->datum #'total-bits)) struct-name rest ...))))))]
      [(k total-bits struct-name type field-name rest ...)
       (eq? (syntax->datum #'type) 'char)
       (if (fx> (fx+ 8 (syntax->datum #'total-bits)) 60)
        (syntax-violation 'define-tiny-struct "struct too large!" stx)
        (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          #`(begin
              #,(cond
                  ((= 0 (syntax->datum #'total-bits))
                  #`(define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                         [(_ x)
                          #`(integer->char (fxand x 255))]))))
                  ((null? (syntax->datum #'(rest ...)))
                  #`(define-syntax getter-name
                      (lambda (stx) 
                        (syntax-case stx ()
                        [(_ x)
                        #`(integer->char (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)))]))))
                  (else
                  #`(define-syntax getter-name
                      (lambda (stx)
                        (syntax-case stx ()
                          [(_ x)
                           #`(integer->char (fxand (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) 255))])))))
              #,(cond
                  ((= 0 (syntax->datum #'total-bits))
                  #`(define-syntax setter-name
                      (lambda (stx) 
                        (syntax-case stx ()
                          [(_ x new)
                           #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) 255)) (char->integer new))]))))
                  (else 
                    #`(define-syntax setter-name
                        (lambda (stx) 
                          (syntax-case stx ()
                          [(_ x new)
                           #`(fxlogor (fxand x 
                                      #,(fxnot (fxarithmetic-shift-left 
                                                  255
                                                  (syntax->datum #'total-bits))))
                                  (fxarithmetic-shift-left (char->integer new) total-bits))])))))
              #,(ts-backend #`(k #,(fx+ 8 (syntax->datum #'total-bits)) struct-name rest ...))))))]
        [(k total-bits struct-name type bits field-name rest ...)
        (if (fx> (fx+ (syntax->datum #'bits) (syntax->datum #'total-bits)) 60)
          (syntax-violation 'define-tiny-struct "struct too large!" stx)
          (let ([base (string-append 
                        (symbol->string (syntax->datum #'struct-name))
                        "-" 
                        (symbol->string (syntax->datum #'field-name)))])
          (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                        [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
            (cond 
              ((and (number? (syntax->datum #'bits))
                    (eq? (syntax->datum #'type) 'u))
                #`(begin
                    #,(cond 
                        ((= 0 (syntax->datum #'total-bits))
                         #`(define-syntax getter-name
                            (lambda (stx)
                              (syntax-case stx ()
                              [(_ x)
                               #`(fxand x #,(sub1 (expt 2 (syntax->datum #'bits))))]))))
                        ((null? (syntax->datum #'(rest ...)))
                        #`(define-syntax getter-name
                            (lambda (stx)
                              (syntax-case stx ()
                              [(_ x)
                               #`(fxarithmetic-shift-right x #,(syntax->datum #'total-bits))]))))
                        (else
                        #`(define-syntax getter-name
                            (lambda (stx) 
                              (syntax-case stx ()
                              [(_ x)
                               #`(fxand (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))])))))
                    #,(cond
                        ((= 0 (syntax->datum #'total-bits))
                         #`(define-syntax setter-name
                            (lambda (stx)
                              (syntax-case stx ()
                              [(_ x new)
                               #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) (sub1 (expt 2 (syntax->datum #'bits))))) new)]))))
                        (else
                          #`(define-syntax setter-name
                            (lambda (stx)
                              (syntax-case stx ()
                                [(_ x new)
                                #`(fxlogor (fxand x 
                                              #,(fxnot (fxarithmetic-shift-left 
                                                          (sub1 (expt 2 (syntax->datum #'bits))) 
                                                          (syntax->datum #'total-bits))))
                                          (fxarithmetic-shift-left new total-bits))])))))
                    #,(ts-backend #`(k #,(fx+ (syntax->datum #'bits) (syntax->datum #'total-bits)) struct-name rest ...))))

              ((and (number? (syntax->datum #'bits))
                    (eq? (syntax->datum #'type) 's))
                    
                #`(begin
                    #,(cond
                        ((= 0 (syntax->datum #'total-bits))
                         #`(define-syntax getter-name
                            (lambda (stx)
                              (syntax-case stx ()
                                [(_ x)
                                 #`(fx- (fxand x #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))]))))
                        ((null? (syntax->datum #'(rest ...)))
                        #`(define-syntax getter-name
                            (lambda (stx)
                              (syntax-case stx ()
                                [(_ x)
                                 #`(fx- (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) #,(expt 2 (fx1- (syntax->datum #'bits))))]))))
                          (else
                          #`(define-syntax getter-name
                              (lambda (stx)
                                (syntax-case stx ()
                                [(_ x)
                                #`(fx- (fxand (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits)))) #,(expt 2 (fx1- (syntax->datum #'bits))))])))))
                    #,(cond
                      ((= 0 (syntax->datum #'total-bits))
                       #`(define-syntax setter-name
                          (lambda (stx)
                            (syntax-case stx ()
                              [(_ x new)
                               #`(fxlogor (fxand x #,(fx- (most-positive-fixnum) (sub1 (expt 2 (syntax->datum #'bits))))) 
                                        (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))))]))))
                      (else
                      #`(define-syntax setter-name
                          (lambda (stx)
                            (syntax-case stx ()
                              [(_ x new)
                              #`(fxlogor (fxand x 
                                            #,(fxnot 
                                              (fxarithmetic-shift-left 
                                                (sub1 (expt 2 (syntax->datum #'bits))) 
                                                (syntax->datum #'total-bits))))
                                            (fxarithmetic-shift-left (fx+ new #,(expt 2 (fx1- (syntax->datum #'bits)))) total-bits))])))))
                    #,(ts-backend #`(k #,(fx+ (syntax->datum #'bits) (syntax->datum #'total-bits)) struct-name rest ...))))))))]
            [(_ rest ...)
            #'(void)])))

(define-syntax define-tiny-struct
  (lambda (stx)
    (syntax-case stx ()
      [(k struct-name rest ...)
       (ts-backend #'(k 0 struct-name rest ...))]))))

#|
(define-tiny-struct my-struct
 s 32 a
 u 13 b
 char c
 bool e
 array s 4 20 f
 4 g)|#
