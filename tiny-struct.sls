(library (tiny-struct)
    (export define-tiny-struct)
    (import (chezscheme))


(meta define ts-backend
  (lambda (stx)
    (syntax-case stx ()
      [(k 0 struct-name type field-name rest ...)
       (eq? (syntax->datum #'type) 'bool)
       (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          #`(begin
              (define getter-name
                (lambda (x)
                  (fxbit-set? x 0)))
              (define setter-name
                (lambda (x new)
                  (if new
                    (fxlogbit1 0 x)
                    (fxlogbit0 0 x))))
            #,(ts-backend #'(k 1 struct-name rest ...)))))]
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
              (define getter-name
                (lambda (x)
                  (fxbit-set? x #,(syntax->datum #'total-bits))))
              (define setter-name
                (lambda (x new)
                  (if new
                    (fxlogbit1 total-bits x)
                    (fxlogbit0 total-bits x))))
            #,(ts-backend #`(k #,(fx1+ (syntax->datum #'total-bits)) struct-name rest ...))))))]
      [(k 0 struct-name type field-name rest ...)
       (eq? (syntax->datum #'type) 'char)
       (let ([base (string-append 
                      (symbol->string (syntax->datum #'struct-name))
                      "-" 
                      (symbol->string (syntax->datum #'field-name)))])
        (with-syntax ([getter-name (datum->syntax #'k (string->symbol base))]
                      [setter-name (datum->syntax #'k (string->symbol (string-append base "-set")))])
          #`(begin
              (define getter-name
                (lambda (x)
                  (integer->char (fxand x 255))))
              (define setter-name
                (lambda (x new)
                  (fxlogor (fxand x #,(fx- (most-positive-fixnum) 255)) (char->integer new))))
            #,(ts-backend #'(k 8 struct-name rest ...)))))]
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
            (define getter-name
              (lambda (x)
                (integer->char (fxand (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) 255))))
            (define setter-name
              (lambda (x new)
                (fxlogor (fxand x 
                            #,(fxnot (fxarithmetic-shift-left 
                                        255
                                        (syntax->datum #'total-bits))))
                        (fxarithmetic-shift-left (char->integer new) total-bits))))
            #,(ts-backend #`(k #,(fx+ 8 (syntax->datum #'total-bits)) struct-name rest ...))))))]
      [(k 0 struct-name type bits field-name rest ...)
      (if (fx> (syntax->datum #'bits) 60)
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
                  (define getter-name
                    (lambda (x)
                      (fxand x #,(fx1- (expt 2 (syntax->datum #'bits))))))
                  (define setter-name
                    (lambda (x new)
                      (fxlogor (fxand x #,(fx- (most-positive-fixnum) (fx1- (expt 2 (syntax->datum #'bits))))) new)))
              #,(ts-backend #'(k bits struct-name rest ...))))
              
              ((and (number? (syntax->datum #'bits))
                    (eq? (syntax->datum #'type) 's))
              #`(begin
                  (define getter-name
                    (lambda (x)
                     (let ([pre (fxand x #,(fx1- (expt 2 (syntax->datum #'bits))))])
                      (if (fxbit-set? pre #,(fx1- (syntax->datum #'bits)))
                        (fxlogor pre #,(fxnot (fx1- (expt 2 (syntax->datum #'bits)))))
                        pre))))
                  (define setter-name
                    (lambda (x new)
                      (if (fxnegative? new)
                      (fxlogor (fxand x #,(fx- (most-positive-fixnum) (fx1- (expt 2 (syntax->datum #'bits))))) 
                               (fxlogbit1 #,(fx1- (syntax->datum #'bits)) (fxand new #,(fx1- (expt 2 (syntax->datum #'bits))))))
                      (fxlogor (fxand x #,(fx- (most-positive-fixnum) (fx1- (expt 2 (syntax->datum #'bits))))) 
                               new))))
              #,(ts-backend #'(k bits struct-name rest ...))))
              
              (else #'type)))))]
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
                    (define getter-name
                      (lambda (x)
                        (fxand (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))))
                    (define setter-name
                      (lambda (x new)
                        (fxlogor (fxand x 
                                    #,(fxnot (fxarithmetic-shift-left 
                                                (sub1 (expt 2 (syntax->datum #'bits))) 
                                                (syntax->datum #'total-bits))))
                                (fxarithmetic-shift-left new total-bits))))
                    #,(ts-backend #`(k #,(fx+ (syntax->datum #'bits) (syntax->datum #'total-bits)) struct-name rest ...))))

              ((and (number? (syntax->datum #'bits))
                    (eq? (syntax->datum #'type) 's))
                    
                #`(begin
                    (define getter-name
                      (lambda (x)
                       (let ([pre (fxand (fxarithmetic-shift-right x #,(syntax->datum #'total-bits)) #,(sub1 (expt 2 (syntax->datum #'bits))))])
                        (if (fxbit-set? pre #,(fx1- (syntax->datum #'bits)))
                          (fxlogor pre #,(fxnot (fx1- (expt 2 (syntax->datum #'bits)))))
                          pre))))
                    (define setter-name
                      (lambda (x new)
                        (if (fxnegative? new)
                          (fxlogor (fxand x 
                                      #,(fxnot 
                                        (fxarithmetic-shift-left 
                                          (sub1 (expt 2 (syntax->datum #'bits))) 
                                          (syntax->datum #'total-bits))))
                                    (fxarithmetic-shift-left 
                                      (fxlogbit1 
                                        #,(fx1- (syntax->datum #'bits)) 
                                        (fxand new #,(fx1- (expt 2 (syntax->datum #'bits))))) 
                                      total-bits))
                          (fxlogor (fxand x 
                                    #,(fxnot 
                                      (fxarithmetic-shift-left 
                                        (sub1 (expt 2 (syntax->datum #'bits))) 
                                        (syntax->datum #'total-bits))))
                                    (fxarithmetic-shift-left new total-bits)))))
                    #,(ts-backend #`(k #,(fx+ (syntax->datum #'bits) (syntax->datum #'total-bits)) struct-name rest ...))))))))]
            [(_ rest ...)
            #'(void)])))

(define-syntax define-tiny-struct
  (lambda (stx)
    (syntax-case stx ()
      [(k struct-name rest ...)
       (ts-backend #'(k 0 struct-name rest ...))]))))
