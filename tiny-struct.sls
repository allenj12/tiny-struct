(library (tiny-struct)
    (export define-tiny-struct)
    (import (chezscheme))


(meta define ts-backend
  (lambda (stx)
    (syntax-case stx ()
      [(k 0 struct-name bits field-name rest ...)
      (if (fx> (syntax->datum #'bits) 61)
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
                  (fxand x #,(fx1- (expt 2 (syntax->datum #'bits))))))
              (define setter-name
                (lambda (x new)
                  (fxlogor (fxand x #,(fx- (most-positive-fixnum) (fx1- (expt 2 (syntax->datum #'bits))))) new)))
              #,(ts-backend #'(k bits struct-name rest ...))))))]
        [(k bits-total struct-name bits field-name rest ...)
        (if (fx> (fx+ (syntax->datum #'bits) (syntax->datum #'bits-total)) 61)
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
                    (fxand (fxarithmetic-shift-right x #,(syntax->datum #'bits-total)) #,(sub1 (expt 2 (syntax->datum #'bits))))))
                (define setter-name
                  (lambda (x new)
                    (fxlogor (fxand x 
                                #,(fxnot (fxarithmetic-shift-left (sub1 (expt 2 (syntax->datum #'bits))) (syntax->datum #'bits-total))))
                             (fxarithmetic-shift-left new bits-total))))
                #,(ts-backend #`(k #,(+ (syntax->datum #'bits) (syntax->datum #'bits-total)) struct-name rest ...))))))]
          [(_ rest ...)
           #'(void)])))

;;figure out bit arithimitec math when bits is greater than 32 bits (most positive fxnum)
(define-syntax define-tiny-struct
  (lambda (stx)
    (syntax-case stx ()
      [(k struct-name bits field-name rest ...)
       (ts-backend #'(k 0 struct-name bits field-name rest ...))])))
