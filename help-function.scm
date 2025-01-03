(define call/cc call-with-current-continuation)
(define char-letter? char-alphabetic?)
(define char-digit? char-numeric?)
(define-syntax safe-cons
  (syntax-rules()
    ((safe-cons head tail)
     (let*
         ((gl head)
          (hv tail))
       (cons gl hv)))))
;; Управление стеком
(define (list-drop lst)
  (and (> (length lst) 1) (cdr lst)))
(define (list-swap lst)
  (and (> (length lst) 1) (cons (cadr lst) (cons (car lst) (cddr lst)))))
(define (list-dup lst)
  (cons (car lst) (cons (car lst) (cdr lst))))
(define (list-over lst)
  (and (> (length lst) 1) (cons (cadr (reverse lst)))))
(define (list-rot lst)
  (and (> (length lst) 2) (cons (caddr lst) (cons (cadr lst) (cons (car lst) (cdddr lst))))))
(define (list-depth lst)
  (cons (length lst) lst))