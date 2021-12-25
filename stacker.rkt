#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum)) ;; datum->syntax to make a syntax object from
;; module datum
(provide read-syntax) ;; make read-syntax public

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  ;; macro gets chunk of code -> define macro with syntax pattern
  ;; syntax pattern is like regular expression
  ;; define doesn't support syntax patterns, so we use define-macro
  ;; we are defining a macro called stacker-module-begin
  ;; HANDLE-EXPR is a pattern variable -> named match within syntax pattern
  ;; when used with ..., this pattern variable will match each line of code
  ;; passed to macro
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))
;; the return value
;; #' to make code into syntax object
;; ' makes code into a datum
;; #; not only makes code into datum, but captures current lexical context,
;; and attaches that to the new syntax object (list of availible variables)
;; basically means a syntax object made with #' has access to all variables
;; defined at that point in the code -> one of them is the HANLDE-EXPR ...
;; another variable is the #%module-begin variable
(provide (rename-out [stacker-module-begin #%module-begin]))
;; make public and rename to #%module-begin
;; we didn't name it that earlier or there would be naming conflict between
;; the one we are defining and the one from br/quicklang

;; designing our expander
;; 1. provide special #%module-begin macro
;; 2. implement stack
;; 3. provide bindings for three identifiers: handle, +, *

(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f]) ;; optional argument
  (cond [(number? arg) (push-stack! arg)]
        [(or (equal? + arg) (equal? * arg))
         (define op-result (arg (pop-stack!) (pop-stack!)))
         (push-stack! op-result)]))

(provide handle)

(provide + *)