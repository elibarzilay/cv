#lang at-exp racket
#|

  (: text ...)    -- shorthand for (list text ...), filtering #f/void/null,
                     returning void if none left
  (V: short long) -- short/long alternatives
  (S: text ...)   -- only-long, same as (V: (: text ...) #f)
  (L: text ...)   -- only-long, same as (V: #f (: text ...))
  (*: item ...)   -- itemize list
  (S*: item ...)  -- (S: (*: item ...))
  (L*: item ...)  -- (L: (*: item ...))
  (W text ...)    -- wrap line
  (sec++) (sec--) -- inc/dec header level (starts at level 1)
  (section! title text ...)  -- add section: header + text
  (header! title)            -- add just the header
  (section*! title text ...) -- add an all-itemized section
                                (has #:pfx to precede the text)

|#
;; -- Public ------------------------------------------------------------------

(require scribble/text scribble/text/wrap)

(provide (except-out (all-from-out racket) #%module-begin)
         (all-from-out scribble/text)
         (rename-out [mod-beg #%module-begin])
         W : V: S: L: *: S*: L*: header section! section*! part! sec++ sec--)

;; -- Mode --------------------------------------------------------------------

(define args (current-command-line-arguments))
(unless (= 1 (vector-length args))
  (raise-user-error 'cv "expecting a single mode argument"))
(define mode (string->symbol (vector-ref args 0)))
(unless (memq mode '(short long))
  (raise-user-error 'cv "unknown mode: ~s" mode))

;; -- Utilities ---------------------------------------------------------------

(define (is-val? x) (not (or (not x) (void? x) (null? x))))
(define (: . xs) (define r (filter is-val? xs)) (and (is-val? r) r))
(define (list->: l) (apply : l))

;; generic properties (from ~/pl/utils/web-utils.rkt)
(define-values [prop-set! prop-ref]
  (let ([t (make-weak-hasheq)])
    (values
     (λ(obj prop val . more)
       (let ([t (hash-ref! t obj make-hash)])
         (let loop ([prop prop] [val val] [more more])
           (hash-set! t prop val)
           (when (pair? more) (loop (car more) (cadr more) (cddr more))))))
     (λ(obj prop [default #f])
       (hash-ref (hash-ref! t obj make-hash) prop default)))))
(define with-props
  (case-lambda [(obj) obj]
               [(obj k v . more) (apply prop-set! obj k v more) obj]))

;; -- Modes -------------------------------------------------------------------

(define-syntax-rule (V: -short -long)
  (case mode [(short) -short] [(long) -long]))
(define-syntax-rule (S: text ...) (V: (: text ...) #f))
(define-syntax-rule (L: text ...) (V: #f (: text ...)))
(define-syntax-rule (S*: x ...) (S: (*: x ...)))
(define-syntax-rule (L*: x ...) (L: (*: x ...)))

;; -- Wrapping ----------------------------------------------------------------

(define (wrap width . text)
  (let* ([text (with-output-to-string (λ() (output text)))]
         [text (regexp-replace* #rx"\n(\n*)" text " \\1")]
         [text (regexp-split #rx"\n" text)]
         [text (map (λ(t) (string-trim (regexp-replace* #px"\\s+" t " ")))
                    text)]
         [text (map (λ(t) (string-join (wrap-line t width) "\n")) text)]
         [text (string-join text "\n\n")]
         ;; replace N LFs by (N/2)+1
         [text (regexp-replace* #px"\n\n(\n+)\\1" text "\n\n\\1")]
         ;; [text (regexp-replace*
         ;;        #px"\n\n\n+" text
         ;;        (λ(ns) (make-string (add1 (floor (/ (string-length ns) 2))) #\newline)))]
         )
    text))

(define (W . text)
  (block flush (λ() (define-values [line col pos]
                      (port-next-location (current-output-port)))
                    (wrap (- 79 col) text))))

;; -- Itemize -----------------------------------------------------------------

(define items-level (make-parameter 0))
(define-syntax-rule (*: x ...)
  (parameterize ([items-level (add1 (items-level))]) (items x ...)))
(define (should-be-loose? is)
  (define (has-empty-line? is)
    (and (pair? is)
         (or (prop-ref (car is) 'loose?)
             (and (equal? "\n" (car is))
                  (pair? (cdr is))
                  (equal? "\n" (cadr is)))
             (has-empty-line? (cdr is)))))
  (define (has-long-paras? is)
    (and (pair? is) (< 4 (count (λ(x) (equal? "\n" x)) is))))
  (or (has-empty-line? is) (has-long-paras? is)))
(define (has-items? x)
  (or (prop-ref x 'items?) (and (pair? x) (ormap has-items? x))))
(define (items . items)
  (define xs (list->: items))
  (when xs
    (define lvl (items-level))
    ;; "*" for toplevel, 2nd is "+" if there's a 3rd, "-" otherwise
    (define * (case lvl
                [(1) "*"]
                [(2) (if (has-items? xs) "+" "-")]
                [else "-"]))
    (define loose? (ormap should-be-loose? xs))
    (define is  (map (λ(x) (list * " " (block x))) xs))
    (define sep (if (or (and (= 1 lvl) (V: #f #t)) loose?) "\n\n" "\n"))
    (with-props (add-between is sep) 'items? #t 'loose? loose?)))

;; -- Parts -------------------------------------------------------------------

(define parts '())

(define (part! . xs)
  (define ps (list->: xs))
  (when (pair? ps)
    (set! parts `(,@ps
                  ,(and (pair? parts)
                        (if (prop-ref (car parts) 'section?)
                          (V: "\n\n" "\n\n\n") "\n\n"))
                  ,@parts))))

(define cur-header (make-parameter "# "))
(define (sec++) (cur-header (string-append "#" (cur-header))))
(define (sec--) (cur-header (substring (cur-header) 1)))

(define (header title)
  (: (cur-header) title))

(define (section! title . text)
  (define xs (list->: text))
  (when (pair? xs)
    (part! (with-props @list{@header[title]
                             @||
                             @xs}
                       'section? #t))))

(define-syntax section*!
  (syntax-rules ()
    [(_ title #:pfx pfx text ...)
     (let ([is (*: text ...)])
       (section! title
                 pfx (and (is-val? pfx) (if (prop-ref is 'loose?) "\n\n" "\n"))
                 is))]
    [(_ title text ...)
     (section! title (*: text ...))]))

;; -- Rendering ---------------------------------------------------------------

(define (render!)
  (port-count-lines! (current-output-port))
  (output (list (reverse parts) "\n")))

(define-syntax-rule (mod-beg x ...)
  (#%module-begin x ... (render!)))
