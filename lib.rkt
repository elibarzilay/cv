#lang at-exp racket
#|

  (: text ...)    -- shorthand for (list text ...), filtering #f/void/null,
                     returning void if none left
  (V: short long) -- short/long alternatives
  (S: text ...)   -- only-short, same as (V: (: text ...) #f)
  (L: text ...)   -- only-long, same as (V: #f (: text ...))
  (F: md tex)     -- md/tex alternatives
  (M: text ...)   -- only-md, same as (F: (: text ...) #f)
  (T: text ...)   -- only-tex, same as (F: #f (: text ...))
  SM:, LM:, ST:, ... -- combinations for both version + format
  (*: item ...)   -- itemize list
  (W text ...)    -- wrap line
  (sec++) (sec--) -- inc/dec header level (starts at level 1)
  (section! title text ...)  -- add section: header + text
  (header! title)            -- add just the header
  (section*! title text ...) -- add an all-itemized section
                                (has #:pfx to precede the text)

|#
;; -- Public ------------------------------------------------------------------

(define-syntax flags '([V [S short] [L long]] [F [M md] [T tex]]))

(require scribble/text scribble/text/wrap)

(provide (except-out (all-from-out racket) #%module-begin)
         (all-from-out scribble/text)
         (rename-out [mod-beg #%module-begin])
         : *: W header section! section*! part! sec++ sec--)

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

;; -- Mode flags --------------------------------------------------------------

(define-syntax (-def-flags- _)
  (define flags (syntax-local-value #'flags))
  (define (sym . xs)
    (string->symbol (apply string-append (map (λ(x) (format "~a" x)) xs))))
  (define (maptree f x)
    (let loop ([x x])
      (cond [(null? x) x]
            [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
            [else (f x)])))
  (define (ids sfx [fs flags])
    (maptree (λ(s) (datum->syntax _ (sym s sfx))) fs))
  (define (cross opts)
    (if (null? opts) '(())
        (let ([rest (cross (cdr opts))])
          (apply append (map (λ(o) (map (λ(r) (cons o r)) rest)) (car opts))))))
  (define combos (map (λ(c) (cons (apply sym c) c))
                      (cross (map (λ(fs) (map car (cdr fs))) flags))))
  (with-syntax ([([Y  [X  f ] ...] ...) (ids "")]
                [([Y? [X? f?] ...] ...) (ids '?)]
                [([Y: [X: f:] ...] ...) (ids ':)]
                [([Y- [X- f-] ...] ...) (ids '-)]
                [([C? P? ...] ...) (ids '? combos)]
                [([C: P: ...] ...) (ids ': combos)])
    #`(begin
        (define args
          (map string->symbol (vector->list (current-command-line-arguments))))
        (let ([bad (remq* '(f ... ...) args)])
          (when (pair? bad) (raise-user-error 'cv "unknown flag/s: ~a" bad)))
        (begin (define X? (and (memq 'f args) #t))
               (define-syntax-rule (X: text (... ...))
                 (and X? (: text (... ...))))
               (provide X:))
        ... ...
        (begin (let ([fs (filter values (list (and X? 'f) ...))])
                 (case (length fs)
                   [(1) (void)]
                   [(0) (raise-user-error 'cv "missing one of: ~s" '(f ...))]
                   [else (raise-user-error 'cv "conflicting flags: ~s" fs)]))
               (define-syntax-rule (Y: X- ...) (cond [X? X-] ...))
               (provide Y:))
        ...
        (begin (define C? (and P? ...))
               (define-syntax-rule (C: text (... ...))
                 (and C? (: text (... ...))))
               (provide C:))
        ...)))
-def-flags-

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
    (define (item x) @block{@* @block{@x}})
    (define is  (map item xs))
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

(define cur-header (make-parameter 1))
(define (sec++) (cur-header (add1 (cur-header))))
(define (sec--) (cur-header (sub1 (cur-header))))

(define (header title)
  @:{@(make-string (cur-header) #\#) @title})

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
