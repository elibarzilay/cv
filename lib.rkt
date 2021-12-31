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
  S?, L?, M?, T?, LM?, ST?, ... -- booleans for the chosen modes
  (*: item ...)   -- itemize list
  (W text ...)    -- wrap line (in text mode)
  (sec++) (sec--) -- inc/dec header level (starts at level 1)
  (section! title text ...)  -- add section: header + text
  (header! title)            -- add just the header
  (section*! title text ...) -- add an all-itemized section
                                (has #:pfx to precede the text)

|#
;; -- Public ------------------------------------------------------------------

(define-syntax flags '([V [S short] [L long]] [F [M md] [T tex]]
                       TEXT))

(require scribble/text scribble/text/wrap)

(provide (except-out (all-from-out racket) #%module-begin)
         (all-from-out scribble/text)
         (rename-out [mod-beg #%module-begin])
         ->string : url *: cventries: cvitemize: W newlines:
         header section! section*! part! sec++ sec--
         \\ it em o date loc NODATE
         ~block ~splice)

;; -- Utilities ---------------------------------------------------------------

(define (->string x)
  (cond [(string? x) x] [(symbol? x) (number->string x)]
        [(is-val? x) (with-output-to-string (λ() (output x)))]
        [else ""])
  (with-output-to-string (λ() (output x))))

(define (is-val? x) (not (or (not x) (void? x) (null? x))))
(define (: . xs) (define r (filter is-val? xs)) (and (is-val? r) r))
(define (list->: l) (apply : l))

(define (maptree f x)
  (let loop ([x x])
    (cond [(null? x) x]
          [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
          [else (f x)])))

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

(define ((lazify f) . text) (lazy (apply f (maptree force text))))
(define ~block  (lazify block))
(define ~splice (lazify splice))

(define (url . xs)
  (define url (->string xs))
  (define email? (regexp-match? #rx"@" url))
  (define full-url (list (if email? "mailto:" "https://") url))
  (F: (if TEXT? url @:{[`@url`](@full-url)})
      @list{\href{@full-url}@;
                 {\texttt{{\addfontfeature{LetterSpace=-2.0}@;
                           @(regexp-replace #rx"/$" url "")}}}}))

;; -- Mode flags --------------------------------------------------------------

(define-syntax (-def-flags- _)
  (define flags (syntax-local-value #'flags))
  (define single-flags (filter symbol? flags))
  (define multi-flags  (filter pair?   flags))
  (define (sym . xs)
    (string->symbol (apply string-append (map (λ(x) (format "~a" x)) xs))))
  (define (symbol-downcase x)
    (string->symbol (string-downcase (symbol->string x))))
  (define (maptree f x)
    (let loop ([x x])
      (cond [(null? x) x]
            [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
            [else (f x)])))
  (define (ids sfx [fs multi-flags])
    (maptree (λ(s) (datum->syntax _ (sym s sfx))) fs))
  (define (cross opts)
    (if (null? opts) '(())
        (let ([rest (cross (cdr opts))])
          (apply append (map (λ(o) (map (λ(r) (cons o r)) rest)) (car opts))))))
  (define combos (map (λ(c) (cons (apply sym c) c))
                      (cross (map (λ(fs) (map car (cdr fs))) multi-flags))))
  (with-syntax ([(SF? ...) (ids '? single-flags)]
                [(sf  ...) (ids "" (map symbol-downcase single-flags))]
                [([Y  [X  f ] ...] ...) (ids "")]
                [([Y? [X? f?] ...] ...) (ids '?)]
                [([Y: [X: f:] ...] ...) (ids ':)]
                [([Y- [X- f-] ...] ...) (ids '-)]
                [([C? P? ...] ...) (ids '? combos)]
                [([C: P: ...] ...) (ids ': combos)])
    #`(begin
        (define args
          (map string->symbol (vector->list (current-command-line-arguments))))
        (let ([bad (remq* '(f ... ... sf ...) args)])
          (when (pair? bad) (raise-user-error 'cv "unknown flag/s: ~a" bad)))
        (begin (define SF? (and (memq 'sf args) #t)) (provide SF?))
        ...
        (begin (define X?  (and (memq 'f args) #t))
               (define-syntax-rule (X: text (... ...))
                 (and X? (: text (... ...))))
               (provide X: X?))
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
               (provide C: C?))
        ...)))
-def-flags-

;; -- Wrapping ----------------------------------------------------------------

(define (wrap width . text)
  (when (<= width 5) (error 'wrap "width too small: ~a" width))
  (let* ([text (->string text)]
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
  (if (not TEXT?) text
      (~block flush (λ() (define-values [line col pos]
                           (port-next-location (current-output-port)))
                         (wrap (- 79 col) text)))))

(define (newlines: . xs)
  (let ([xs (apply : xs)])
    (and xs (add-between xs "\n"))))

;; -- TeX envs ----------------------------------------------------------------

(define (cventries: . xs)
  (apply newlines: `("\\begin{cvplains}" ,@xs "\\end{cvplains}")))

(define (cvitemize: . xs)
  (apply newlines: `("\\begin{cvplains}" ,(apply *: xs) "\\end{cvplains}")))

;; -- Itemize -----------------------------------------------------------------

(define items-level (make-parameter 0))
(define (*: . xs)
  (lazy (parameterize ([items-level (add1 (items-level))])
          (apply items (maptree force xs)))))
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
    (define item
      (F: (let ([* (case lvl
                     [(1) "*"] ; "*" for toplevel
                     [(2) (if (has-items? xs) "+" "-")] ; "+" if there's a 3rd
                     [else "-"])])
            (λ(x) @~block{@* @~block{@x}}))
          (λ(x) @:{\item @x})))
    (define loose? (ormap should-be-loose? xs))
    (define is  (map item xs))
    (define sep (F: (if (or (and (= 1 lvl) (V: #f #t)) loose?) "\n\n" "\n")
                    "\n"))
    (define wrap
      (F: values (λ(x) @block{\begin{itemize}
                                @x
                              \end{itemize}})))
    (with-props (wrap (add-between is sep)) 'items? #t 'loose? loose?)))

;; -- Parts -------------------------------------------------------------------

(define parts '())

(define (part! . xs)
  (define ps (list->: xs))
  (when (pair? ps)
    (set! parts `(,@ps
                  ,(and (pair? parts)
                        (if (and LM? (prop-ref (car parts) 'section?))
                          "\n\n\n" "\n\n"))
                  ,@parts))))

(define cur-header (make-parameter 1))
(define (sec++) (cur-header (add1 (cur-header))))
(define (sec--) (cur-header (sub1 (cur-header))))

(define (header title)
  (define (bad)
    (error 'header "bad tex header level ~s for ~s" (cur-header) title))
  (F: (if TEXT?
        (let ([title (->string title)])
          @:{@title
             @(make-string (string-length title)
                           (case (cur-header)
                             [(1 2) #\=] [(3) #\-] [else (bad)]))})
        @:{@(make-string (cur-header) #\#) @title})
      @:{\@(case (cur-header)
             [(2) 'cvsection] [(3) 'cvplainsection] [(4) 'cvsubsection]
             [else (bad)])@;
         {@title}@"\n"}))

(define (section! title #:sec-dates [sec-dates #f] . text)
  (define xs (list->: (map force text)))
  (when (pair? xs)
    (part! (with-props
             @list{@header[title]
                   @(M: "\n")@;
                   @(list (λ() (date-info 'section! title sec-dates)) xs)}
             'section? #t))))

(define (section*! #:pfx [pfx #f] #:itemize [itemize (F: *: cventries:)] title
                   #:sec-dates [sec-dates #f]
                   . text)
  (define xs (list->: (map force text)))
  (when (pair? xs)
    (define is (apply itemize xs))
    (section! title #:sec-dates sec-dates
      pfx (and (is-val? pfx) (if (prop-ref (force is) 'loose?) "\n\n" "\n"))
      is)))

;; -- Tex stuff ---------------------------------------------------------------

(define date (make-parameter #f))
(define loc  (make-parameter #f))
(define NODATE (gensym))

(define \\ (and (not TEXT?) @F:[" \\" " \\\\"]))

;; just for titles
(define (it . text)
  (F: (if TEXT? @~splice{«@|text|»} @~splice{*“@|text|”*})
      @~splice{\textit{@text}}))

(define (em . text)
  (F: @~splice{*@|text|*}
      @~splice{\emph{@text}}))

;; the date/loc are not rendered in md, add explicitly or with #:md-*
(define (o #:loc [l #f] #:md-pfx [mpfx #f] #:md-sfx [msfx #f]
           #:nobr [nobr #f] #:dname [dname #f] #:dinfo [dinfo '()] 1st
           . rest)
  (and 1st (apply o* l mpfx msfx nobr dname dinfo
                  (if (eq? #t 1st) rest (cons 1st rest)))))

(define (o* l mpfx msfx nobr dname dinfo d title . text)
  (define d* (and (not (eq? d NODATE)) d))
  (define (dsubst str) (and str (regexp-replace #rx"D" (->string str) d*)))
  (define dname* (or dname title))
  (list (λ() (date d*) (loc l)
             (when (and dname* d*) (date-info 'item! dname* d* dinfo)))
        (F: (apply ~splice (: (dsubst mpfx) title) `(,@text ,(dsubst msfx)))
            @~splice{
              \cvplain{@d*}{@l}{@(if nobr title (regexp-replace
                                                 #rx":$" (->string title) ""))}
              @(and (not nobr) "\n")@;
              @(apply : (if (and (pair? text) (equal? "\n" (car text)))
                          (cdr text) text))
              @||})
        (λ() (date #f) (loc #f))))

;; -- Tex stuff ---------------------------------------------------------------

(define all-sections '())
(define current-section #f)

(define ref-counter (let ([n 0]) (λ() (set! n (add1 n)) n)))

(define (date-info . msg)
  (define (done)
    (when (and current-section (pair? (cdr current-section)))
      (set! all-sections `(,@all-sections ,(reverse current-section)))))
  (define (keyval xs sep)
    (add-between (map (λ(kv) @:{@(car kv): "@(cadr kv)"}) xs) (list "," sep)))
  (define entry
    (match-lambda
     [(list R name datestr dinfo)
      @splice{{
        @block{@(keyval `([datestr ,datestr] [name ,name] [R ,R] ,@dinfo) "\n")}
      }}]))
  (define (section x)
    @splice{@(format "~s" (car (car x))): {
              @(keyval (cadr (car x)) " "),
              @block{entries: [@(splice (add-between (map entry (cdr x))
                                                     ", "))]}}})
  (define (json)
    @splice{{
      @(block (add-between (map section all-sections) ",\n"))
    }})
  (match msg
    [`(section! ,title ,sec-dates)
     (done)
     (set! current-section (and title sec-dates `((,title ,sec-dates))))]
    [`(item! . ,info)
     (when (and current-section (andmap is-val? info)
                (not (equal? "" (car info))))
       (define R (ref-counter))
       (set! current-section (cons (cons R info) current-section))
       (and M? (not TEXT?) @:{<span id="R@R"></span>}))]
    ['() @:{const dateInfo = @json;
            @||}]))

;; -- Rendering ---------------------------------------------------------------

(define (tex-writer str p [start 0] [end (string-length str)])
  (let loop ([start start])
    (define m (and (< start end) (regexp-match-positions #rx"[#&]" str start end p)))
    (when m
      (write-string (case (string-ref str (caar m)) [(#\&) "\\&"] [(#\#) "\\#"]) p)
      (loop (cdar m)))))

(define (render!)
  (port-count-lines! (current-output-port))
  (define text (list (reverse parts) "\n"))
  (output (F: text (with-writer tex-writer text)))
  (define dates-file (getenv "dates_file"))
  (when (and dates-file (not (equal? "" dates-file)))
    (with-output-to-file dates-file #:exists 'truncate
      (λ() (output date-info)))))

(define-syntax-rule (mod-beg x ...)
  (#%module-begin x ... (render!)))
