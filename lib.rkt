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
  !S:, !L:, !M:, !T: -- negated forms of S:, ...
  TEXT?, TEXT:, !TEXT: -- same for text mode (subvariant of md mode)
  (*: item ...)   -- itemize list
  (W text ...)    -- wrap line (in text mode)
  (sec++) (sec--) -- inc/dec header level (starts at level 1)
  (section! title text ...)  -- add section: header + text
  (header! title)            -- add just the header
  (section*! title text ...) -- add an all-itemized section
                                (has #:pfx to precede the text)

|#
;; ---->> Public --------------------------------------------------------------

(define-syntax flags '([V [S short] [L long]] [F [M md] [T tex]]
                       TEXT))

(require scribble/text scribble/text/wrap)

(provide (except-out (all-from-out racket) #%module-begin)
         (all-from-out scribble/text)
         (rename-out [mod-beg #%module-begin])
         ->string : url *: cventries: cvitemize: W newlines:
         header section! section*! part! sec++ sec--
         \\ it em o
         ~block ~splice)

;; ---->> Utilities -----------------------------------------------------------

(define (->string x)
  (with-output-to-string (λ() (output x))))

(define-syntax-rule (push! val var) (set! var (cons val var)))

(define (is-val? x) (not (or (not x) (void? x) (null? x))))
(define (: . xs) (define r (filter is-val? xs)) (and (is-val? r) r))
(define (list->: l) (apply : l))

(define (maptree f x)
  (let loop ([x x])
    (cond [(null? x) x]
          [(pair? x) (cons (loop (car x)) (loop (cdr x)))]
          [else (f x)])))

;; generic properties (from ~/pl/utils/web-utils.rkt)
(define-values [prop-set! prop-ref copy-missing-props]
  (let ([t (make-weak-hasheq)])
    (values
     (λ(obj prop val . more)
       (let ([t (hash-ref! t obj make-hash)])
         (let loop ([prop prop] [val val] [more more])
           (hash-set! t prop val)
           (when (pair? more) (loop (car more) (cadr more) (cddr more))))))
     (λ(obj prop [default #f])
       (hash-ref (hash-ref! t obj make-hash) prop default))
     (λ(from to)
       (define from-props (hash-ref! t from make-hash))
       (define to-props   (hash-ref! t to   make-hash))
       (define to-keys    (hash-keys (hash-ref! t to   make-hash)))
       (for ([(k v) (in-hash from-props)] #:unless (memq k to-keys))
         (hash-set! to-props k v))))))
(define with-props
  (case-lambda
    [(obj) (if (promise? obj) (lazy (copy-missing-props obj (force obj))) obj)]
    [(obj k v . more) (apply prop-set! obj k v more) obj]))

(define ((lazify f) . text) (lazy (apply f (maptree force text))))
(define ~block  (lazify block))
(define ~splice (lazify splice))

(define simple-text? (make-parameter #f))

;; @url{some-url} or @url{text | some-url}
;;   the second version shows just the text in plain-text contexts
(define (url . xs)
  (define-values [text url]
    (let ([m (regexp-match #rx"^(?:([^|]+?) *\\|+ *([^|]+)|.*)$"
                           (->string xs))])
      (if (cadr m) (apply values (cdr m)) (values #f (car m)))))
  (define pfx
    (cond [(regexp-match? #rx":" url) ""]
          [(regexp-match? #rx"@" url) "mailto:"]
          [(regexp-match? #rx"^\\+?[0-9]+[0-9() .,-]+[0-9]$" url)
           (unless text
             (let ([n (regexp-replace #rx"^\\+1-*" url "")])
               (set! text (if TEXT? n @list{`@n`}))))
           (set! url (regexp-replace* #rx"[^0-9+]" url ""))
           "tel:"]
          [else "https://"]))
  (define full-url (list pfx url))
  (λ() (if (simple-text?)
         (or text (regexp-replace #rx"/$" url ""))
         (F: (if TEXT? (or text url) @:{[@(or text @list{`@url`})](@full-url)})
             @list{\href{@full-url}{\textbf{\textit{@;
               @(or text @list{\texttt{@(regexp-replace #rx"/$" url "")}})}}}}))))

;; ---->> Mode flags ----------------------------------------------------------

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
  (define (ids #:pfx [pfx ""] sfx [fs multi-flags])
    (maptree (λ(s) (datum->syntax _ (sym pfx s sfx))) fs))
  (define (cross opts)
    (if (null? opts) '(())
        (let ([rest (cross (cdr opts))])
          (apply append (map (λ(o) (map (λ(r) (cons o r)) rest)) (car opts))))))
  (define combos (map (λ(c) (cons (apply sym c) c))
                      (cross (map (λ(fs) (map car (cdr fs))) multi-flags))))
  (with-syntax ([(SF?  ...) (ids '? single-flags)]
                [(SF:  ...) (ids ': single-flags)]
                [(!SF: ...) (ids #:pfx '! ': single-flags)]
                [(sf   ...) (ids "" (map symbol-downcase single-flags))]
                [([Y   [X  f ]   ...] ...) (ids "")]
                [([Y?  [X? f?]   ...] ...) (ids '?)]
                [([Y:  [X: f:]   ...] ...) (ids ':)]
                [([!Y: [!X: !f:] ...] ...) (ids #:pfx '! ':)]
                [([Y-  [X- f-]   ...] ...) (ids '-)]
                [([C?  P? ...]   ...) (ids '? combos)]
                [([C:  P: ...]   ...) (ids ': combos)])
    #`(begin
        (define args
          (map string->symbol (vector->list (current-command-line-arguments))))
        (let ([bad (remq* '(f ... ... sf ...) args)])
          (when (pair? bad) (raise-user-error 'cv "unknown flag/s: ~a" bad)))
        (begin (define SF? (and (memq 'sf args) #t))
               (define-syntax-rule (SF: text (... ...))
                 (and SF? (: text (... ...))))
               (define-syntax-rule (!SF: text (... ...))
                 (and (not SF?) (: text (... ...))))
               (provide SF? SF: !SF:))
        ...
        (begin (define X?  (and (memq 'f args) #t))
               (define-syntax-rule (X: text (... ...))
                 (and X? (: text (... ...))))
               (define-syntax-rule (!X: text (... ...))
                 (and (not X?) (: text (... ...))))
               (provide X? X: !X:))
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

;; ---->> Meta-info -----------------------------------------------------------

(define meta-info (make-hash))

(provide meta:)
(define-syntax meta:
  (syntax-rules ()
    [(meta:) (begin)]
    [(meta: name val more ...)
     (begin (define name val)
            (hash-set! meta-info 'name val)
            (meta: more ...))]))

(define-syntax-rule (meta-ref name)
  (hash-ref meta-info 'name))

;; ---->> Wrapping ------------------------------------------------------------

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

;; ---->> TeX envs ------------------------------------------------------------

(define (cventries: . xs)
  (apply newlines: `("\\begin{cvplains}" ,@xs "\\end{cvplains}")))

(define (cvitemize: . xs)
  (apply newlines: `("\\begin{cvplains}" ,(apply *: xs) "\\end{cvplains}")))

;; ---->> Itemize -------------------------------------------------------------

(define items-level (make-parameter 0))
(define (*: #:loose [loose? '?] . xs)
  (with-props (lazy (parameterize ([items-level (add1 (items-level))])
                      (apply items #:loose loose? (maptree force xs))))
              'items? #t))
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
(define (items #:loose [loose0? '?] . items)
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
    (define loose? (if (boolean? loose0?) loose0? (ormap should-be-loose? xs)))
    (define is  (map item xs))
    (define sep (F: (if (or (and (= 1 lvl) (V: #f #t)) loose?) "\n\n" "\n")
                    "\n"))
    (define wrap
      (F: values (λ(x) @block{\begin{itemize}
                                @x
                              \end{itemize}})))
    (with-props (wrap (add-between is sep)) 'items? #t 'loose? loose?)))

;; ---->> Parts ---------------------------------------------------------------

(define parts '())

(define (part! . xs)
  (define ps (list->: xs))
  (when (pair? ps)
    (when (pair? parts)
      (push! (if (and LM? (prop-ref (car parts) 'section?)) "\n\n\n" "\n\n")
             parts))
    (for-each (λ(p) (push! p parts)) ps)))

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

(define (section! title #:sec-dates [sec-dates #f] #:if [bool #t] . text)
  (define xs (list->: (map force text)))
  (when (and bool (pair? xs))
    (part! (with-props
             @list{@header[title]
                   @(M: "\n")@;
                   @(list (λ() (date-info 'section! title sec-dates)) xs)}
             'section? #t))))

(define (section*! #:pfx [pfx #f] #:itemize [itemize (F: *: cventries:)] title
                   #:sec-dates [sec-dates #f] #:if [bool #t]
                   . text)
  (define xs (list->: (map force text)))
  (when (pair? xs)
    (define is (apply itemize xs))
    (section! title #:sec-dates sec-dates #:if bool
      pfx (and (is-val? pfx) (if (prop-ref (force is) 'loose?) "\n\n" "\n"))
      is)))

;; ---->> TeX stuff -----------------------------------------------------------

(define \\ (and (not TEXT?) @F:[" \\" " \\\\"]))

;; just for titles
(define (it . text)
  (F: (if TEXT? @~splice{«@|text|»} @~splice{*“@|text|”*})
      @~splice{\textit{@text}}))

(define (em . text)
  (F: @~splice{*@|text|*}
      @~splice{\emph{@text}}))

;; the date/loc are not rendered in md
;;   add the date with #:md-* and a `D` in the string
(define (o #:date date #:title title #:loc [loc #f]
           #:dname [dname title] #:datespec [datespec #f] #:short [short #f]
           #:title-sfx [tsfx #f]
           #:md-pfx [mpfx #f] #:md-title-sfx [mtsfx tsfx]     ; <- MD flags
           #:tex-title-sfx [ttsfx tsfx] #:tex-nobr [tnobr #f] ; <- TEX flags
           #:if [bool #t]
           . text)
  (when bool
    (define (dsubst str)
      (and str date (regexp-replace #px"\\bD\\b" (->string str) date)))
    (list (and dname date (λ() (date-info 'item! dname date datespec short)))
          (F: @~splice{
                @(: (dsubst mpfx) title (dsubst mtsfx))
                @text}
              @~splice{
                \cvplain{@date}{@loc}{@|title|@ttsfx}
                @(and (not tnobr) "\n")@;
                @text
                @||}))))

;; ---->> TeX customizastion --------------------------------------------------

(define (tex-prefix)
  @T:{\documentclass[11pt, letterpaper]{awesome-cv}
      \geometry{left=1.4cm, top=.8cm, right=1.4cm, bottom=1.8cm, footskip=.5cm}
      \fontdir[fonts/]
      \setmonofont{Consolas}
      \definecolor{awesome-mine}{HTML}{5500CC}
      \definecolor{darklink}{HTML}{4400AA}
      \colorlet{awesome}{awesome-mine}
      \setbool{acvSectionColorHighlight}{true}
      \hypersetup{colorlinks=true,linkcolor=darklink,urlcolor=darklink,citecolor=darklink}
      \renewcommand{\acvHeaderSocialSep}{\quad—\bullet—\quad}
      \name{@(regexp-replace #rx" ([^ ]+$)" (meta-ref name) "}{\\1")}
      \title{@meta-ref[title]}
      \address{@meta-ref[address]}
      \mobile{@meta-ref[phone]}
      \phone{@meta-ref[phone2]}
      \email{@meta-ref[email]}
      \homepage{@(regexp-replace #rx"/$" (meta-ref web) "")}
      \github{@meta-ref[github]}
      \linkedin{@meta-ref[linkedin]}
      \begin{document}
      \makecvheader[C]
      \makecvfooter{}{@meta-ref[name]~~~·~~~@meta-ref[title]}{\thepage}
      \vskip 0mm})

(define (tex-suffix)
  @T:{\end{document}})

;; ---->> References ----------------------------------------------------------

(define all-sections '())
(define current-section #f)

(define get-ref-hash
  (let ([n 0] [hashes (make-hash)])
    (λ(x)
      (define hash (number->string (abs (equal-hash-code x)) 16))
      (define H (~a hash #:width 4 #:pad-string "0" #:align 'right))
      (define bad (hash-ref hashes H #f))
      (when bad (error 'get-ref-hash "hash collision for ~s and ~s" bad x))
      (hash-set! hashes H x)
      H)))

(define (date-info . msg)
  (define (done)
    (when (and current-section (pair? (cdr current-section)))
      (push! (reverse current-section) all-sections)))
  (define (keyval xs sep)
    (add-between
     (filter-map (λ(kv) (and (cadr kv) @:{@(car kv): "@(cadr kv)"})) xs)
     (list "," sep)))
  (define (entry info)
    @splice{{
      @block{@(keyval info "\n")}
    }})
  (define (section x)
    @splice{@(format "~s" (car (car x))): {
              @(keyval (cadr (car x)) " "),
              @block{entries: [@(splice (add-between (map entry (cdr x))
                                                     ", "))]}}})
  (define (json)
    @splice{{
      @(block (add-between (map section (reverse all-sections)) ",\n"))
    }})
  (match msg
    [`(section! ,title ,sec-dates)
     (done)
     (set! current-section (and title sec-dates `((,title ,sec-dates))))]
    [`(item! . ,(list name datestr datespec short))
     (define (->simple x)
       (and x (parameterize ([simple-text? #t]) (string-trim (->string x)))))
     (when (and current-section name datestr datespec)
       (let* ([name  (->simple name)]
              [short (->simple short)])
         (define R (get-ref-hash (list (or short name) datespec)))
         (define info `([datestr ,datestr] [name ,name] [R ,R]
                        [D ,datespec] [short ,short]))
         (push! info current-section)
         (and M? (not TEXT?) @:{<span id="R@R"></span>})))]
    ['() @:{const dateInfo = @json;
            @||}]))

;; ---->> Rendering -----------------------------------------------------------

(define (tex-writer str p [start 0] [end (string-length str)])
  ;; \n\n\n doesn't really work, since this works on written fragments
  (define tex-rx #rx"([&#])|(\n\n\n+)")
  (let loop ([start start])
    (define m (and (< start end)
                   (regexp-match-positions tex-rx str start end p)))
    (when m
      (define-values [1st 2nd] (apply values (cdr m)))
      (cond [1st (write-string (format "\\~a" (string-ref str (car 1st))) p)]
            [2nd (write-string "\n\n" p)]
            [else (error 'tex-writer "internal error")])
      (loop (cdar m)))))

(define (render!)
  (port-count-lines! (current-output-port))
  (define text (list (reverse parts) "\n"))
  (define r (->string (F: text (with-writer tex-writer text))))
  (output (if T? (regexp-replace* #rx"\n\n\n+" r "\n\n") r))
  (define dates-file (getenv "dates_file"))
  (when (and dates-file (not (equal? "" dates-file)))
    (with-output-to-file dates-file #:exists 'truncate
      (λ() (output date-info)))))

(define-syntax-rule (mod-beg (meta: m ...) x ...)
  (#%module-begin
    (meta: m ...)
    (part! (tex-prefix))
    x ...
    (part! (tex-suffix))
    (render!)))
