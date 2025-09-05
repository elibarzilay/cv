#!/usr/bin/env racket

#lang at-exp s-exp "lib.rkt"

(define name     "Eli Barzilay")
(define title    "Curriculum Vitae")
(define email    "eli@barzilay.org")
(define web      "barzilay.org")
(define phone    "+1-617-372-2483")
(define phone2   "+1-617-383-9313")
(define phones   @:{@phone / @phone2})
(define address  (V: "Brookline, MA 02446"
                     "19 Winchester St, Apt #610, Brookline, MA 02446"))
(define github   "elibarzilay")
(define linkedin "eli-barzilay")

(define (main)

(define oo #f)
(part! (tex-prefix))

(section*! @:{@|name|: @title}
  @SM:{@url[email]@";" @url[web]}
  @SM:{@|phone|@";" @address}
  ;;
  @LM:{Email:   @url[email]}
  @LM:{Web:     @url[web]}
  @LM:{Phone:   @phones}
  @LM:{Address: @address})

(part! (sec++))

(section*! "Quick Overview"
  @o[L? "2022–2025"]{
    Principal Software Engineer at GoDaddy, August 2022–August 2025
    @(*: @:{Code infrastructure, repo & build pipeline migrations}
         @:{Lead work on code quality tools}
         @:{Backend work: Services, APIs, Automation, Docker, AWS}
         @:{Heavy cross-team work})}
  @o[L? "2022"]{
    Principal Software Engineer at Sundae, February–June 2022
    @(*: @:{Backend design and API implementation})}
  @o[L? "2015–2022"]{
    Senior Software Developer at Microsoft, December 2015–January 2022
    @(*: @:{Azure Machine Learning group in Boston}
         @:{Python Group}
         @:{TypeScript Group})}
  @o[L? "1992–Present"]{
    Professional Programmer since 1992
    @(*: @:{Many languages, focus on functional programming}
         @:{Strong academic background, specializing in Programming Languages
            and Formal Methods}
         @:{Many environments, fluent in Linux}
         @:{Preferable focus on backend/full-stack work, experience at all
            levels})}
  @(set! oo (curry o L? #:md-sfx (list "," \\ "\nD")))
  @oo["1994–Present"]{Teaching:
    Programming Languages, Programming & Fundamentals}
  @oo["1994–2014"]{Research:
    Programming Languages, Theorem Proving, Computer Music}
  @oo["1991–2005"]{Education:
    B.Sc., M.Sc., Ph.D. in Computer Science})

(section*! "Industry Experience" #:sec-dates '([side up])
  @o["August 2022 — August 2025"
     #:md-pfx "D: " #:dinfo `([D "2022-08-15::Present"])]{
    GoDaddy
    @(*: @S:{Joined the recently-acquired Poynt team.}
         @L:{Looking for a new startup-style place, I was pulled into a GoDaddy
             group which was a recent enough acquisition of a startup (Poynt)
             making for a startup-like work environment.}
         @S:{Main focus on migration and modernization of a huge project with
             many repositories.}
         @L:{My focus was on migration and modernization of a huge project with
             many repositories, which was mostly maintained in on-premise GHE
             with an outdated Jenkins, to a modern GHEC and using GH Workflows.}
         @S:{Another important area: promote code quality via automations.}
         @L:{I also served as a "stabilizing force" for a hectic environment.
             The recent move has led to quality problems, and constant fires.
             I worked towards improving this and help move things in the desired
             direction while improving stability. In addition to the work that
             I did, this also involved developer education: both on technical
             subjects that many find hard (eg, bash scripting), and on a more
             abstract level (eg, proper code ownership).}
         @S:{Also included a lot of typical backend work: APIs of all kinds,
             services, automations, docker images, AWS deployments. This involved
             a lot of cross-team and -org work.}
         @L:{As part of and in addition to the above, there was the usual
             typical backend kind of work: APIs of all kinds (code- and
             build-related GH APIs)@";" services (implementing our own APIs)@";"
             automations (GH workflows, managing deployed nodes, cron jobs,
             document batch processing)@";" docker images (as well as working with
             k8s and AWS images)@";" AWS deployments (migrating deploys from a
             jenkins pipeline pushing to a PM2-managed server nodes to a modern
             image-based deployment tool). The nature of my work was always heavy
             on cross-team impact (code quality for several teams) as well as
             cross-org (mainly with the central engineering division).})}
  @o["February 2022 — June 2022"
     #:md-pfx "D: " #:dinfo `([D "2022-02-28::2022-06-17"])]{
    Sundae
    @(*: @S:{Joined Sundae on February 2022 as a Principal Software Engineer.}
         @L:{Following a decision to switch to a more greenfield-style work, I
             joined a startup, Sundae, as a Principal Software Engineer.}
         @S:{Work focused on improving the young codebase, implementing new
             APIs, handling "special projects", and making the code base more
             robust.}
         @L:{During this time my work was mainly focused on improving the very
             young codebase, as well as implementing new APIs.  The former was
             mainly making the code robust by improving the TypeScript type
             declarations, and revamping the overall design of the system. Also
             included were various "special projects".}
         @S:{I enjoyed Sundae, but the faltering housing market in June 2022 made
             the company drop most of its software engineering effort.}
         @L:{I greatly enjoyed my time at Sundae which went very well, affirming
             my decision to leave Microsoft.  However, the faltering housing
             market in June 2022 made the company drop most of its software
             engineering effort.})}
  @(set! oo (λ (date D title #:short [short title] . xs)
              (apply o #:md-pfx "D: " #:dinfo `([D ,D] [short ,short])
                     #:dname @:{Microsoft: @title} date title xs)))
  @oo["October 2019 — January 2022" "2019-10-01::2022-01-31"]{
    @url{TypeScript | www.typescriptlang.org/}
    @(*: @S:{Code maintenance (language server, vscode, builds, DefinitelyTyped)}
         @L:{Day-to-day work on bugs in TS and related systems (language server,
             vscode, builds, DefinitelyTyped maintenance)}
         @S:{Tracing profiler}
         @L:{Implemented TypeScript's tracing profiler (with Andrew Casey), which
             helps people debug compilation performance issues}
         @S:{DefinitelyTyped mergebot}
         @L:{@W{Implementation of DefinitelyTyped's
                @url{mergebot | github.com/DefinitelyTyped/dt-mergebot}: a
                github bot that enables self-maintenance, managing the huge
                load of incoming PRs (hundreds per week)}}
         @S:{Conversion of TypeScript to JS modules (ongoing)}
         @L:{Conversion of the TypeScript code base to JS modules (ongoing)})}
  @oo["March 2018 — October 2019" "2018-03-01::2019-09-30" #:short "Azure Python"]{
    Python group
    @(*: @S:{The main Microsoft offering for running Jupyter notebooks}
         @L:{The main Microsoft offering for running Jupyter notebooks (iPython)
             online}
         @S:{Focus on infrastructure work: server, docker images, network design}
         @L:{Focus on infrastructure work: server maintenance, docker images
             infrastructure (library collection, image builds, publish, internal
             distribution), and network design (including Kubernetes compute
             backends on Azure)})}
  @oo["Mid 2016 — March 2018" "2016-07-01::2018-02-28"]{
    @url{MMLSpark | github.com/microsoft/SynapseML/}
    @(*: @S:{Open source core library for Azure ML, built from scratch}
         @L:{Open source project, core library for the Vienna project (Azure ML)}
         @L:{Code development from the project's very beginning}
         @S:{Designed, implemented, and maintained all infrastructure}
         @L:{Completely designed and implemented the project's infrastructure,
             including implementation of many VSTS features that are unavailable
             for a public project (e.g., maintaining a VSTS build, publish
             artifacts and documentation, PR builds, code style and clean git
             history)}
         @S:{PR coordinator and release manager}
         @L:{Also served as a PR coordinator and release manager for the project,
             including all public artifacts (jars, spark packages, python
             packages, docker images, etc)}
         @L:{Helped the rest of the team to get more comfortable with functional
             programming (in Scala), replace imperative idioms by functional
             ones, and learn about more advanced functional programming patterns}
         @L:{The project was later renamed “SynapseML”})}
  @oo["December 2015 — March 2018" "2015-12-01::2018-03-01" #:short "AzureML Studio"]{
    @:{Azure Machine Learning Studio@L:{ (Boston)}}
    @(*: @S:{Frontend visualization (infinite grid scroller w/ on-demand service)}
         @L:{Front-end visualizations, mainly table view of infinite matrix,
             backed by an on-demand service}
         @S:{Mostly done in TypeScript}
         @L:{Mostly done in TypeScript@";" also involved Visual Studio, TFS, and
             micro-services})}
  @o["December 2015 — January 2022" #:dinfo `([D "2015-12-01::2022-01-31"])
     #:md-pfx "D: "]{
    Microsoft@(M: (and (not TEXT?) " \\"))
    @V:[@:{Joined MS as a Senior Software Dev. in late 2015.}
        @:{In 2014 I decided to switch from academia to industry work.  After a
           few short projects I joined Microsoft as a Senior Software Developer
           in late 2015.}]}
  @o[L? NODATE]{
    Used numerous technologies (abridged list):
    @(*: @:{Platforms: Linux, Windows, Hyper-V, MS Azure, AWS,
            GitHub & GitLab (incl. API, Automation, DevOps, etc), GraphQL,
            Docker}
         @:{Build/code management: Git, Bash, Docker}
         @:{Languages: TypeScript, JavaScript, Python, Bash, Powershell, Scala,
            Java, R, ARM Templates, Lisp(s)})})

(part! (L: (header "Academic") (sec++)))

(define research-interests
  @splice{Programming Languages (design and implementation),
          Formal Languages, Meta Programming, Reflection.})
(section! "Research Interests"
  (@L: research-interests))

(section*! "Education" #:sec-dates '([side down])
  (edu "1997–2003" "1997-01-01::2003-07-31"
       @:{@phd in Computer Science} "CS PhD @ Cornell"
       cornell "Prof. Robert Constable" "Implementing Reflection in Nuprl")
  (edu "1994–1996" "1994-09-01::1996-12-31"
       @:{@msc in Computer Science (summa cum laude)} "CS MSc @ BGU"
       bgu "Prof. Mira Balaban" "Framework for Creative Editing")
  (edu "1991–1993" "1991-09-01::1994-06-30"
       @:{@bsc in Math & Computer Science (cum laude)} "CS+Math BSc @ BGU"
       bgu #f #f))

(section*! "Teaching Experience" #:sec-dates '([side down])
  @(set! oo (curry o #:md-pfx "D: "))
  @oo["2004–Present" #:loc "Northeastern"
      #:dname "Programming Languages, Northeastern"
      #:dinfo '([D "2004-01-01::NOW"] [short "Programming Languages"])]{
    Lecturer, Northeastern University
    @L:["\n"]@;
    @V:[@:{Teaching CSU4400/CSG5400, Programming Languages (ugrad + master)}
        @(*: @:{CSU4400/CSG5400 (@url{pl.barzilay.org/})@\\
                Programming Languages (combined master and undergraduate levels).
                @||
                Reconstructed the course materials based on a (then new) textbook
                from Brown University, @it{Programming Languages: Application and
                Interpretation}, by Shriram Krishnamurthi.  Eventually, the
                material had evolved to much more than the book, adding completely
                new and detailed chapters on some advanced topics in programming
                languages that are either not skimmed or not included in the book.
                Examples are lambda calculus, macros, type systems, domain-specific
                languages, and continuations.  This was taught every semester from
                2004 until the present.  (Combined with the graduate course since
                the fall semester of 2008.)
                @||
                In addition, I designed a (Racket-based) paperless system for
                homework submission, grading, and for exams.  While some of these
                systems are no longer used, I keep implementing new ones as needed
                (for example, the exam server/client was replaced by a WPA in-class
                quiz, @url{plq.barzilay.org/}) — up to 100 commits in most weeks.
                @||
                To support the course material, I designed a large number of
                S-expression-based languages with wildly different semantics: a lazy
                language, an implicitly-curried language, a fast lambda-calculus
                language (compiled to Racket rather than a rewrite engine
                implementation), a dynamically-scoped language, a language with
                ML-like scope for top-level definitions and more.  This is in
                addition to the main language, which is a variant of Typed Racket,
                extended with disjoint sum types.
                @||
                My materials were taken by a number of people, and they are
                currently being taught in Canada, Korea, and in Israel.}
             @:{CSU213, Fundamentals of Computer Science II, Spring 2006.})]}
  @oo["1997–2000" #:loc "Cornell"
      #:dname "Teaching Assistant, Cornell"
      #:dinfo '([D "1997-01-01::2000-05-31"] [short "TA @ Cornell"])]{
    Teaching Assistant, Cornell University
    @L:["\n"]@;
    @V:[@:{CS212, Structures and Interpretation of Computer Programs}
        @(*: @:{CS212, Structures and Interpretation of Computer Programs.
                @||
                Conducted recitation sessions, participated in developing homework
                materials and exams and graded them during the two semesters of fall
                1997 and spring 2000.  From 1998 to 2000, I designed, implemented,
                and maintained Swindle, a rich language implemented on top of
                PLT-Scheme that replaced a Dylan-like environment that was
                previously used in the PL course.  This system provides a CLOS-like
                object oriented environment and many other features required to make
                it viable for the material.  The new language was used in the course
                until 2000, when the course was re-revised by newer staff and
                switched to SML.  During this period, and in a number of years
                following it, I kept in touch with the course teams, was involved in
                course material development, and gave occasional guest lectures.
                @||
                Swindle was used for many years in a number of additional colleges
                and universities, including Dartmouth, Vassar and Duke.  In some, it
                is being used to this day.  (It is part of the Racket distribution.)})]}
  @oo["1994–1996" #:loc "BGU, Israel"
      #:dname "Teaching Assistant, BGU"
      #:dinfo '([D "1994-09-01::1996-12-31"] [short "TA @ BGU"])]{
    Teaching Assistant, Ben-Gurion University
    @L:["\n"]@;
    @V:[@:{Automata and Formal Languages, Structures and Interpretation of Computer
           Programs, Advanced Programming Languages}
        @:{@(*: @:{Automata and Formal Languages}
                @:{Structures and Interpretation of Computer Programs}
                @:{Advanced Programming Languages})
           @||
           Conducted recitation sessions, graded exams, helped in extending
           course contents and developed new material.}]})

(section*! "Research Experience" #:sec-dates '([side down])
  #:pfx @S:{Research Interests: @research-interests
            @||}
  @(set! oo (curry o #:md-pfx "D: " #:nobr #t))
  @oo["2003–2014" #:loc "Northeastern"
      #:dname "Researcher, Programming Research Laboratory, Northeastern"
      #:dinfo '([D "2003-09-01::2014-07-01"] [short "PRL, Northeastern"])]{
    Researcher, Programming Research Laboratory,
    Northeastern University, working with Prof. Matthias Felleisen@;
    @L:{@"\n"
        During this time, I was part of the small core development team behind
        Racket (formerly PLT Scheme) and helped grow it into the “language
        greenhouse” that it is today.  Specifically, I have designed and
        implemented a number of important core features in the language:
        @||
        @(*: @:{A libffi-based dynamic foreign interface, which allows interfacing
                foreign libraries from Racket.  This interface has replaced the
                previous ad-hoc system, and enabled rapid development in many
                important areas such as a platform-independent GUI system (a
                re-implementation of wxWindows in Racket), a portable database
                interface, an OpenGL library, and many more.}
             @:{A Lazy Racket language that is similar in its syntax to Racket, but
                different in its execution semantics.  This implementation paved the
                way to a number of similar important languages, such as Typed
                Racket.}
             @:{A concrete syntax that makes it possible to deal with text-rich
                code, in a way that provides functionality similar to here-docs and
                string interpolation, yet is more integrated in the language than
                common approaches.  This syntax forms the bases for a family of
                domain specific languages that are used for Racket's documentation
                system, textbooks, articles, and more.}
             @:{I have created the group's infrastructure and everything that is
                involved, and partially served as a system administrator to maintain
                it.  This included substantial technical pieces like making up the
                Racket build process and web page generation as well as social
                aspects like setting up mailing lists and other community building
                efforts.}
             @:{Numerous other projects within the Racket community and the PLT
                research group.  Some of these would be considered as major language
                features in other languages, but in Racket they are just libraries.
                For example, I have implemented Racket's generators, and its system
                of generic functions.})}}
  @oo["1997–2003" #:loc "Cornell"
      #:dname "Graduate Research Assistant in the Nuprl group, Cornell"
      #:dinfo '([D "1997-01-01::2003-07-31"] [short "Nuprl, Cornell"])]{
    Graduate Research Assistant in the Nuprl group,
    directed by Prof. Robert Constable, Cornell University@;
    @L:{@"\n"
        The focus of this research was the implementation of a reflection
        system for the Nuprl theorem prover, covering both the practical and
        the theoretical aspects.  Made additional contributions to Nuprl, and
        participated in the development of the MetaPRL theorem prover
        (designed and implemented by Jason Hickey).}}
  @oo["1994–1996" #:loc "BGU, Israel"
      #:dinfo '([D "1994-09-01::1996-12-31"] [short "Computer Music @ BGU"])]{
    Research Assistant, Ben-Gurion University@;
    @L:{@"\n"
        Developed a Common Lisp based system for creative music editing as
        part of my Masters thesis.  This research was centered around an
        implementation of a musical composition tool, resulting in a system
        that is essentially a visual functional language.  The system was
        shown to be useful in other forms of structured creative editing such
        as graphic editing.  The implementation was heavily influenced by
        programming languages work combined with operational graph editing,
        which are used to form user-defined reusable abstractions.}})

(section! "Ph.D. Dissertation"
  @o[L? "1997–2003" #:loc "Cornell"]{
    @it{Implementing Reflection in Nuprl}
    @||
    Nuprl is a theorem prover, a system that materializes the relation
    between logic and programming languages.  The logical aspects of such an
    environment naturally pulls any implementation of reflection towards a
    reimplementation of the logical system within itself.  This approach is
    adequate for achieving theoretical results (e.g., Godel numbers), but
    for actual work where reflection is needed, it is impractical.
    @||
    My research work on implementing reflection for Nuprl borrowed heavily
    from common practices in programming languages, and most specifically
    from Lisp and Scheme: achieve reflection through exposure of internal
    system-level functionality to the user-level.  By design, this is a
    @em{strong reflection} principle, meaning that the reflected system is
    inherently identical to the base system.  This work diverged from the
    more common logical tradition of a weak reflection, where a separate
    description of the reflected layer is provided, requiring proof that the
    resulting reflective layer is equivalent to the source layer.
    @||
    Implementing this kind of reflection leads to similar issues as in
    reflective programming languages.  Most notably, I used a higher order
    abstract syntax (HOAS) approach for syntax representation, and
    implemented operations on these new constructs.  The theoretical side of
    this work, however, went beyond what is done on the PL side, requiring a
    specification of the semantics for encoded quotations of syntax with
    bindings, resulting in new logical rules for the system.  The major
    contribution of this work is in closing the gap between the logical
    world and the programming languages world in the context of reflection.
    Much of this work has influenced the reflection work in MetaPRL too, and
    many years later it has even inspired a textual language for Racket
    which is in heavy use today (Scribble).})

(section*! "Publications" #:sec-dates '([side down])
  (pub "MMLSpark: Unifying Machine Learning Ecosystems at Massive Scales"
       "MMLSpark" "Mark..."
       "2018" "2018-10-20"
       "arXiv 1810.08744")
  (pub "A Programmable Programming Language"
       "Programmable PL" "Matthias..."
       "2018" "2018-05-01"
       "Communications of the ACM (CACM)")
  (pub "The Racket Manifesto"
       "Racket Manifesto" "Matthias..."
       "2015" "2015-05-01"
       "SNAPL: The Inaugural Summit on Advances in Programming Languages")
  (pub "Keeping it Clean with Syntax Parameters"
       "Syntax Parameters" "Eli, Ryan, Matthew"
       "2011" "2011-10-23"
       "Workshop on Scheme and Functional Programming")
  (pub "From Stack Traces to Lazy Rewriting Sequences"
       "Lazy Rewriting" "Stephen, Eli, John, Matthias"
       "2011" "2011-10-01"
       "Implementation and Application of Functional Languages")
  (pub "The Scribble Reader"
       "Scribble Reader" "Eli"
       "2009" "2009-08-15"
       "Workshop on Scheme and Functional Programming")
  (pub "Keyword and Optional Arguments in PLT Scheme"
       "Keyword Args" "Matthew, Eli"
       "2009" "2009-08-01"
       "Workshop on Scheme and Functional Programming")
  (pub "Scribble: Closing the Book on Ad Hoc Documentation Tools"
       "Scribble" "Matthew, Eli, Robby"
       "2009" "2009-09-01"
       "International Conference on Functional Programming")
  (pub "A Self-Hosting Evaluator using HOAS"
       "HOAS" "Eli"
       "2006" "2006-09-17"
       "Workshop on Scheme and Functional Programming")
  (pub "Implementing Direct Reflection in Nuprl"
       "Direct Reflection" "Eli"
       "2006" "2006-01-01"
       "Ph.D. Thesis")
  (pub "Laziness Without All the Hard Work: Combining Lazy and Strict Languages for Teaching"
       "Lazy Racket" "Eli, John"
       "2005" "2005-03-01" ; moved from 2005-09-01
       "Functional and Declarative Programming in Education")
  (pub "Foreign Interface for PLT Scheme"
       "FFI" "Eli, Dmitry"
       "2004" "2004-09-01"
       "Workshop on Scheme and Functional Programming")
  (pub "MetaPRL — A Modular Logical Environment"
       "MetaPRL" "Jason..."
       "2003" "2003-08-01"
       "International Conference on Theorem Proving in Higher Order Logics")
  (pub "Practical Reflection in Nuprl"
       "Practical Reflection" "Eli, Stuart, Robert"
       "2003" "2003-06-01"
       "IEEE Symposium on Logic in Computer Science")
  (pub "Reflecting Higher-Order Abstract Syntax in Nuprl"
       "Nuprl HOAS" "Eli, Stuart"
       "2002" "2002-07-01" ; moved from 2002-08-01
       "Theorem Proving in Higher Order Logics; Track B Proceedings of the 15th International Conference on Theorem Proving in Higher Order Logics (TPHOLs), pp. 23–32, NASA")
  (pub "Abstraction as a Means for End-User Computing in Creative Applications"
       "Creative Abstr." "Mira, Eli, Michael"
       "2002" "2002-05-01" ; moved from 2002-11-01
       "IEEE Transactions on Systems, Man, and Cybernetics, Part A, Vol. 32, No. 6, pp. 640–653")
  (pub "Quotation and Reflection in Nuprl and Scheme"
       "Quotation+Reflection" "Eli"
       "2001" "2001-01-01"
       "Cornell University technical report TR2001-1832")
  (pub "Booms Object Oriented Music System"
       "Booms" "Eli"
       "1997" "1996-12-31"
       "M.Sc. Thesis"))

(part! (L: (sec--)))

(section*! "Older Experience" #:sec-dates '([side up])
  @(set! oo (λ(date D title #:short [sh #f] . xs)
              (apply o #:md-pfx "D: " #:nobr #t date #:dname title
                     #:dinfo `([D ,D] ,@(if sh `([short ,sh]) '()))
                     @:{@title —} @V:[" " "\n"] xs)))
  @oo["2014–2015" "2014-05-01::2015-10-01"
      "Gefen Dekel"]{
    @V:[@:{Senior software dev, desktop apps (Chromium)}
        @:{Senior Software Developer
           @||
           Helped embedding Chromium into Dalet's desktop application, later
           followed up with learning about how React can be used in a
           browser-based editor, and eventually ran a two day tutorial on React,
           to ensure that it will actually get used.}]}
  @oo["2003–2014" "2003-09-01::2014-07-01"
      "Systems / Infrastructure" #:short "Sys/Infra @ PLT"]{
    @V:[@:{as part of the Racket (PLT) group}
        @:{as part of the PLT group behind Racket, I was in charge of the framework of
           servers, builds, releases, code repository and all other services that are
           required to run such a project.}]}
  @oo["1995–1997" "1995-01-01::1996-12-31"
      "Gefen-Dekel"]{
    @V:[@:{Multi-media applications for radio stations}
        @:{Was among the first programmers of Gefen Dekel, the software development
           branch of Dalet Digital Media Systems. Developed multi-media applications
           for Windows, aimed mainly at radio station software and advertisement
           management.}]}
  @oo["1994" "1994-05-01::1994-12-31"
      "Shibutzit"]{
    @V:[@:{Prolog-based application for HR allocations}
        @:{Summer work in Shibutzit, a small company, developing a Prolog-based
           application for Windows that was used for allocation of human resources.}]}
  @oo["1993" "1993-08-01::1994-06-30"
      "System Administrator" #:short "Sysadmin @ BGU CS"]{
    @V:[@:{Various Unix systems}
        @:{During the last year of my undergraduate studies, I worked as part of the
           system administration group for the Computer Science department in Ben-Gurion
           University.}]})

(section*! "Old Major Projects" #:sec-dates '([side up])
  #:pfx @L:{(Approximate dates.)}
  @(set! oo (λ(date D title #:short [sh #f] #:dinfo [di '()] . xs)
              (let* ([title (->string title)]
                     [ch? (regexp-match? #rx"[^a-zA-Z0-9]$" title)]
                     [ch  (and ch? (substring title (sub1 (string-length title))))]
                     [title* (if (not ch?) title
                                 (substring title 0 (sub1 (string-length title))))]
                     [xs (apply : xs)]
                     [xs (or xs '())])
                (apply o #:md-pfx "D: " #:nobr #t date #:dname title*
                       #:dinfo `([D ,D] ,@(if sh `([short ,sh]) '()) ,@di)
                       (list (: title* (ST: ch))
                             (V: (: (M: ch (and (pair? xs) " ")))
                                 (F: "\n" "")))
                       xs))))
  @oo["2008–2010" "2008-01-01::2010-12-31"
       "The Scribble Reader:" #:short "Scribble"]{
    @V:[@:{syntax for text-rich code (docs, html, etc)}
        @:{@||
           Designed a concrete syntactic extension to Racket that combines the
           convenience of “here-documents” and string interpolation, while
           maintaining the benefits of S-expressions.  The idea behind the syntax
           is based on the Nuprl quotations, which result in a language that is
           easier to use than quasi-quotations.  The syntax was named “Scribble”,
           and later evolved into Racket's documentation system, making one of
           the earlier cases of using Racket as a real language framework, where
           a single program can be written multiple languages that are different
           even at the concrete syntax level.}]}
  @oo["2005–2006" "2005-01-01::2006-12-31"
      "Lazy Racket:"]{
    @V:[@:{identical to Racket but with lazy execution}
        @:{@||
           Implemented a Lazy Scheme language for PLT Scheme (later called Lazy
           Racket).  This is a language that is identical to Scheme except for
           its lazy execution semantics.  It was developed for the PL course, but
           the result is a powerful ability to mix lazy and strict code in a
           single language, using Racket's module system.  This was in addition
           to other languages developed for the course.}]}
  @oo["2004–2007" "2004-01-01::2007-12-31"
      "Handin server and client," #:short "Handin Server+Client"]{
    @V:[@:{and other teaching related software}
        @:{@||
           Extended the Racket Handin server and client (originally developed by
           Matthew Flatt) considerably, making it usable in courses with widely
           varying demands.  In addition, designed a completely automated system
           to deal with running the PL course, including website publishing and
           on-line content (class notes, homework handins, etc.), electronic
           grading, secure grades publishing with statistics and graphs,
           class-participation tool, a kiosk-mode client for taking exams, and
           more.}]}
  @oo["2003–2004" "2003-01-01::2004-12-31"
      "Foreign Function Interface for Racket," #:short "Racket FFI"]{
    @V:[@:{design & implementation}
        @:{@||
           Designed and implemented Racket's foreign function interface for
           Racket, replacing a more traditional C-based approach where Racket
           code replaces the role of C.}]}
  @oo["2003–2014" "2003-09-01::2014-07-01"
       "PLT / Racket Infrastructure," #:short "Racket Infrastructure"]{
    @V:[@:{development & maintenance}
        @:{@||
           Implemented the build process for making nightly builds of Racket as
           well as official distributions, revised the web-content framework
           using the new Scribble-based syntax as a markup language, was in
           charge of moving the Racket code base from CVS to Subversion first and
           then to git next.}]}
  @oo["1999–2003" "1999-01-01::2003-07-31"
      @V:[@:{Reflection for the Nuprl theorem prover}
          @:{Reflection for Nuprl}]]{
    @L:{@||
        Implemented reflection for the Nuprl theorem prover.}}
  @oo["2001" "2001-01-01::2001-12-01"
      @V:[@:{Web-server implementation in Scheme, with dynamic code execution}
          @:{Web-server}]]{
    @L:{@||
        Wrote a complete web-server in Scheme, with the purpose of being very
        flexible and for making an on-line picture collection.}}
  @oo["1999" "1999-01-01::1999-12-31"
      "Picture collection management" #:short "Picture collection"]{
    @L:{@||
        Designed and implemented a set of tools for managing a picture
        collection, as part of a photography project (for a minor in art at
        Cornell).}}
  @oo["1999" "1999-01-01::1999-12-31"
      "HTML generation"]{
    @V:[@:{(precursor to Scribble)}
        @:{@||
           Wrote a system to easily generate HTML code in an easily extensible
           way, relying on such features as self-quoting code and keyword
           argument.  This system was used for several years by a few academic
           and commercial sites.  Years later it evolved into the Scribble
           syntax.}]}
  @oo["1998" "1998-01-01::1998-12-31"
      "Swindle:"]{
    @V:[@:{MOP-based object system, used in a few universities}
        @:{@||
           Implemented Swindle: an extension language on top of PLT-Scheme/Racket
           for CS212 in the Cornell CS department.  Swindle was in active use at
           several universities for many years, and still is in use in some
           places.}]}
  @oo["1997" "1996-09-01::1997-12-31"
      "Emacs calculator"]{
    @V:[@:{(part of the standard Emacs distribution)}
        @:{@||
           Implemented an interactive calculator package which is now a standard
           part of Emacs.}]}
  @oo["1995–1996" "1995-01-01::1996-12-31"
      "Booms:"]{
    @V:[@:{A visual language for music composition}
        @:{@||
           A visual language environment for creative editing, mainly intended
           for music composition.  Implemented in Allegro Common Lisp for
           Windows.}]}
  @oo["1994–1996" "1995-01-01::1996-12-31"
      "Multeam:"]{
    @V:[@:{A Usenet-like system for media content}
        @:{@||
           A Usenet-like system for managing media content which was developed at
           Gefen-Dekel Technologies (part of Dalet Digital Media Systems).  The
           original project was discontinued but served as a basis for the “Dalet
           Web Publisher” which was eventually integrated in Dalet's main
           application — DaletPlus, and later Dalet Galaxy.}]})

(section*! "Personal" #:itemize (F: *: cvitemize:)
  @L:{Citizen of Israel, permanent US residence since April 2006.}
  @L:{Work preference:
      @(*: @:{Functional programming.}
           @:{Building new projects rather than maintenance of existing software.}
           @:{Languages:
              @(*: @:{I love hacking Racket; specifically, creating little languages and
                      using its meta-programming capabilities.}
                   @:{More recently I switched to JavaScript/TypeScript as my language
                      of choice in commercial settings.})}
           @:{Remote or hybrid in the Boston area.})}
  @L:{Photography: A minor in Art at Cornell, mostly digital photography.}
  @L:{Music: I love electronic music and experiment with making some as well as
      hacking on my own music player (implemented as a web app).})

(part! (tex-suffix))

)

(define phd (V: "PhD" "Ph.D."))
(define msc (V: "MSc" "M.Sc."))
(define bsc (V: "BSc" "B.Sc."))
(define cornell
  (list (V: "Cornell University"
            "Cornell University, New York, NY, USA")
        "Cornell"))
(define bgu
  (list (V: "Ben-Gurion Univ, Israel"
            "Ben-Gurion University of the Negev, Be'er-Sheva, Israel")
        "BGU, Israel"))
(define (edu date D title short where advisor subject)
  (let ([advisor (and advisor @:{Advisor: @advisor,})]
        [subject (and subject (V: (it subject) @:{Subject: @(it subject),}))]
        [longloc @:{@(car where).}]
        [title (->string title)])
    (o date #:dname (list (regexp-replace #rx" *\\([^()]+\\)$" title "")
                          ", " (cadr where))
       #:dinfo `([D ,D] [short ,short]) #:loc (cadr where) #:md-pfx "D: "
       title @M:{,@S:[@\\]
                 @||}
       (if SM?
         @:{@(car where)@and[subject]{@splice{,@\\
            @subject}}.}
         (*: advisor subject longloc)))))

(define all-authors
  '("Eli Barzilay"
    "Mark Hamilton"
    "Matthias Felleisen" "Matthew Flatt" "Robby Findler" "John Clements"
    "Ryan Culpepper" "Stephen Chang"
    "Dmitry Orlovsky"
    "Robert Constable" "Stuart Allen" "Jason Hickey"
    "Mira Balaban" "Michael Elhadad"))
(define (pub title short authors date D where)
  (define (auth a)
    (define full (findf (λ(full) (string-prefix? full a)) all-authors))
    (V: (regexp-replace #rx"^.* " full "") full))
  (let* ([et-al?  (regexp-match? #px"\\.{3}$" authors)]
         [authors (if et-al? (regexp-replace #rx" *\\.*$" authors "") authors)]
         [authors (regexp-split #rx" *, *" authors)]
         [authors (add-between (map auth authors) '(", ") #:splice? #t
                               #:after-last (if et-al? '(" et al") '()))]
         [title*  @it{@title}])
    (o date #:dname title #:dinfo `([D ,D] [short ,short]) #:nobr S?
       (F: "" title*)
       (F: (V: @:{@W{@|title*|@";" @|authors|@";" @date}}
               @:{@W{@title*,@\\}
                  @W{@authors,@\\}
                  @W{@where, @|date|.}})
           @:{@(V:"• " @:{\vspace{-7pt}@"\n"})@;
              @|authors|@\\
              @where}))))

(define (tex-prefix)
  @T:{\documentclass[11pt, letterpaper]{awesome-cv}
      \geometry{left=1.4cm, top=.8cm, right=1.4cm, bottom=1.8cm, footskip=.5cm}
      \fontdir[fonts/]
      \setmonofont{Consolas}
      \colorlet{awesome}{awesome-red}
      \setbool{acvSectionColorHighlight}{true}
      \renewcommand{\acvHeaderSocialSep}{\quad—\bullet—\quad}
      \name{@(regexp-replace #rx" ([^ ]+$)" name "}{\\1")}
      \title{@title}
      \address{@address}
      \mobile{@phone}
      \phone{@phone2}
      \email{@email}
      \homepage{@(regexp-replace #rx"/$" web "")}
      \github{@github}
      \linkedin{@linkedin}
      \begin{document}
      \makecvheader[C]
      \makecvfooter{}{@|name|~~~·~~~@|title|}{\thepage}
      \vskip 0mm})
(define (tex-suffix)
  @T:{\end{document}})

(main)
