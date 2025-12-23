#!/usr/bin/env racket

#lang at-exp s-exp "lib.rkt"

;; ---->> Meta ----------------------------------------------------------------

(meta: name     "Eli Barzilay"
       title    "Curriculum Vitae"
       email    "eli@barzilay.org"
       web      "barzilay.org"
       phone    "+1-617-372-2483"
       phone2   "+1-617-383-9313"
       address  (V: "Brookline, MA"
                    "19 Winchester St, Apt #610, Brookline, MA 02446")
       github   "elibarzilay"
       linkedin "eli-barzilay")

(define oo #f)

;; ---->> Toplevel ------------------------------------------------------------

(section! #:if M? @:{@|name|: @title}
  @V:[@:{@url[email] • @url[web] • @url{@phone} • @address}
      @*:[@:{Email:   @url[email]}
          @:{Web:     @url[web]}
          @:{Phone:   @:{@url{@phone} / @url{@phone2}}}
          @:{Address: @address}]])

(part! (sec++))

;; ---->> Overview ------------------------------------------------------------

(section! "Overview"
  @:{@W{My work focuses on building and evolving systems where technical
        quality is taken seriously and tradeoffs are handled deliberately.}
     @||
     @V:[@W{Senior/Principal backend developer with 20+ years of experience
            designing and evolving backend systems across startups and large
            organizations. I work on new systems and existing codebases, with
            an emphasis on careful evolution, clear structure, and engineering
            practices that support long-term maintainability.
            @||
            My background in programming languages informs a disciplined
            approach to design and refactoring, where incremental changes are
            used to manage risk while accumulating substantial structural
            improvement over time. In senior roles, I tend to work close to the
            core of systems—APIs, build and deployment pipelines, and developer
            tooling—while influencing both technical direction and engineering
            culture through hands-on work.}
         @W{Senior/Principal backend and infrastructure developer with over 20
            years of experience designing and building backend systems across
            startups and large organizations. I have worked extensively on
            stabilizing and modernizing existing platforms, but I also enjoy
            and seek out opportunities to design and implement new systems from
            the ground up.
            @||
            My academic background in programming languages informs a
            disciplined approach to software design, whether improving an
            existing codebase or starting fresh. In the former case, I work
            incrementally to avoid unnecessary risk, letting old and new code
            exist side by side, shifting usage gradually, and removing old code
            once stability is clear. When a redesign is justified, I start from
            a clean design and re-implement by breaking apart the existing
            system and reusing what already works, resulting in mostly existing
            code reorganized around a better design spine.
            @||
            I see senior and principal roles as having influence beyond
            individual contributions: shaping expectations around code quality,
            review discipline, and ownership, as well as counterbalancing
            feature pressure with technical judgment. I usually do this through
            hands-on work—designing core components, writing code, and
            mentoring engineers through day-to-day collaboration—while
            contributing to an engineering culture that values correctness,
            clarity, and sustainability.}]})

;; ---->> Preferences ---------------------------------------------------------

(part! (sec++))
(section! "Preferences"
  @V:[@W{I do my strongest work when building new systems or introducing
         structure and tooling that helps codebases evolve in a controlled,
         understandable way.
         @||
         I am seeking a remote role, but I value direct collaboration and
         shared context, including regular conversation and occasional
         in-person interaction.}
      @W{I do my strongest work when building new systems or introducing new
         structure and tooling that helps existing codebases evolve cleanly
         over time. This includes both greenfield work and foundational
         improvements that shape how a system grows.
         @||
         I favor languages and tools that support good abstractions and
         encourage clear, disciplined code. In industry work, this has often
         meant JavaScript and TypeScript, which I've found to be friendly to
         large, long-lived systems and to more functional styles of design.
         While I don't yet have industry experience with Rust, I've spent time
         learning and working with it and see it as bringing much of
         TypeScript's spirit into a more system-level setting.
         @||
         I am seeking a remote role, but I place real value on human
         collaboration and shared context. I find that technical work greatly
         benefits from richer communication, including regular calls and
         occasional in-person interaction.}])
(part! (sec--))

;; ---->> Industry Experience -------------------------------------------------

(define sublist
  (F: (λ xs (apply *: #:loose L? xs)) :))

(section*! "Industry Experience" #:sec-dates '([side up])
  @o[#:date "August 2022 – August 2025" #:datespec "2022-08-15::2025-08-13"
     #:title @:{GoDaddy, Principal Software Developer}
     #:md-pfx "D: "]{
    @*:[@L:{Looking for a new startup-style place, I was pulled into a GoDaddy
            group which was a recent acquisition of a startup (Poynt) making for
            a startup-like work environment}
        @S:{Led modernization and migration of a large multi-repository payments
            platform post-acquisition (Poynt), improving stability, reliability,
            and developer productivity}
        @L:{My focus was on migration and modernization of a huge project with
            many repositories, which was mostly maintained in on-premise GHE
            with an outdated Jenkins, to a modern GHEC and using GH Workflows}
        @S:{Built automation pipelines that raised code quality and reduced
            integration errors and service failures}
        @L:{I also served as a "stabilizing force" for a hectic environment.
            The acquisition and code move has led to quality problems, and
            constant outages. I worked towards improving this and help move
            things in the desired direction while improving stability. In
            addition to the work that I did, this also involved developer
            education: both on technical subjects that many find hard (eg, bash
            scripting), and on a more abstract level (eg, proper code ownership)}
        @S:{Delivered core backend APIs, services, and containerized
            deployments on AWS, collaborating across multiple teams and
            organizations within the company}
        @L:{As part of and in addition to the above, there was the usual
            typical backend kind of work: APIs of all kinds (code- and
            build-related GH APIs)@";" services (implementing our own APIs)@";"
            automations (GH workflows, managing deployed nodes, cron jobs,
            document batch processing)@";" docker images (as well as working with
            k8s and AWS images)@";" AWS deployments (migrating deploys from a
            jenkins pipeline pushing to a PM2-managed server nodes to a modern
            image-based deployment tool). The nature of my work was always heavy
            on cross team impact (code quality for several teams) as well as
            cross-org (mainly with the central engineering division)}]}
  @o[#:date "February 2022 – June 2022" #:datespec "2022-02-28::2022-06-17"
     #:title @:{Sundae, Principal Software Developer}
     #:md-pfx "D: "]{
    @*:[@S:{Brought in to stabilize and extend a young codebase}
        @L:{Following a decision to switch to a more greenfield-style work, I
            joined a startup, Sundae, as a Principal Software Developer}
        @S:{Designed and delivered new APIs and special projects, strengthening
            system robustness, including a design & implementation of an
            authentication scheme}
        @L:{During this time my work was mainly focused on improving the very
            young codebase, as well as implementing new APIs.  The former was
            mainly making the code robust by improving the TypeScript type
            declarations, and revamping the overall design of the system. The
            latter included design and implementation work, including a
            JWT-based authentication scheme. Also included were various "special
            projects"}
        @S:{Role ended when the company scaled back engineering due to a
            faltering housing market}
        @L:{I greatly enjoyed my time at Sundae which went very well, affirming
            my decision to leave Microsoft.  However, the faltering housing
            market in June 2022 made the company drop most of its software
            engineering effort}]}
  @(set! oo (λ (date D title #:short [short title] . xs)
              (o #:date date #:datespec D #:title (F: title @:{• @title})
                 #:dname @:{Microsoft: @title} #:short short
                 #:md-pfx "D: "
                 xs)))
  @o[#:date "December 2015 – January 2022" #:datespec "2015-12-01::2022-01-31"
     #:title @:{Microsoft, Senior Software Developer}
     #:md-pfx "D: "]{
    @L:{@M:["\n"]@;
        In 2014 I decided to go back to industry work.  After a few short projects
        I joined Microsoft as a Senior Software Developer in late 2015, and worked
        in three different products teams.
        @"\n"}@;
    @sublist[
      @oo["October 2019 – January 2022" "2019-10-01::2022-01-31"
          @url{TypeScript || www.typescriptlang.org/}]{
        @*:[@S:{Maintained and extended core TypeScript tooling: language server,
                VS Code integration, build pipeline, and DefinitelyTyped ecosystem}
            @L:{Day-to-day work on bugs in TS and related systems (language server,
                vscode, builds, DefinitelyTyped maintenance)}
            @S:{Built a tracing profiler that improved performance visibility and
                debugging for large projects}
            @L:{Implemented TypeScript's tracing profiler (with Andrew Casey), which
                helps people debug compilation performance issues}
            @S:{Developed the DefinitelyTyped mergebot, streamlining PR workflows
                for a repo with ~10k type packages and the wider TS community}
            @L:{@W{Implementation of DefinitelyTyped's
                   @url{mergebot || github.com/DefinitelyTyped/dt-mergebot}: a
                   github bot that enables self-maintenance, managing the huge
                   load of incoming PRs (hundreds per week)}}
            @S:{Drove conversion of the TypeScript compiler to ES modules,
                aligning it with modern JavaScript standards}
            @L:{One of my major projects was the conversion of the TypeScript compiler
                to ES modules, aligning with modern JS standards. This was completed
                after I left the group}]}
      @oo["March 2018 – October 2019" "2018-03-01::2019-09-30"
          "Python group" #:short "Azure Python"]{
        @*:[@S:{Engineered backend infrastructure for Microsoft's Jupyter notebooks
                offering}
            @L:{The main Microsoft offering for running Jupyter notebooks (iPython)
                online}
            @S:{Designed server architecture, containerized deployments, and network
                topologies to deliver scalable and responsive execution environments}
            @L:{Focus on infrastructure work: server maintenance, docker images
                infrastructure (library collection, image builds, publish, internal
                distribution), and network design (including Kubernetes compute
                backends on Azure)}]}
      @oo["Mid 2016 – March 2018" "2016-07-01::2018-02-28"
          @url{MMLSpark || github.com/microsoft/SynapseML/}]{
        @*:[@S:{Designed and built core infrastructure for MMLSpark (now SynapseML),
                an open-source distributed machine learning library for Azure ML}
            @L:{Open source project, core library for the Vienna project (Azure ML)}
            @L:{Code development from the project's very beginning}
            @L:{Completely designed and implemented the project's infrastructure,
                including implementation of many VSTS features that are unavailable
                for a public project (e.g., maintaining a VSTS build, publish
                artifacts and documentation, PR builds, code style and clean git
                history)}
            @S:{Managed releases and supervised PRs across internal and external
                contributors, ensuring code quality and community adoption}
            @L:{Also served as a PR coordinator and release manager for the project,
                including all public artifacts (jars, spark packages, python
                packages, docker images, etc)}
            @L:{Helped the rest of the team to get more comfortable with functional
                programming (in Scala), replace imperative idioms by functional
                ones, and learn about more advanced functional programming patterns}
            @L:{The project was later renamed “SynapseML”}]}
      @oo["December 2015 – March 2018" "2015-12-01::2018-03-01"
          @:{Azure Machine Learning Studio@L:{ (Boston)}} #:short "AzureML Studio"]{
        @*:[@S:{Built scalable frontend visualizations, including an infinite
                grid scroller with dynamic data fetching}
            @L:{Front-end visualizations, mainly table view of infinite matrix,
                backed by an on-demand service}
            @S:{Delivered TypeScript-based UI components for ML workflows}
            @L:{Mostly done in TypeScript@";" also involved Visual Studio, TFS, and
                micro-services}]}]}
  @o[#:if L? #:date #f
     #:title @:{Used numerous technologies (abridged list)}]{
    @*:[@:{Platforms: Linux, Windows, Hyper-V, MSFT Azure, AWS,
           GitHub & GitLab (incl. API, Automation, DevOps, etc), GraphQL,
           Docker}
        @:{Build/code management: Git, Bash, Docker}
        @:{Languages: TypeScript, JavaScript, Python, Bash, Powershell, Scala,
           Java, R, ARM Templates, Lisp(s)}]})

;; ---->> Projects ------------------------------------------------------------

(define (quick-entry date D title0 #:short [sh #f] #:if [bool #t] . xs0)
  (define title (->string title0))
  (define xs (or (apply : xs0) '()))
  (o #:if bool #:date date #:datespec D
     #:title title
     #:md-pfx @L:{D: }
     #:md-title-sfx (V: " (D):" "\n")
     #:tex-title-sfx @:{:}
     #:tex-nobr #t
     #:dname title #:short sh
     xs))
(set! oo quick-entry)

(section*! "Notable Projects" #:sec-dates '([side up])
  #:pfx @:{A selection of projects, including both personal work and projects
           where I played a major role.}
  @oo["September 2025 – Present" "2025-09-01::NOW"
      @url{API Server || github.com/elibarzilay/api-server}]{
    @V:[@:{A feature-rich Hono-based API server}
        @:{A feature-rich Hono-based API server that I built as a personal
           project to explore and for personal use. Includes a lot of built-in
           functionality, like custom+oauth authentication, users and groups,
           efficient Sqlite-based key-value storage, and much more.}]}
  @oo["May 2023 – March 2024" "2023-05-01::2024-03-31"
      @url{El Turco || elturco.diemutstrebe.com/}]{
    @V:[@:{An interactive AI-driven art installation}
        @:{Bootstrapped El Turco: an interactive AI project by @url{Diemut||https://www.diemutstrebe.com/}.
           This is a big project that includes: ChatGPT API for conversations,
           Unreal Engine for animation, Azure text to speech, and its core is a
           Racket program that orchestrates it all. My part was in the overall
           design and most of the implementation of the core orchestrator code.
           I also built the project's website.}]}
  @oo["June 2020 – September 2022" "2020-06-01::2022-09-30"
      @url{DT Mergebot || github.com/DefinitelyTyped/dt-mergebot}]{
    @V:[@:{The DefinitelyTyped mergebot}
        @:{Implemented most of the DefinitelyTyped mergebot, which enables
           community maintenance of ~10k type packages.}]}
  @oo["February 2020 – Present" "2020-02-27::NOW"
      @url{Music Player || github.com/elibarzilay/player}]{
    @V:[@:{A web-based music player built for personal use}
        @:{A web-based music player that I implemented in JavaScript/HTML/CSS;
           built to learn, for fun, and because I couldn't find a music player
           that does quite everything I wanted. Includes features like
           folder-based navigation, intuitive playlist management, and some
           nice visualizations.}]}
  @oo["January 2016 – April 2018" "2017-01-01::2018-04-30"
      @url{SynapseML || github.com/microsoft/SynapseML}]{
    @V:[@:{An open-source distributed machine learning library}
        @:{An open-source library that simplifies building massively scalable
           machine learning pipelines on Apache Spark. I led the project's
           developer infrastructure from its inception, including migration from
           Azure DevOps to GitHub, build and release automation, clean repository
           organization, and contribution workflows, fostering external
           contributions and long-term adoption.}]}
  @oo["2016 – 2023" "2016-01-01::2023-10-31"
      "Docker DevEnv"]{
    @V:[@:{A self-contained, Docker-based development environment implemented
           as a single Bash script}
        @:{A self-contained, Docker-based development environment implemented
           as a single Bash script.  The script builds and launches a complete
           developer workspace — editors, browsers, and tooling — independent
           of the host machine, while mirroring the user's local environment
           (home directory, username, UID/GID). The system predates modern
           dev-container specifications; it emphasizes portability via minimal
           dependencies. Uses low-level Docker functionality to build and use
           the image.}]})

;; ---->> Academic ------------------------------------------------------------

(part! (L: (header "Academic") (sec++)))

(define research-interests
  @splice{Programming Languages (design and implementation),
          Formal Languages, Meta Programming, Reflection.})
(section! "Research Interests" #:if L?
  research-interests)

;; ---->> Education -----------------------------------------------------------

(define (edu date D title short where advisor subject)
  (let ([advisor (and advisor @:{Advisor: @advisor,})]
        [subject (and subject (L: @:{Subject: @(it subject),}))]
        [longloc @:{@V:[(cadr where) (caddr where)].}]
        [title (->string title)])
    (o #:date date #:title (list title @ST:{@";" @(cadr where)})
       #:dname @:{@title, @(car where)}
       #:datespec D #:short short #:loc (LT: (car where))
       #:md-pfx "D: " #:md-title-sfx @S:{,@!TEXT:{ \}}
       (V: (M: (cadr where)) @*:[advisor subject longloc]))))
(define phd-in (V: "PhD," "Ph.D. in"))
(define msc-in (V: "MSc," "M.Sc. in"))
(define bsc-in (V: "BSc," "B.Sc. in"))
(define cornell
  (list "Cornell"
        "Cornell University"
        "Cornell University, New York, NY, USA"))
(define bgu
  (list "BGU, Israel"
        "Ben-Gurion Univ, Israel"
        "Ben-Gurion University of the Negev, Be'er-Sheva, Israel"))

(section*! "Education" #:sec-dates '([side down])
  (edu "1997–2003" "1997-01-01::2003-07-31"
       @:{@phd-in Computer Science} "CS PhD @ Cornell"
       cornell "Prof. Robert Constable" "Implementing Reflection in Nuprl")
  (edu "1994–1996" "1994-09-01::1996-12-31"
       @:{@msc-in Computer Science (summa cum laude)} "CS MSc @ BGU"
       bgu "Prof. Mira Balaban" "Framework for Creative Editing")
  (edu "1991–1993" "1991-09-01::1994-06-30"
       @:{@bsc-in Math & Computer Science (cum laude)} "CS+Math BSc @ BGU"
       bgu #f #f))

;; ---->> Academic Experience -------------------------------------------------

(section*! "Academic Experience" #:if S?
  @:{Research Interests: @research-interests}
  @(set! oo (λ (date title . xs)
              (o #:date date #:title title
                 #:md-title-sfx " (D):" #:tex-title-sfx ":" #:tex-nobr #t
                 xs)))
  @oo["1994–2014" "Researcher"]{
    Programming languages, theorem proving, and computer
    music@";" core member of the PLT Scheme/Racket project}
  @oo["1994–present" "Lecturer"]{
    Programming Languages@";"
    currently adjunct faculty at Northeastern})

;; ---->> Teaching Experience -------------------------------------------------

(section*! "Teaching Experience" #:sec-dates '([side down]) #:if L?
  @(set! oo (curry o #:md-pfx "D: "))
  @oo[#:date "2004–Present" #:datespec "2004-01-01::NOW" #:loc "Northeastern"
      #:title @:{Lecturer, Northeastern University}
      #:dname "Programming Languages, Northeastern"
      #:short "Programming Languages"]{
    @||
    @*:[@:{CSU4400/CSG5400 (@url{pl.barzilay.org/})@\\
           Programming Languages (combined master and undergraduate).
           @||
           Designed and taught a long-running Programming Languages course,
           rebuilt from the Brown University textbook @it{Programming Languages:
           Application and Interpretation} and substantially extended with
           original material. Added in-depth coverage of lambda calculus,
           macros, type systems, continuations, domain-specific languages, and
           language semantics.
           @||
           Developed extensive course infrastructure and tooling, including a
           Racket-based @url{assignment submission system || https://github.com/racket/handin},
           grading framework, exam client/server system,
           @url{in-class quizzes || https://github.com/elibarzilay/plq} and more.
           These systems evolved continuously over years of teaching, with
           regular re-implementations as course needs changed.
           @||
           Created numerous custom S-expression-based teaching languages with
           distinct semantics (lazy evaluation, dynamic scoping, implicit
           currying, ML-style binding, compiled lambda calculus). The primary
           course language is a Typed Racket variant extended with disjoint sum
           types.
           @||
           Course materials have been adopted by instructors internationally,
           including use in Canada, Korea, and Israel.}
        @:{CSU213, Fundamentals of Computer Science II, Spring 2006.}]}
  @oo[#:date "1997–2000" #:datespec "1997-01-01::2000-05-31" #:loc "Cornell"
      #:title @:{Teaching Assistant, Cornell University}
      #:dname "Teaching Assistant, Cornell" #:short "TA @ Cornell"]{
    @||
    @*:[@:{CS212, Structures and Interpretation of Computer Programs.
           @||
           Conducted recitation sessions; participated in developing homework
           materials and exams; and graded coursework during Fall 1997 and
           Spring 2000. From 1998 to 2000, designed, implemented, and
           maintained Swindle, a rich language built on PLT Scheme that
           replaced a previously used Dylan-like environment. Swindle provides
           a CLOS-like object system and additional features required for the
           course material.
           @||
           The language was used in CS212 until 2000, when the course was
           revised and switched to SML. I remained involved with course teams
           for several years afterward, contributing to materials and giving
           occasional guest lectures.
           @||
           Swindle was adopted by a number of additional universities,
           including Dartmouth, Vassar, and Duke, and remains part of the
           Racket distribution.}]}
  @oo[#:date "1994–1996" #:datespec "1994-09-01::1996-12-31" #:loc "BGU, Israel"
      #:title @:{Teaching Assistant, Ben-Gurion University}
      #:dname "Teaching Assistant, BGU" #:short "TA @ BGU"]{
    @||
    @*:[@:{Automata and Formal Languages}
        @:{Structures and Interpretation of Computer Programs}
        @:{Advanced Programming Languages}]})

;; ---->> Research Experience -------------------------------------------------

(section*! "Research Experience" #:sec-dates '([side down]) #:if L?
  ;; #:pfx @S:{Research Interests: @research-interests
  ;;           @||}
  @(set! oo (curry o #:md-pfx "D: " #:tex-nobr #t))
  @oo[#:date "2003–2014" #:datespec "2003-09-01::2014-07-01" #:loc "Northeastern"
      #:title @:{Researcher, Programming Research Laboratory,}
      #:dname "Researcher, Programming Research Laboratory, Northeastern"
      #:short "PRL, Northeastern"]{
    Northeastern University, working with Prof. Matthias Felleisen
    @||
    Member of the small core development team behind Racket (formerly PLT
    Scheme), contributing to its growth into a large-scale “language
    greenhouse.” Designed and implemented several core language features,
    including:
    @||
    @*:[@:{A libffi-based dynamic foreign function interface, replacing earlier
           ad-hoc mechanisms and enabling portable GUIs, database interfaces,
           OpenGL bindings, and other foreign-library integrations.}
        @:{Lazy Racket, a language variant with the same syntax as Racket but
           different execution semantics, which influenced later languages
           such as Typed Racket.}
        @:{A concrete syntax implementation for text-rich code, providing
           functionality similar to here-docs and string interpolation while
           remaining fully integrated with S-expressions. This syntax forms the
           basis for a family of domain-specific languages used for Racket's
           documentation system, textbooks, and articles.}]
    @||
    Built and maintained much of the project's infrastructure, including the
    build system, release process, and web content generation, and partially
    served as system administrator. This work combined substantial technical
    engineering with community-scale maintenance, release coordination, and
    long-term support.
    @||
    Contributed numerous additional libraries within the Racket and PLT
    ecosystems, including generators and generic functions, which would be
    considered major language features in many other systems.}
  @oo[#:date "1997–2003" #:datespec "1997-01-01::2003-07-31" #:loc "Cornell"
      #:title @:{Graduate Research Assistant in the Nuprl group,}
      #:dname "Graduate Research Assistant in the Nuprl group, Cornell"
      #:short "Nuprl, Cornell"]{
    directed by Prof. Robert Constable, Cornell University@;
    @L:{@"\n"
        Conducted research on implementing reflection for the Nuprl theorem
        prover, covering both theoretical foundations and practical system
        design. Made additional contributions to Nuprl and participated in the
        development of the MetaPRL theorem prover.}}
  @oo[#:date "1994–1996" #:datespec "1994-09-01::1996-12-31" #:loc "BGU, Israel"
      #:title @:{Research Assistant, Ben-Gurion University}
      #:short "Computer Music @ BGU"]{
    @L:{@||
        Developed a Common Lisp–based system for creative music editing as part
        of my Master's thesis. The work focused on a musical composition tool
        that functioned as a visual functional language and was shown to
        generalize to other forms of structured creative editing, such as
        graphic editing. The implementation combined programming language
        techniques with operational graph editing to support reusable,
        user-defined abstractions.}})

;; ---->> Dissertation --------------------------------------------------------

(section! "Ph.D. Dissertation" #:if L?
  @o[#:date "1997–2003" #:loc "Cornell"
     #:title @it{Implementing Reflection in Nuprl}]{
    @||
    Developed a strong reflection model for the Nuprl theorem prover,
    contrasting with traditional weak-reflection approaches based on separate
    encodings of the reflected layer. Reflection is achieved by exposing
    internal system-level functionality directly to the user level, borrowing
    techniques from Lisp and Scheme to make the reflected and base systems
    identical.
    @||
    The work adopts a higher-order abstract syntax (HOAS) representation for
    syntax with bindings and defines operations over these representations.
    This required specifying semantics for quoted syntax with bindings and
    introducing new logical rules in Nuprl. The results influenced later
    reflection work in MetaPRL and, years later, inspired textual language
    mechanisms in Racket, most notably Scribble.})

;; ---->> Publications --------------------------------------------------------

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
    (o #:date date #:datespec D #:dname title #:short short #:tex-nobr S?
       #:md-title-sfx @:{,@!TEXT:{ \}}
       #:title (W title*)
       (F: (V: @:{@W{@|authors|@";" @date}}
               @:{@W{@authors,@\\}
                  @W{@where, @|date|.}})
           @:{@(V:"• " @:{\vspace{-7pt}@"\n"})@;
              @|authors|@\\
              @where}))))
(section*! "Publications" #:sec-dates '([side down]) #:if L?
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
  (pub "MetaPRL: A Modular Logical Environment"
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

;; ---->> Earlier Experience --------------------------------------------------

(set! oo quick-entry)

(define earlier-experience
  (list
   @oo["2014–2015" "2014-05-01::2015-10-01"
       "Gefen-Dekel"]{
     @V:[@:{desktop app (Chromium+CEF);
            spearhead company-wide React adoption}
         @:{Helped embedding Chromium (via CEF) into Dalet's desktop application,
            later followed up with learning about how React can be used in a
            browser-based editor, and eventually ran a two day tutorial on React,
            to help transition the company codebase.}]}
   @oo["2003–2014" "2003-09-01::2014-07-01"
       "Systems / Infrastructure" #:short "Sys/Infra @ PLT"]{
     @V:[@:{part of the Racket (PLT) group}
         @:{As part of the PLT group behind Racket, I was in charge of the framework of
            servers, builds, releases, code repository and all other services that are
            required to run such a project.}]}
   @oo["1995–1997" "1995-01-01::1996-12-31"
       "Gefen-Dekel"]{
     @V:[@:{multimedia applications for radio stations}
         @:{Was among the first programmers of Gefen-Dekel, the software development
            branch of Dalet Digital Media Systems. Developed multimedia applications
            for Windows, aimed mainly at radio station software and advertisement
            management.}]}
   @oo[#:if S? "1993–1994" "1993-08-01::1994-12-31" "Other early roles"]{
     @:{Prolog-based staff scheduling system;
        Unix system administration}}
   @oo[#:if L? "1994" "1994-05-01::1994-12-31"
       "Shibutzit"]{
     @V:[@:{Prolog-based application for shift allocations}
         @:{Summer work in Shibutzit, a small company, developing a Prolog-based
            application for Windows for allocation of human resource shifts.}]}
   @oo[#:if L? "1993" "1993-08-01::1994-06-30"
       "System Administrator" #:short "Sysadmin @ BGU CS"]{
     @V:[@:{various Unix systems}
         @:{During the last year of my undergraduate studies, I worked as part of the
            system administration group for the Computer Science department in Ben-Gurion
            University.}]}))

(apply section*! "Earlier Experience" #:sec-dates '([side up]) #:if L?
  earlier-experience)

;; ---->> Earlier Projects ----------------------------------------------------

(define earlier-projects
  (list
   @oo["2008–2010" "2008-01-01::2010-12-31"
       "The Scribble Reader" #:short "Scribble"]{
     @V:[@:{syntax for text-rich code, leading to Racket's documentation system}
         @:{Designed a concrete syntactic extension to Racket that combines the
            convenience of “here-documents” and string interpolation, while
            maintaining the benefits of S-expressions.  The idea behind the syntax
            is based on the Nuprl quotations, which result in a language that is
            easier to use than quasi-quotations.  The syntax was named “Scribble”,
            and later evolved into Racket's documentation system, making one of
            the earlier cases of using Racket as a real language framework, where
            a single program can be written multiple languages that are different
            even at the concrete syntax level.}]}
   @oo["2005–2006" "2005-01-01::2006-12-31"
       "Lazy Racket"]{
     @V:[@:{implemented the lazy dialect of Racket}
         @:{Implemented the Lazy Racket language.  It is a language that is
            identical to Racket except for its lazy execution semantics.  It was
            developed for the PL course, but the result is a powerful ability to
            mix lazy and strict code in a single language, using Racket's module
            system.  This is in addition to other languages developed for the
            course.}]}
   @oo["2004–2007" "2004-01-01::2007-12-31"
       "Handin Server+Client"]{
     @V:[@:{along with other teaching-related software}
         @:{Extended the Racket Handin server and client (originally developed by
            Matthew Flatt) considerably, making it usable in courses with widely
            varying demands.  In addition, designed a completely automated system
            to deal with running the PL course, including website publishing and
            on-line content (class notes, homework handins, etc.), electronic
            grading, secure grades publishing with statistics and graphs,
            class-participation tool, a kiosk-mode client for taking exams, and
            more.}]}
   @oo["2003–2004" "2003-01-01::2004-12-31"
       "Foreign Function Interface for Racket" #:short "Racket FFI"]{
     @V:[@:{designed and implemented}
         @:{Designed and implemented Racket's foreign function interface for
            Racket, replacing a more traditional C-based approach where Racket
            code replaces the role of C.}]}
   @oo[#:if L? "2003–2014" "2003-09-01::2014-07-01"
        "PLT / Racket Infrastructure" #:short "Racket Infrastructure"]{
     @V:[@:{development & maintenance}
         @:{Designed and implemented the build process for making nightly builds
            of Racket as well as official distributions, revised the web-content
            framework using the new Scribble-based syntax as a markup language,
            was in charge of moving the Racket codebase from CVS to Subversion
            first, and to git later.}]}
   @oo[#:if L? "1999–2003" "1999-01-01::2003-07-31"
       @V:[@:{Reflection for the Nuprl theorem prover}
           @:{Reflection for Nuprl}]]{
     @L:{Implemented reflection for the Nuprl theorem prover.}}
   @oo[#:if L? "2001" "2001-01-01::2001-12-01"
       @V:[@:{Web-server implementation in Scheme, with dynamic code execution}
           @:{Web-server}]]{
     @L:{Wrote a complete web-server in Scheme, with the purpose of being very
         flexible and for making an on-line picture collection.}}
   @oo[#:if L? "1999" "1999-01-01::1999-12-31"
       "Picture collection management" #:short "Picture collection"]{
     @:{Designed and implemented a set of tools for managing a picture
        collection, as part of a photography project (for a minor in art at
        Cornell).}}
   @oo[#:if L? "1999" "1999-01-01::1999-12-31"
       "HTML generation"]{
     @V:[@:{(precursor to Scribble)}
         @:{Wrote a system to easily generate HTML code in an easily extensible
            way, relying on such features as self-quoting code and keyword
            argument.  This system was used for several years by a few academic
            and commercial sites.  Years later it evolved into the Scribble
            syntax.}]}
   @oo["1998" "1998-01-01::1998-12-31"
       "Swindle"]{
     @V:[@:{MOP-based object system, used in several universities}
         @:{Implemented Swindle: an extension language on top of PLT-Scheme/Racket
            for CS212 in the Cornell CS department.  Swindle was in active use at
            several universities for many years, and still is in use in some.}]}
   @oo["1997" "1996-09-01::1997-12-31"
       "Emacs calculator"]{
     @V:[@:{part of the standard Emacs distribution}
         @:{Implemented an interactive calculator package which is now a standard
            part of Emacs.}]}
   @oo[#:if L? "1995–1996" "1995-01-01::1996-12-31"
       "Booms"]{
     @V:[@:{a visual language for music composition}
         @:{A visual language environment for creative editing, mainly intended
            for music composition.  Implemented in Allegro Common Lisp for
            Windows.}]}
   @oo[#:if L? "1994–1996" "1995-01-01::1996-12-31"
       "Multeam"]{
     @V:[@:{a Usenet-like system for media content}
         @:{A Usenet-like system for managing media content which was developed at
            Gefen-Dekel Technologies (part of Dalet Digital Media Systems).  The
            original project was discontinued but served as a basis for the “Dalet
            Web Publisher” which was eventually integrated in Dalet's main
            application—DaletPlus, and later Dalet Galaxy.}]}))

(apply section*! "Notable Earlier Projects" #:sec-dates '([side up]) #:if L?
  earlier-projects)

;; ---->> Earlier Experience & Projects ---------------------------------------
;; A combined version of both of the above

(apply section*! "Earlier Experience & Projects" #:sec-dates '([side up]) #:if S?
  (let* ([xs (append earlier-experience earlier-projects)]
         [xs (filter is-val? xs)])
    (sort xs (λ(a b) (string>? (prop-ref a 'datespec) (prop-ref b 'datespec))))))

;; ---->> Personal ------------------------------------------------------------

(section*! "Personal" #:itemize (F: *: cvitemize:) #:if L?
  @:{Citizen of Israel, permanent US residence since April 2006.}
  @:{Photography: A minor in Art at Cornell, mostly digital photography.}
  @:{Music: I love electronic music and experiment with making some as well as
     hacking on my own music player (implemented as a web app).})
