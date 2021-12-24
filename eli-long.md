# Eli Barzilay: Curriculum Vitae

* Email:   <eli@barzilay.org>

* Web:     <https://barzilay.org/>

* Phone:   +1-617-372-2483

* Address: 19 Winchester St, Apt #610, Brookline, MA  02446


## Overview

* Senior Software Developer at Microsoft since December 2015
  - Azure Machine Learning group in Boston
  - Python Group
  - TypeScript Group

* Professional Programmer since 1993
  - Many languages, focus on functional programming
  - Many environments, focus on Linux
  - Preferable focus on full stack work, experience at all levels

* Education:
  B.Sc., M.Sc., Ph.D. in Computer Science, \
  1991–2005

* Research:
  Computer Music, Theorem Proving, Programming Languages, \
  1994–2014

* Teaching:
  Programming Languages, Programming & Fundamentals, \
  1994–Present


## Recent Microsoft Experience

* October 2019 — Present: TypeScript
  - Day-to-day work on bugs in TS and related systems (language server,
    vscode, builds, DefinitelyTyped maintenance)
  - Implementation of most of the DefinitelyTyped mergebot
    (<https://github.com/DefinitelyTyped/dt-mergebot>): a github bot
    that enables self-maintenance, reducing the load of handling the
    huge constant stream of incoming PRs (hundreds per week)
  - Conversion of the TypeScript code base to JS modules (ongoing)

* March 2018 — October 2019: Python group
  - The main Microsoft offering for running online iPython (Jupyter)
    notebooks
  - Focus on infrastructure work: server maintenance, docker images
    infrastructure (library collection, image builds, publish, internal
    distribution), and network design (including Kubernetes compute
    backends on Azure)

* Mid 2016 — March 2018: MMLSpark \
  (<https://github.com/microsoft/SynapseML>)
  - Open source project, core library for the Vienna project (Azure ML)
  - Code development from the project's beginning
  - Completely designed and implemented the project's infrastructure,
    including implementation of many VSTS features that are unavailable
    for a public project (e.g., maintaining a VSTS build, publish
    artifacts and documentation, PR builds, code style and clean git
    history)
  - Also served as a PR coordinator and release manager for the project,
    including all public artifacts (jars, spark packages, python
    packages, docker images, etc)
  - Helped the rest of the team to get more comfortable with functional
    programming (in Scala), replace imperative idioms by functional
    ones, and learn about more advanced functional programming patterns
  - The project was later renamed "SynapseML"

* December 2015 — Mid 2016: Azure Machine Learning Studio (Boston),
  - Front-end visualizations, mainly table view of unlimited-size
    matrix, backed by an on-demand service
  - Mostly done in TypeScript; also involved Visual Studio and TFS

* Used numerous technologies (abridged list):
  - Platforms: Linux, Windows, Hyper-V, Ubuntu, Azure VMs and other
    services, Github, Azure DevOps, Docker
  - Build/code management: Git, Bash (install+build scripts), Docker
  - Languages: TypeScript, JavaScript, Python, Bash, Powershell, Scala,
    Java, R, ARM Templates


## Academic

### Research Interests

Programming Languages (design and implementation), Meta Programming,
Reflection.


### Education

* 1997–2003 Ph.D. in Computer Science,
  - Advisor: Prof. Robert Constable,
  - Subject: *"Implementing Reflection in Nuprl"*,
  - Cornell University, New York, NY, USA.

* 1994–1996 M.Sc. in Computer Science (summa cum laude),
  - Advisor: Prof. Mira Balaban,
  - Subject: *"Framework for Creative Editing"*,
  - Ben-Gurion University of the Negev, Be'er-Sheva, Israel.

* 1991–1993 B.Sc. in Math & Computer Science (cum laude),
  - Ben-Gurion University of the Negev, Be'er-Sheva, Israel.


### Teaching Experience

* 2004–Present Lecturer, Northeastern University

  - CSU4400/CSG5400 (<https://pl.barzilay.org/>) \
    Programming Languages (combined master and undergrad levels).

    Reconstructed the course materials based on a (then new) textbook
    from Brown University, *"Programming Languages: Application and
    Interpretation"*, by Shriram Krishnamurthi.  Eventually, the
    material had evolved to much more than the book, adding completely
    new and detailed chapters on some advanced topics in programming
    languages that are either not skimmed or not included in the book.
    Examples are lambda calculus, macros, type systems, domain-specific
    languages, and continuations.  This was taught every semester from
    2004 until the present.  (Combined with the graduate course since
    the fall semester of 2008.)

    In addition, I designed a (Racket-based) paperless system for
    homework submission, grading, and for exams.  While some of these
    systems are no longer used, I keep implementing new ones as needed
    (for example, the exam server/client was replaced by a WPA in-class
    quiz, <https://plq.barzilay.org/>) — up to 100 commits most years.

    To support the course material, I designed a large number of
    S-expression-based languages with wildly different semantics: a lazy
    language, an implicitly-curried language, a fast lambda-calculus
    language (compiled to Racket rather than a rewrite engine
    implementation), a dynamically-scoped language, a language with
    ML-like scope for top-level definitions and more.  This is in
    addition to the main language, which is an variant of Typed Racket,
    extended with disjoint sum types.

    My materials were taken by a number of people, and they are
    currently being taught in Canada, Korea, and in Isreal.

  - CSU213, Fundamentals of Computer Science II, Spring 2006.

* 1997–2000 Teaching Assistant, Cornell University

  - CS212, Structures and Interpretation of Computer Programs.

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

    Swindle was used for many years in a number of additional colleges
    and universities, including Dartmouth, Vassar and Duke.  In some, it
    is being used to this day.  (It is part of the Racket distribution.)

* 1994–1996 Teaching Assistant, Ben-Gurion University

  - Automata and Formal Languages
  - Structures and Interpretation of Computer Programs
  - Advanced Programming Languages

  Conducted recitation sessions, graded exams, helped in extending
  course contents and develop new material.


### Research Experience

* 2003–2014 Researcher, Programming Research Laboratory,
  Northeastern University, working with Prof. Matthias Felleisen

  During this time, I was part of the small core development team behind
  Racket (formerly PLT Scheme) and helped grow it into the "language
  greenhouse" that it is today.  Specifically, I have designed and
  implemented a number of important core features in the language:

  - A libffi-based dynamic foreign interface, which allows interfacing
    foreign libraries from Racket.  This interface has replaced the
    previous ad-hoc system, and enabled rapid development in many
    important areas such as a platform-independent GUI system (a
    re-implementation of wxWindows in Racket), a portable database
    interface, an OpenGL library, and many more.

  - A Lazy Racket language that is similar in its syntax to Racket, but
    different in its execution semantics.  This implementation paved the
    way to a number of similar important languages, such as Typed
    Racket.

  - A concrete syntax that makes it possible to deal with text-rich
    code, in a way that provides functionality similar to here-docs and
    string interpolation, yet is more integrated in the language than
    common approaches.  This syntax forms the bases for a family of
    domain specific languages that are used for Racket's documentation
    system, textbooks, articles, and more.

  - I have created the group's infrastructure and everything that is
    involved, and partially served as a system administrator to maintain
    it.  This included substantial technical pieces like making up the
    Racket build process and web page generation as well as social
    aspects like setting up mailing lists and other community building
    efforts.

  - Numerous other projects within the Racket community and the PLT
    research group.  Some of these would be considered as major language
    features in other languages, but in Racket they are just libraries.
    For example, I have implemented Racket's generators, and its system
    of generic functions.

* 1997–2003 Graduate Research Assistant in the Nuprl group,
  directed by Prof. Robert Constable, Cornell University

  The focus of this research was the implementation of a reflection
  system for the Nuprl theorem prover, covering both the practical and
  the theoretical aspects.  Made additional contributions to Nuprl, and
  participated in the development of the MetaPRL theorem prover
  (designed and implemented by Jason Hickey).

* 1994–1996 Research Assistant, Ben-Gurion University

  Developed a Common Lisp based system for creative music editing as
  part of my Masters thesis.  This research was centered around an
  implementation of a musical composition tool, resulting in a system
  that is essentially a visual functional language.  The system was
  shown to be useful in other forms of structured creative editing such
  as graphic editing.  The implementation was heavily influenced by
  programming languages work combined with operational graph editing,
  which are used to form user-defined reusable abstractions.


### Ph.D. Dissertation

*"Implementing Reflection in Nuprl"*

Nuprl is a theorem prover, a system that materializes the relation
between logic and programming languages.  The logical aspects of such an
environment naturally pulls any implementation of reflection towards a
reimplementation of the logical system within itself.  This approach is
adequate for achieving theoretical results (e.g., Godel numbers), but
for actual work where reflection is needed, it is impractical.

My research work on implementing reflection for Nuprl borrowed heavily
from common practices in programming languages, and most specifically
from Lisp and Scheme: achieve reflection through exposure of internal
system-level functionality to the user-level.  By design, this is a
*strong* reflection principle, meaning that the reflected system is
inherently identical to the base system.  This work diverged from the
more common logical tradition of a weak reflection, where a separate
description of the reflected layer is provided, requiring proof that the
resulting reflective layer is equivalent to the source layer.

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
which is in heavy use today (Scribble).


### Publications

* *"MMLSpark: Unifying Machine Learning Ecosystems at Massive Scales"*, \
  Mark Hamilton et al, \
  arXiv 1810.08744, 2018.

* *"A Programmable Programming Language"*, \
  Matthias Felleisen et al, \
  Communications of the ACM (CACM), 2018.

* *"The Racket Manifesto"*, \
  Matthias Felleisen et al, \
  SNAPL: The Inaugural Summit on Advances in Programming Languages, 2015.

* *"Keeping it Clean with Syntax Parameters"*, \
  Eli Barzilay, Ryan Culpepper, Matthew Flatt, \
  Workshop on Scheme and Functional Programming, 2011.

* *"From Stack Traces to Lazy Rewriting Sequences"*, \
  Stephen Chang, Eli Barzilay, John Clements, Matthias Felleisen, \
  Implementation and Application of Functional Languages, 2011.

* *"The Scribble Reader"*, \
  Eli Barzilay, \
  Workshop on Scheme and Functional Programming, 2009.

* *"Keyword and Optional Arguments in PLT Scheme"*, \
  Matthew Flatt, Eli Barzilay, \
  Workshop on Scheme and Functional Programming, 2009.

* *"Scribble: Closing the Book on Ad Hoc Documentation Tools"*, \
  Matthew Flatt, Eli Barzilay, Robby Findler, \
  International Conference on Functional Programming, 2009.

* *"A Self-Hosting Evaluator using HOAS"*, \
  Eli Barzilay, \
  Workshop on Scheme and Functional Programming, 2006.

* *"Implementing Direct Reflection in Nuprl"*, \
  Eli Barzilay, \
  Ph.D. Thesis, 2006.

* *"Laziness Without All the Hard Work: Combining Lazy and Strict
  Languages for Teaching"* \
  Eli Barzilay, John Clements, \
  Functional and Declarative Programming in Education, 2005.

* *"Foreign Interface for PLT Scheme"*, \
  Eli Barzilay, Dmitry Orlovsky, \
  Workshop on Scheme and Functional Programming, 2004.

* *"MetaPRL — A Modular Logical Environment"*, \
  Jason Hickey et al, \
  International Conference on Theorem Proving in Higher Order Logics, 2003.

* *"Practical Reflection in Nuprl"*, \
  Eli Barzilay, Stuart Allen, Robert Constable, \
  IEEE Symposium on Logic in Computer Science, 2003.

* *"Reflecting Higher-Order Abstract Syntax in Nuprl"*, \
  Eli Barzilay, Stuart Allen, \
  Theorem Proving in Higher Order Logics; Track B Proceedings of the
  15th International Conference on Theorem Proving in Higher Order
  Logics (TPHOLs), pp. 23–32, NASA, 2002.

* *"Abstraction as a Means for End-User Computing in Creative Applications"*, \
  Mira Balaban, Eli Barzilay, Michael Elhadad, \
  IEEE Transactions on Systems, Man, and Cybernetics, Part A, Vol. 32,
  No. 6, Nov. 2002, pp. 640–653.

* *"Quotation and Reflection in Nuprl and Scheme"*, \
  Eli Barzilay, \
  Cornell University technical report TR2001-1832, 2001.

* *"Booms Object Oriented Music System"*, \
  Eli Barzilay, \
  M.Sc. Thesis, 1997.


## Pre-Microsoft Experience

* 2014–2015 Gefen Dekel — Senior Software Developer

  Helped embedding Chromium into Dalet's desktop application, later
  followed up with learning about how React can be used in a
  browser-based editor, and eventually ran a two day tutorial on React,
  to ensure that it will actually get used.

* 2003–2014 System Administrator — as part of the PLT group behind
  Racket, I was in charge of the framework of servers, builds, releases,
  code repository and all other services that are required to run such a
  project.

* 1995–1997 Gefen-Dekel — Was among the first programmers of Gefen
  Dekel, the software development branch of Dalet Digital Media Systems.
  Developed multi-media applications for Windows, aimed mainly at radio
  station software and advertisement management.

* 1994 Shibutzit — Summer work in Shibutzit, a small company, developing
  a Prolog-based application for Windows that was used for allocation of
  human resources.

* 1993 System Administrator — During the last year of my undergraduate
  studies, worked as part of the system administration group for the
  Computer Science department in Ben-Gurion University.


## Pre-Microsoft Major Projects

(Approximate dates)

* 2008–2010 The Scribble Reader

  Designed a concrete syntactic extension to Racket that combines the
  convenience of "here-documents" and string interpolation, while
  maintaining the benefits of S-expressions.  The idea behind the syntax
  is based on the Nuprl quotations, which result in a language that is
  easier to use than quasi-quotations.  The syntax was named "Scribble",
  and later evolved into Racket's documentation system, making one of
  the earlier cases of using Racket as a real language framework, where
  a single program can be written multiple languages that are different
  even at the concrete syntax level.

* 2005–2006 Lazy Scheme

  Implemented a Lazy Scheme language for PLT Scheme (later called Lazy
  Racket).  This is a language that is identical to Scheme except for
  its lazy execution semantics.  It was developed for the PL course, but
  the result is a powerful ability to mix lazy and strict code in a
  single language, using Racket's module system.  This was in addition
  to other languages developed for the course.

* 2004–2007 Handin Server and Client

  Extended the Racket Handin server and client (originally developed by
  Matthew Flatt) considerably, making it usable in courses with widely
  varying demands.  In addition, designed a completely automated system
  to deal with running the PL course, including website publishing and
  on-line content (class notes, homework handins, etc.), electronic
  grading, secure grades publishing with statistics and graphs,
  class-participation tool, a kiosk-mode client for taking exams, and
  more.

* 2003–2004 Foreign Function Interface for Racket

  Designed and implemented Racket's foreign function interface for
  Racket, replacing a more traditional C-based approach where Racket
  code replaces the role of C.

* 2003–2014 PLT Racket Infrastructure

  Implemented the build process for making nightly builds of Racket as
  well as official distributions, revised the web-content framework
  using the new Scribble-based syntax as a markup language, was in
  charge of moving the Racket code base from CVS to Subversion first and
  then to git next.

* 1999–2003 Reflection for Nuprl

  Implemented reflection for the Nuprl theorem prover.

* 2001 Web-server

  Wrote a complete web-server in Scheme, with the purpose of being very
  flexible and for making an on-line picture collection.

* 1999 Picture collection management

  Designed and implemented a set of tools for managing a picture
  collection, as part of a photography project (for a minor in art at
  Cornell).

* 1999 HTML generation

  Wrote a system to easily generate HTML code in an easily extensible
  way, relying on such features as self-quoting code and keyword
  argument.  This system was used for several years by a few academic
  and commercial sites.  Years later it evolved into the Scribble
  syntax.

* 1998 Swindle

  Implemented Swindle: an extension language on top of PLT-Scheme/Racket
  for CS212 in the Cornell CS department.  Swindle was in active use at
  several universities for many years, and still is in use in some
  places.

* 1997 Calculator

  Implemented an interactive calculator package which is now a standard
  part of Emacs.

* 1995–1996 Booms

  A visual language environment for creative editing, mainly intended
  for music composition.  Implemented in Allegro Common Lisp for
  Windows.

* 1994–1996 Multeam

  A Usenet-like system for managing media content which was developed at
  Gefen-Dekel Technologies (part of Dalet Digital Media Systems).  The
  original project was discontinued but served as a basis for the "Dalet
  Web Publisher" which was eventually integrated in Dalet's main
  application — DaletPlus, and later Dalet Galaxy.


## Personal

* Citizen of Israel, permanent US residence since April 2006.

* Work preference:
  - Functional programming.
  - Building new projects rather than maintenance of existing software.
  - Languages:
    - I love hacking Racket; specifically, creating little languages and
      using its meta-programming capabilities.
    - More recently I switched to JavaScript/TypeScript as my language
      of choice in commercial settings.
  - Remote or hybrid in the Boston area.

* Photography: A minor in Art at Cornell, mostly digital photography.
