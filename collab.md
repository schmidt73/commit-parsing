To the members of the selection committee:


We are excited about this collaboration in part because we both share
a deep interest in *functional programming*---an intriguing, proven
body of techniques for creating computer software.  Functional
programming is notable for its expressive power: instead of being
shackled to the language of the computer, functional programmers can
often express software solutions using the same language we use to
think about problems.  Professor Ramsey also has a longstanding interest in
(and international reputation for) the implementation of programming
languages, an interest that Mr. Schmidt shares.

Professor Ramsey likes the project that Mr. Schmidt has proposed, for
several reasons.

  - The project serves a need that is felt by anyone who is learning
    to program computers, and therefore by anyone who is teaching
    others who are learning to program computers.  This need is
    aligned with Professor Ramsey's interest in developing and
    teaching new computer-programming languages.  In addition, this
    need has already been felt by the creators of the Elm programming
    language, who work extensively with K-12 teachers.  

  - There is a genuine cap in our knowledge: although the professional
    literature contains a large body of work on "combinator parsing,"
    little of that work is devoted to error handling, and none of that
    addresses the question of how errors are reported to a person.

  - The project is well scoped.  Algebraic laws, which generalize the
    familiar $x + 0 = x$ from our school days, are perfectly suited to
    explore solutions to the problem of error reporting, and their use
    can be mastered by an undergraduate research student.  A summer's
    work seems ample to explore several alternatives and identify the
    best.  Followup work during the succeeding academic year could
    well result in professional publication.

Our mentoring plan fits the "SCRUM for research" model described by
Mike Hicks and Jeff Foster in a 2010 article from *Communications of
the ACM*.  Professor Ramsey meets frequently with all his research
students in a body, typically four days per week for 20 minutes
each day.  In that meeting, each student states his or her current
status, needs, and action plan.  The daily meeting is supplemented by
longer meetings scheduled *ad hoc*.  Professor Ramsey anticipates
using those longer meetings to help Mr. Schmidt refine algebraic laws,
to consider alternatives, and later in the summer, to design
experiments that will help evaluate the work.

Professor Ramsey's expectations of Mr. Schmidt include

  - He will design new parsing combinators and express the meanings of
    those designs using algebraic laws.

  - He will consider design alternatives, and moreover, he will write
    short, "zero draft" technical essays explaining what alternatives
    are being considered, what makes each alternative attractive, and
    when an alternative is rejected, what made it inferior.

  - He will implement the best alternative in a library of parsing
    combinators.

  - He will use the library to implement experimental parsers for at
    least two, and preferably three, syntactically different formal
    languages.  Each language should be one with a pre-existing
    implementation, so that error messages can be compared.
    Implementing a well-chosen subset of each language will be
    acceptable.

  - He will develop a corpus of syntactically incorrect programs,
    together with corresponding error messages.

  - He will refine the experimental parsers to improve the error
    messages, and he will carefully document, with contemporaneous lab
    notes, the steps needed to improve error messages, as well as the
    costs.

  - He will compare results with those from pre-existing
    implementations.

  - If results from the summer look promising, he will continue to
    follow up, either through May of 2019 or until a paper is
    submitted for publication, whichever comes first.

Several doctoral students in Computer Science have interests that
overlap with this project, but no others are directly involved.
We expect to stay in communication with them, and also with Evan
Czaplicki, who is the chief creator of the Elm programming language.


Yours Sincerely,


Norman Ramsey\
Henri Schmidt
