Parsing Laws
------------
A parser may have one of these four outcomes:

  - *Committed* recognizes the input and is committed to it.
  - *Good* recognizes the input but is not committed to it.
  - *Nope* doesn't recognize the input.
  - *Failure* has found an unrecoverable error.

Notes:

  - "Committed" is a decision point that says, "after this point, the
    only possible outcomes are that I recognize something or I find a
    syntax error."  There are some algebraic laws below.

Different outcomes carry values.

  - A *good* or *committed* outcome carries a value of type `a`.
  - A *nope* or *failure* outcome carries an error messsage.

Here are some algebraic laws for outcomes.  (I abuse Applicative
notation.)  The wildcard `_` suggests "not evaluated," and outcome `p`
(for "parser") designates an arbitrary outcome.

    commit  <*> commit  = commit
    commit  <*> good    = commit
    commit  <*> nope    = failure      
    commit  <*> failure = failure

    failure <*> _       = failure
    _       <*> failure = failure

    nope    <*> _       = nope
    good    <*> p       = p

Usability
---------
The distinction between *good* and *commit* creates design choices.
Evan suggests that, for usability,

  - A parser that consumes input *commits* by default.
  - A parser that consumes no input is *good* by default.

Whatever the defaults, it's easy to define

   commits : 'a parser -> 'a parser
   nocommit : 'a parser -> 'a parser

with these laws:

   commits good    = commit
   commits p       = p
   nocommit commit = good
   nocommit p      = p

It's also possible to define a parser that always commits:

   commit : unit parser

I don't see how to turn commitment *off* in pointlike fashion,
however.

If we like Elm's "parser pipelines," we can imagine something like
this:

    nextInt : Parser Int
    nextInt =
        succeed identity
          |. nocommit spaces
          |. symbol ","
          |. spaces
          |= int
