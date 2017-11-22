Parsing laws
------------
A parser may have one of these four outcomes:

  - *Committed* recognizes the input and is committed to it.
  - *Good* recognizes the input but is not committed to it.
  - *Nope* doesn't recognize the input.
  - *Error* has found a syntax error.

Notes:

  - "Committed" is a decision point that says, "after this point, the
    only possible outcomes are that I recognize something or I find a
    syntax error."  There are some algebraic laws below.

Different outcomes carry values.

  - A *good* or *committed* outcome carries a value of type `a`.
  - A *nope* outcome carries no value.
  - An *error* outcome carries a value that might have these parts:
      - An error message (perhaps parametric)
      - A region in the source code
      - One of Evan's "contexts"


Here are some algebraic laws for outcomes.  (I abuse Applicative
notation.)  The wildcard `_` suggests "not evaluated," and outcome `p`
(for "parser") designates an arbitrary outcome.

    commit <*> nope   --> error      (source of blame unclear)
    commit <*> commit = commit
    commit <*> good   = commit
    error  <*> _      = error
    p      <*> error  = error
    nope   <*> _      = nope
    good   <*> p      = p


Usability
---------
The distinction between *good* and *commit* creates design choices.
Evan suggests that, for usability,

  - A parser that consumes input *commits* by default.
  - A parser that consumes no input is *good* by default.

Whatever the defaults, it's easy to define

   commits  :: Parser a -> Parser a
   nocommit :: Parser a -> Parser a

with these laws:

   commits good    = commit
   commits p       = p
   nocommit commit = good
   nocommit p      = p

It's also possible to define a parser that always commits:

   commit :: Parser ()

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
