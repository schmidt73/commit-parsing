## What is a parser?
--------------------

Abstractly, a parser is something that attempts to understand 
some input. In our representation, a parser takes a list of tokens and
produces a result of a certain type along with an unconsumed list of 
tokens. The type signature (in Standard ML) is as follows:

``` sml
type 'a parser = token list -> token list * 'a result
```

The result can be in one of the following four contexts:

    - A success context which simply holds the parsed value.
    - A nope context which represents failure to recognize input.    
    - A failure context which represents absolute failure. 
    - A commit context which holds the parsed value and represents further commitment to success.

These outcomes may seem a little confusing at first, but will
be further explored through examples and algebraic laws.

### Failure 
-----------

Essentially, the difference between nope and failure is that
in the former the parser can recover (using the backtrack combinator),
while in the latter it can not.

The following laws describe that more idea concretely. When defining 
laws, assume p represents an arbitrary parser and good represents an 
arbitrary parser that always succeeds. All laws evaluate to true.

```sml
val backtrackLaw   = nope <|> p = p
val noBacktrackLaw = fail <|> p = fail

val failureLaw1    = fail *> p = fail
val failureLaw2    = p *> fail = fail

val nopeLaw        = nope *> p = nope
val goodLaw        = good *> p = p
```

The reason for differentiating between nope and failure is that in
some situations we know we will never understand the input
and must fail. While other times we simply want to test if any
parser succeeds. Examples will help to clarify:

```sml
fun threeTup a b c = (a, b, c)

(* A theoretical parser that will parse an ifThenElse expression
 * into a three tuple containing the three expressions. *)
val ifThenElse = 
  threeTup <$> (ifP *> expr) <*> (thenP *> expr) <*> (elseP *> expr)
```

There are a many ways this parser will act depending on how the ifP, thenP, 
elseP and expression parsers are defined. One reasonable way to do this
would be to have each produce nope on failure. However, then if ifThenElse
attempts to parse an input such as "if e1 thex e2 else e3" it 
would produce a nope, allowing us to backtrack and try
different parsers on the input. In some situations this may be desirable, 
but in many we know after parsing an "if" successfully that the only 
parser that could succeed is the ifThenElse parser and that we shouldn't 
try any others. To represent this parser we could have ifP produce a 
nope on invalid input and the remainder produce failure on invalid input.
This would make the example input produce failure and
an example such as "var x = 5" produce a nope.

Notice though that it would be more desirable to have ifP, thenP, elseP,
and expr each produce a nope on invalid input for reuse in different 
parsers, while still keeping ifThenElse's semantics. This is where the 
idea of commitment comes into play.

### Commitment
--------------

Commitment, as the name implies, represents the notion of comitting to 
success or failure within the parser. The idea is at some point we can tell 
the parser that if the following succeeds, everything after it must 
succeed or fail (it will not produce a nope). 

The following laws explain this relationship:

```sml
val identityLaw = (nocommits o commits) p = p

val commitsLaw = commit *> p = commits p <|> fail

val commitsNopeLaw  = commits nope   = nope
val commitsNopeLaw2 = commit *> nope = fail

val commitsGoodLaw  = commit *> good *> nocommit *> p = good *> p
val commitsGoodLaw2 = commits good *> p = commits (good *> (p <|> fail))

val doubleCommitLaw = commit *> commits p = commits p <|> fail
```

Following is the new definition of ifThenElse using the notion of
commitment:

```sml
val newIf = commits ifP
val ifThenElse = 
    threeTup <$> (newIf *> expr) <*> (thenP *> expr) <*> (elseP *> expr)
```

This new definition of ifThenElse has exactly the behaviour previously
desired while still allowing ifP, thenP, elseP, and expr to nope on
invalid input. That is, running ifThenElse on the string 
"if e1 thex e2 else e3" outputs failure while running it on "val x = 5" 
outputs nope. Here commitment serves as an elegant way to shortcut
failure and avoid excess computation.

### Errors
----------



