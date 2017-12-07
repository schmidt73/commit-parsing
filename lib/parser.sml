signature ERROR = 
sig
  type error

  val default : error
  val toString : error -> string
end

signature TOKEN =
sig 
  type token
end

signature PARSER = 
sig
  type token
  type error
  type 'a result

  type 'a parser = token list -> token list * 'a result

  datatype 'a outcome = OK of 'a | ERR of error

  (*
   * Although error and nope might seem similar, one should note
   * that errors are unrecoverable whereas nopes are recoverable.
   *
   * Example: 
   *          nope <|> succeed a == succeed a
   *          error e <|> _ == error e
   *)
  val error : error -> 'a parser (* A parser that always errors *)
  val succeed : 'a -> 'a parser (* A parser that always succeeds *)
  val nope : 'a parser (* A parser that fails to recognize its input *)

  val <$> : ('a -> 'b) * 'a parser -> 'b parser
  val <*> : ('a -> 'b) parser * 'a parser -> 'b parser
  val >>= : 'a parser * ('a -> 'b parser) -> 'b parser
  val <|> : 'a parser * 'a parser -> 'a parser

  val *> : 'a parser * 'b parser -> 'b parser
  val <* : 'a parser * 'b parser -> 'a parser

  val eof : unit parser
  val one : token parser
  val sat : ('a -> bool) -> 'a parser -> 'a parser
  val option : ('a option) parser -> 'a parser

  val many : 'a parser -> ('a list) parser
  val many1 : 'a parser -> ('a list) parser

  (* 
   * Committing a parser means that on success, the parser "commits" to 
   * its path ensuring that the only two outcomes are producing a value or
   * a parser error.
   *)
  val commits : 'a parser -> 'a parser
  val nocommits : 'a parser -> 'a parser

  val runParser : 'a parser -> token list -> token list * 'a outcome
end ;


functor ParserFun (E:ERROR) (T:TOKEN) :>
  PARSER where type error = E.error 
           and type token = T.token =
struct 
  type error = E.error
  type token = T.token

  datatype 'a outcome = OK of 'a | ERR of error

  datatype 'a result = SUCCESS of 'a
                     | COMMIT of 'a
                     | ERROR of error
                     | NOPE

  type 'a parser = token list -> token list * 'a result

  fun error e = fn xs => (xs, ERROR e)
  fun succeed a = fn xs => (xs, SUCCESS a)
  val nope = fn xs => (xs, NOPE)

  infix 2 <|>
  infix 3 >>=
  infix 4 <*> <* *>
  infix 5 <$>

  fun p >>= f = fn xs =>
    case p xs of 
         (xs', SUCCESS a) => (f a) xs'
       | (xs', ERROR e) => (xs', ERROR e)
       | (xs', NOPE) => (xs, NOPE)
       | (xs', COMMIT a) => (case (f a) xs' of
                                  (xs'', SUCCESS a) => (xs'', COMMIT a)
                                | (xs'', NOPE) => (xs'', ERROR E.default)
                                | def => def)

  fun p <|> p' = fn xs =>
    case p xs of 
         (_, NOPE) => p' xs
       | def => def

  fun pf <*> pa = pf >>= (fn f => pa >>= (fn a => succeed (f a)))
  fun f <$> p = succeed f <*> p

  fun pa *> pb = (fn _ => fn b => b) <$> pa <*> pb
  fun pa <* pb = (fn a => fn _ => a) <$> pa <*> pb

  fun one [] = nope []
    | one (x::xs) = (succeed x) xs
    
  fun eof [] = (succeed ()) []
    | eof xs = nope []

  fun sat pred p = p >>= (fn a => if pred a then succeed a else nope)
  fun option p = p >>= (fn NONE => nope | SOME a => succeed a)

  fun curry f x y = f (x, y)

  fun many p = fn cs => ((curry op :: <$> p <*> many p) <|> succeed []) cs
  fun many1 p = curry op :: <$> p <*> many p

  fun commits p = fn xs =>
    case p xs of
         (xs', SUCCESS a) => (xs', COMMIT a)
       | def => def

  fun nocommits p = fn xs =>
    case p xs of
         (xs', COMMIT a) => (xs', SUCCESS a)
       | def => def

  fun runParser p xs = 
    case p xs of
         (xs, ERROR e) => (xs, ERR e)
       | (xs, NOPE) => (xs, ERR E.default)
       | (xs, SUCCESS a) => (xs, OK a)
       | (xs, COMMIT a) => (xs, OK a)

end
