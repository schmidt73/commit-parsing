signature ERROR = 
sig
  type error

  val default : error
  val toString : error -> string
end

signature PARSER = 
sig
  include ERROR

  type 'a parser

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

  val eof : unit parser
  val one : char parser
  val sat : ('a -> bool) -> 'a parser -> 'a parser

  val many : 'a parser -> ('a list) parser
  val many1 : 'a parser -> ('a list) parser

  (* 
   * Committing a parser means that on success, the parser "commits" to 
   * its path ensuring that the only two outcomes are producing a value or
   * a parser error.
   *)
  val commits : 'a parser -> 'a parser
  val nocommits : 'a parser -> 'a parser
end ;


functor ParserFun(E:ERROR) : PARSER =
struct 
  type error = E.error

  datatype 'a result = SUCCESS of 'a
                     | COMMIT of 'a
                     | ERROR of error
                     | NOPE

  type 'a parser = char list -> char list * 'a result

  val default = E.default
  val toString = E.toString

  fun error e = fn xs => (xs, ERROR e)
  fun succeed a = fn xs => (xs, SUCCESS a)
  val nope = fn xs => (xs, NOPE)

  infix 2 <|>
  infix 3 >>=
  infix 4 <*>
  infix 5 <$>

  fun p >>= f = fn xs =>
    case p xs of 
         (xs', SUCCESS a) => (f a) xs'
       | (xs', ERROR e) => (xs', ERROR e)
       | (xs', NOPE) => (xs, NOPE)
       | (xs', COMMIT a) => (case (f a) xs' of
                                  (xs'', SUCCESS a) => (xs'', COMMIT a)
                                | (xs'', NOPE) => (xs'', ERROR default)
                                | def => def)

  fun p <|> p' = fn xs =>
    case p xs of 
         (_, NOPE) => p' xs
       | def => def

  fun pf <*> pa = pf >>= (fn f => pa >>= (fn a => succeed (f a)))
  fun f <$> p = succeed f <*> p

  fun one [] = nope []
    | one (x::xs) = (succeed x) xs
    
  fun eof [] = (succeed ()) []
    | eof xs = nope []

  fun sat pred p = p >>= (fn a => if pred a then succeed a else nope)

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

end
