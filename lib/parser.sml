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

  (* Although failure and nope might seem similar, one should note
   * that failure is unrecoverable whereas nopes are recoverable.
   * Both produce the default error message.
   *
   * Example: 
   *          nope <|> succeed a == succeed a
   *          fail <|> _ == fail
   *)
  val fail : 'a parser (* A parser that always fails *)
  val nope : 'a parser (* A parser that fails to recognize its input *)

  (* Converts the parser to one that produces the error message
   * on failure. 
   *
   * It is important to note that: 
   *    withError e nope != withError e fail
   *
   * The first is a parser that does not recognize input and
   * will produce the error e if committed. The second will
   * always fail with the error e.
   *)
  val withError : error -> 'a parser -> 'a parser

  val succeed : 'a -> 'a parser (* A parser that always succeeds *)

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

 (* A parser that will always succeed, 
  * attempting to consume input of type a. *)
  val optional : 'a parser -> unit parser 

  val many : 'a parser -> ('a list) parser
  val many1 : 'a parser -> ('a list) parser

  val alt : ('a parser) list -> 'a parser
  val sequence : ('a parser) list -> ('a list) parser

 (* A parser that looks at the next token without 
  * consuming any input, if it fails to consume 
  * input it will nope *)
  val lookahead : token parser

 (* Committing a parser means that on success, the parser "commits" to 
  * its path ensuring that the only two outcomes are producing a value or
  * a parser error. *)
  val commits : 'a parser -> 'a parser
  val nocommits : 'a parser -> 'a parser

  val runParser : 'a parser -> token list -> token list * 'a outcome
end

functor Parser (E:ERROR) (T:TOKEN) :>
  PARSER where type error = E.error 
           and type token = T.token =
struct 
  type error = E.error
  type token = T.token

  datatype 'a outcome = OK of 'a | ERR of error

  datatype 'a result = SUCCESS of 'a
                     | COMMIT of 'a
                     | FAILURE of error
                     | NOPE of error

  type 'a parser = token list -> token list * 'a result

  val fail = fn xs => (xs, FAILURE E.default)
  val nope = fn xs => (xs, NOPE E.default)

  fun withError e p = fn xs =>
    case p xs of
         (xs', FAILURE _) => (xs', FAILURE e)
       | (xs', NOPE _) => (xs', NOPE e)
       | def => def

  fun succeed a = fn xs => (xs, SUCCESS a)

  infix 2 <|>
  infix 3 >>=
  infix 4 <*> <* *>
  infix 5 <$>

  fun p >>= f = fn xs =>
    case p xs of 
         (xs', SUCCESS a) => (f a) xs'
       | (xs', FAILURE e) => (xs', FAILURE e)
       | (xs', NOPE e) => (xs, NOPE e)
       | (xs', COMMIT a) => (case (f a) xs' of
                                  (xs'', SUCCESS a) => (xs'', COMMIT a)
                                | (xs'', NOPE e) => (xs'', FAILURE e)
                                | def => def)

  fun p <|> p' = fn xs =>
    case p xs of 
         (_, NOPE _) => p' xs
       | def => def

  fun pf <*> pa = pf >>= (fn f => pa >>= (fn a => succeed (f a)))
  fun f <$> p = succeed f <*> p

  fun pa *> pb = (fn _ => fn b => b) <$> pa <*> pb
  fun pa <* pb = (fn a => fn _ => a) <$> pa <*> pb

  fun one [] = nope []
    | one (x::xs) = (succeed x) xs
    
  fun optional p = (p *> succeed ()) <|> succeed ()
    
  fun eof [] = (succeed ()) []
    | eof xs = nope []

  fun sat pred p = p >>= (fn a => if pred a then succeed a else nope)
  fun option p = p >>= (fn NONE => nope | SOME a => succeed a)

  fun curry f x y = f (x, y)

  fun many p = fn cs => ((curry op :: <$> p <*> many p) <|> succeed []) cs
  fun many1 p = curry op :: <$> p <*> many p

  fun alt xs = foldr (op <|>) nope xs

  fun sequence [] = succeed []
    | sequence (x :: xs) = curry op :: <$> x <*> sequence xs

  fun lookahead [] = nope []
    | lookahead (x :: xs) = (x::xs, SUCCESS x)

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
         (xs, FAILURE e) => (xs, ERR e)
       | (xs, NOPE e) => (xs, ERR e)
       | (xs, SUCCESS a) => (xs, OK a)
       | (xs, COMMIT a) => (xs, OK a)

end
