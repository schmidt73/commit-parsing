(* DATA DECLARATIONS *)

(*
 * State represents the current state of the parser.
 *)
type state = { source : string
             , offset : int
             , row : int
             , col : int
             }

datatype ('a, 'b) result = SUCCESS of 'a
                         | ERROR of 'b
                         | FAIL

(*
 * A parser is a function that takes some initial state and outputs
 * a final state along with a result.
 *
 * On success the result has type 'a and on error has type 'b.
 *)
type ('a, 'b) parser = state -> state * ('a, 'b) result

(* END OF DATA DECLARATIONS *)





(* UTILITY FUNCTIONS *)

fun select [] def = def
  | select (x::xs) def = 
    case x of 
         (true, a) => a
       | _ => select xs def

fun curry f a b = f (a, b)
fun uncurry f (a, b) = f a b
 
fun strHead str = String.sub (str, 0)
fun strTail str = String.extract (str, 1, NONE)
         
fun createState src off row col = {source=src, offset=off, row=row, col=col}

fun updateState {source=src, offset=off, row=row, col=col} c =
  let 
    val newSt = createState src (off + 1)
    val tab = ((col div 8) + 1) * 8
    val p = select [(c = #"\r" orelse c = #"\n", newSt (row + 1) 0),
                    (c = #"\t", newSt row tab)]
  in 
    p (newSt row (col + 1))
  end
(* END OF UTILITY FUNCTIONS *)





(* PRIMITIVE PARSING FUNCTIONS *)

fun runParser (p : ('a, 'b) parser) src = 
  p {source=src, offset=0, row=0, col=0}

(*
 * Parser that always succeeds.
 *)
fun succeeds a : ('a, 'b) parser = 
  fn st => (st, SUCCESS a)

(* 
 * Parser that always fails.
 *)
val fail : ('a, 'b) parser =
  fn st => (st, FAIL)

(* 
 * Parser that always fails with fatal error.
 *)
fun error e : ('a, 'b) parser = 
  fn st => (st, ERROR e)

fun satisfy (p : char -> bool) : (char, 'b) parser = 
  fn st => 
    let 
      val {source=inp, offset=off, row=row, col=col} = st
      fun curr () = String.sub (inp, off) (* Kind of wish I had lazy evaluation here *)
    in
      if size inp <= off then fail st
      else if p (curr ()) then succeeds (curr ()) (updateState st (curr ()))
      else fail (updateState st (curr ()))
    end

fun isChar c = satisfy (fn c' => c' = c)

infix 2 <|>
infix 3 >>=
infix 4 <*>
infix 5 <$>

fun (pa : ('a, 'b) parser) <|> (pb : ('a, 'b) parser) : ('a, 'b) parser = 
  fn st =>
    case pa st of
         (st', ERROR e) => (st', ERROR e)
       | (_, FAIL) => pb st
       | p => p

fun (p : ('a, 'c) parser) >>= (f : 'a -> ('b, 'c) parser) : ('b, 'c) parser =
  fn st =>
    case p st of
         (st', SUCCESS a) => (f a) st'
       | (st', ERROR e) => (st', ERROR e)
       | (st', FAIL) => fail st

fun pf <*> pa = pf >>= (fn f => pa >>= (fn a => succeeds (f a)))

fun f <$> (p : ('a, 'c) parser) : ('b, 'c) parser = 
  succeeds f <*> p

fun str s = 
  let
    fun str' s i =
      if (size s <= i) then succeeds []
      else (curry op ::) <$> isChar (String.sub (s, i)) <*> str' s (i + 1)
  in
    implode <$> str' s 0
  end

(* 
* This doesn't work (I suspect due to strict evaluation), but I still don't
* understand fully why.
*
* fun many p  = ((curry op :: <$> p <*> many p) <|> succeeds [])
*)

fun many p = fn cs => ((curry op :: <$> p <*> many p) <|> succeeds []) cs
fun some p = curry op :: <$> p <*> many p

(* 
 * Forces the parser to either succeed or error.
 *
 * Laws:
 *    - (commits e) fail = error e 
 *
 * Example: 
 *   runParser (commits e (literal "a") <|> literal "b") "b" = (_, ERROR e)
 *)
fun commits err (p : ('a, 'b) parser) : ('a, 'b) parser = 
  fn st =>
    case p st of
         (st', FAIL) => (st', ERROR err)
       | p => p

(* 
 * Forces the parser to either succeed or fail.
 *
 * Laws: 
 *    - nocommits (error _) = fail
 *
 * Example: 
 *   runParser (nocommits (error 5)) _ = (_, FAIL)
 *)
fun nocommits (p : ('a, 'b) parser) =
  fn st =>
    case p st of
         (st', ERROR e) => (st', FAIL)
       | p => p

(* END OF PRIMITIVE PARSING FUNCTIONS *)
