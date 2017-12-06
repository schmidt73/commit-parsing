(*
structure IntError =
struct 
  type error = int

  fun toString e = Int.toString e
  val default = 0
end

structure P = ParserFun(IntError)

open P

infix 2 <|>
infix 3 >>=
infix 4 <*> <* *>
infix 5 <$>

val digit = sat (fn c => #"1" <= c andalso c <= #"9") one
val number = Option.valOf <$> 
  (sat Option.isSome ((Int.fromString o implode) 
  <$> many1 digit))
*)


structure PropertyTests=
struct

(*val randChar = chr (Rand.range (0, Char.maxOrd) Rand.random)*)

end

(*
infix 1 ==

fun p1 == p2 = 
  val gen_strings = 
    *)
