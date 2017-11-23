use "parser.sml";

structure IntError =
struct 
  type error = int

  fun toString e = Int.toString e
  val default = 0
end

structure P = ParserFun(IntError)

open P

val digit = sat (fn c => #"1" <= c andalso c <= #"9") one
val number = Option.valOf <$> 
  (sat Option.isSome ((Int.fromString o implode) 
  <$> many1 digit))

val main = 
  let
    val (str, LEFT valid) = runParser number "12345678"
  in
    print (Int.toString (valid * 2))
  end
