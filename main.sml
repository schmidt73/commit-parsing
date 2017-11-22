use "parser.sml";

structure IntError =
struct 
  type error = int

  fun toString e = Int.toString e
  val default = 0
end

structure Parser = ParserFun(IntError)

val main = print "NOT IMPLEMENTED YET\n"
