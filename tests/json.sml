signature JSON_PARSER = 
sig
  datatype jvalue = STR of string (* Would like to use WideString *)
                  | NUM of real
                  | OBJ of jobject 
                  | ARR of jarray 
                  | BOOL of bool
                  | NULL
  withtype member = string * jvalue
       and jobject = member list
       and jarray = jvalue list

  datatype result = OK of jvalue | ERR of string

  val parseJSON : string -> result
  val toString : jvalue -> string
end

structure JSONParser :> JSON_PARSER = 
struct
  datatype jvalue = STR of string
                  | NUM of real
                  | OBJ of jobject 
                  | ARR of jarray 
                  | BOOL of bool
                  | NULL
   withtype member = string * jvalue
       and jobject = member list
       and jarray = jvalue list

  datatype result = OK of jvalue | ERR of string

  (* TODO: Improve error messages. *) 
  structure ParserError = 
  struct
    type error = string

    fun toString e = e
    val default = "Bad json."
  end

  structure P = ParserFun(ParserError)

  val <|> = P.<|>; val >>= = P.>>=; val <*> = P.<*>
  val *> = P.*>; val <* = P.<*; val <$> = P.<$>

  infix 2 <|>
  infix 3 >>=
  infix 4 <*> <* *>
  infix 5 <$>

  fun fst (a, _) = a
  fun snd (_, a) = a

  fun curry f x y = f (x, y)
  fun id x = x

  fun elem x xs = isSome (List.find (fn a => a = x) xs)
  fun pow' (a, 0) = 1.0
    | pow' (a, n) = a * pow' (a, n - 1)
  fun pow a n = if n < 0 then pow' (1.0 / a, ~n)
                else pow' (a, n)

  fun readHexStr str = 
    StringCvt.scanString (Int.scan StringCvt.HEX) str

  fun hexToChar c1 c2 c3 c4 =
  let 
    val str = implode [c1, c2, c3, c4]
    fun validate x = if (x >= 0 andalso x <= Char.maxOrd) 
                     then SOME (chr x)
                     else NONE
  in 
    Option.mapPartial validate (readHexStr str)
  end

  fun charSat c = P.sat (fn c' => c' = c) P.one 
  fun alt xs = foldr (op <|>) P.nope xs

  val whiteSpace = P.many (alt [
    charSat #" ", charSat #"\n", charSat #"\r", charSat #"\t"
  ])

  fun between p (c1, c2) = charSat c1 *> whiteSpace *> p <* whiteSpace <* charSat c2
  fun manySep p s = curry op :: <$> p <*> P.many (s *> whiteSpace *> p)

  fun strSat' (x::xs) = curry (op ::) <$> charSat x <*> strSat' xs
    | strSat' [] = P.succeed []

  fun strSat str = implode <$> strSat' (explode str)

  val hexDigits = P.option (hexToChar <$> P.one <*> P.one <*> P.one <*> P.one)

  val escapeChars = alt [
    (charSat #"u" *> hexDigits),
    (charSat #"\""),
    (charSat #"\\"),
    (charSat #"/"),
    (charSat #"b" *> P.succeed #"\b"),
    (charSat #"f" *> P.succeed #"\f"),
    (charSat #"n" *> P.succeed #"\n"),
    (charSat #"r" *> P.succeed #"\r"),
    (charSat #"t" *> P.succeed #"\t")
  ]

  val char = (P.commits (charSat #"\\") *> escapeChars) <|>
              P.sat (fn c => c <> #"\"" andalso c <> #"\\") P.one
  val string = between (implode <$> P.many char) (#"\"", #"\"")

  val digit = P.option ((Int.fromString o Char.toString) <$> P.one)
  val isNeg = (charSat #"-" *> P.succeed true) <|> P.succeed false

  val posPart = 
  let 
    fun sumDigits (d, (a, p)) = (p*d + a, 10*p)
    val listToInt = fst o (foldr sumDigits (0, 1))
    fun listToFrac xs = ((Real.fromInt o listToInt) xs) * (pow 10.0 (~(length xs)))
    val intPart = listToInt <$> P.many1 digit
    val fracPart = (charSat #"." *> listToFrac <$> P.many1 digit) <|> P.succeed 0.0
    val num = curry op + <$> (Real.fromInt <$> intPart) <*> fracPart
    val e = charSat #"e" <|> charSat #"E"
    val posNeg = (charSat #"+" *> P.succeed 1) <|> (charSat #"-" *> P.succeed ~1)
    val exp = curry op * <$> (e *> posNeg) <*> intPart
  in 
    curry op * <$> num <*> (pow 10.0 <$> (exp <|> P.succeed 0))
  end

  val num = isNeg >>= (fn a => (if a then (op ~) else id) <$> posPart)

  (* Parsers for json values *)
  val jString = STR <$> string
  val jNull = strSat "null" *> P.succeed NULL
  val jBool = strSat "true" *> P.succeed (BOOL true) <|>
              strSat "false" *> P.succeed (BOOL false)
  val jNumber = NUM <$> num

  fun jArray _ = fn cs => 
    (ARR <$> between (manySep (jValue ()) (charSat #",")) (#"[", #"]")) cs
  and jObject _ = fn cs =>
    let
      fun mkTuple a b = (a, b)
      val k = string
      val v = jValue ()
      val pair = mkTuple <$> (k <* whiteSpace <* charSat #":" <* whiteSpace) <*> v
    in
      (OBJ <$> between (manySep pair (charSat #",")) (#"{", #"}")) cs
    end
  and jValue _ = 
    fn cs => (alt [jString, jNull, jBool, jNumber, jArray (), jObject ()]) cs
 
  fun parseJSON str = 
    case (P.runParser (jValue ()) str) of
         (_, P.OK a) => OK a
       | (_, P.ERR s) => ERR s

  fun concatMap f xs = concat (map f xs)

  fun indent str = 
    concatMap (fn str => "\t" ^ str ^ "\n") (String.tokens (fn c => c = #"\n") str)

  fun toString (STR x) = "\"" ^ x ^ "\""
    | toString (NUM x) = Real.toString x
    | toString (BOOL x) = Bool.toString x
    | toString (NULL) = "null"
    | toString (OBJ xs) = "{\n" ^ indent (concatMap memberToString xs) ^ "},\n"
    | toString (ARR xs) = "[" ^ (concatMap arrToString xs) ^ "]"
  and memberToString (str, v) = "\"" ^ str ^ "\"" ^ " : " ^ (toString v) 
  and arrToString str = (toString str) ^ ", "

end
