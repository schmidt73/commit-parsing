signature JSON_LEXER = 
sig
  datatype structural = LBRACKET
                      | RBRACKET
                      | LBRACE
                      | RBRACE
                      | COLON
                      | COMMA

  datatype literal = STR of string
                   | NUM of real
                   | BOOL of bool
                   | NULL

  datatype token = STRUCTURAL of structural
                 | LIT of literal

  type row = int and col = int
  type pos = row * col

  datatype result = OK of (token * pos) list | ERR of string

  val lexJSON : string -> result
  val toString : token list -> string
end

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

  datatype result = OK of jvalue list | ERR of string

  val parseJSON : string -> result
end

signature JSON_VALIDATOR = 
sig
  datatype result = VALID | ERR of string
  type filename = string

  val validateJSON : filename -> result
  val validateJSON_IO : filename -> unit
end

structure Utility =
struct
  fun fst (a, _) = a
  fun snd (_, a) = a

  fun flip f x y = f y x

  fun curry f x y = f (x, y)
  fun uncurry f (x, y) = f x y

  fun tup a b = (a, b)
  fun id x = x

  fun elem x xs = isSome (List.find (fn a => a = x) xs)
  fun zipWith f _ [] = []
    | zipWith f [] _ = []
    | zipWith f (x::xs) (y::ys) = (f (x, y)) :: (zipWith f xs ys)

  fun scanl f q ls = 
    q :: (case ls of
               []   => []
             | (x::xs) => scanl f (f (q, x)) xs)

  fun readHexStr str = StringCvt.scanString (Int.scan StringCvt.HEX) str

  fun hexToChar c1 c2 c3 c4 =
  let 
    val str = implode [c1, c2, c3, c4]
    fun validate x = if (x >= 0 andalso x <= Char.maxOrd) 
                     then SOME (chr x)
                     else NONE
  in 
    Option.mapPartial validate (readHexStr str)
  end

  fun pow' (a, 0) = 1.0
    | pow' (a, n) = a * pow' (a, n - 1)
  fun pow a n = if n < 0 then pow' (1.0 / a, ~n)
                else pow' (a, n)
end

structure JSONLexer :> JSON_LEXER = 
struct
  datatype structural = LBRACKET
                      | RBRACKET
                      | LBRACE
                      | RBRACE
                      | COLON
                      | COMMA

  datatype literal = STR of string
                   | NUM of real
                   | BOOL of bool
                   | NULL

  datatype token = STRUCTURAL of structural
                 | LIT of literal
  
  type row = int and col = int
  type pos = row * col

  datatype result = OK of (token * pos) list | ERR of string

  open Utility

  structure ParserError = 
  struct
    type error = string

    fun toString e = e
    val default = "invalid token, expected literal or structural token"
  end

  structure P = Parser(ParserError)(struct 
    type row = int and col = int
    type pos = row * col
    type token = pos * char 
  end)

  val <|> = P.<|>; val >>= = P.>>=; val <*> = P.<*>
  val *> = P.*>; val <* = P.<*; val <$> = P.<$>

  infix 2 <|>
  infix 3 >>=
  infix 4 <*> <* *>
  infix 5 <$>

  val oneChar = snd <$> P.one
  fun charSat c = P.sat (fn c' => c = c') oneChar 
  fun strSat str = implode <$> P.sequence (map charSat (explode str))

  fun mapPos cs = 
  let
    val linebreaks = [#"\r", #"\n"]
    fun f ((row, col), c) = if elem c linebreaks then (row + 1, 1)
                            else (row, col + 1)
    val positions = scanl f (1, 1) cs
  in
    zipWith id positions cs
  end 

  val hexDigits = 
    P.option (hexToChar <$> oneChar <*> oneChar <*> oneChar <*> oneChar)

  val escapeChars = P.alt [
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
             (P.sat (fn c => c <> #"\"" andalso c <> #"\\") oneChar)
  val str = STR <$>
    (charSat #"\"" *> (implode <$> P.many char) <* charSat #"\"")

  val digit = P.option ((Int.fromString o Char.toString) <$> oneChar)
  val isNeg = (charSat #"-" *> P.succeed true) <|> P.succeed false

  val positiveNum = 
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

  val num = NUM <$> (isNeg >>= 
    (fn a => (if a then (op ~) else id) <$> positiveNum))
  val null = strSat "null" *> P.succeed NULL
  val boolean = strSat "true" *> P.succeed (BOOL true) <|> 
                strSat "false" *> P.succeed (BOOL false)

  val structural = STRUCTURAL <$> P.alt [
    charSat #"{" *> P.succeed LBRACE,
    charSat #"}" *> P.succeed RBRACE,
    charSat #"[" *> P.succeed LBRACKET,
    charSat #"]" *> P.succeed RBRACKET,
    charSat #":" *> P.succeed COLON,
    charSat #"," *> P.succeed COMMA
  ]

  val lit = LIT <$> P.alt [boolean, null, num, str]
  val token = structural <|> lit

  val whiteSpace = P.many1 (P.alt [
    charSat #" ", charSat #"\n", charSat #"\r", charSat #"\t"
  ])

  val getPos = fst <$> P.lookahead
  fun addPos p = (flip tup) <$> getPos <*> p

  val tokens = 
  let 
    val tok = P.optional whiteSpace *> token <* P.optional whiteSpace
  in
    P.many (addPos tok) <* P.eof
  end

  fun lexJSON str = 
  let 
    val input = mapPos (explode str)
  in
    case (P.runParser tokens input) of
         (_, P.OK a) => OK a
       | (_, P.ERR s) => ERR s
  end

  fun tokToString (STRUCTURAL LBRACKET) = "LBRACKET"
    | tokToString (STRUCTURAL RBRACKET) = "RBRACKET"
    | tokToString (STRUCTURAL LBRACE) = "LBRACE"
    | tokToString (STRUCTURAL RBRACE) = "RBRACE"
    | tokToString (STRUCTURAL COLON) = "COLON"
    | tokToString (STRUCTURAL COMMA) = "COMMA"
    | tokToString (LIT (STR str)) = "STR: " ^ str
    | tokToString (LIT (NUM n)) = "NUM: " ^ (Real.toString n)
    | tokToString (LIT (BOOL true)) = "BOOL: true"
    | tokToString (LIT (BOOL false)) = "BOOL: false"
    | tokToString (LIT NULL) = "NULL"
    
  fun toString toks = String.concatWith "\n" (map tokToString toks)
end

functor JSONParserFromLexer (L:JSON_LEXER) :> JSON_PARSER =
struct
  datatype jvalue = STR of string (* Would like to use WideString *)
                  | NUM of real
                  | OBJ of jobject 
                  | ARR of jarray 
                  | BOOL of bool
                  | NULL
  withtype member = string * jvalue
       and jobject = member list
       and jarray = jvalue list

  datatype result = OK of jvalue list | ERR of string

  open Utility
  
  (* TODO: Improve error messages. *) 
  structure ParserError = 
  struct
    type error = string

    fun toString e = e
    val default = "Bad json."
  end

  structure P = Parser(ParserError)(struct 
    type token = L.token * L.pos
  end)

  val <|> = P.<|>; val >>= = P.>>=; val <*> = P.<*>
  val *> = P.*>; val <* = P.<*; val <$> = P.<$>

  infix 2 <|>
  infix 3 >>=
  infix 4 <*> <* *>
  infix 5 <$>

  val oneTok = fst <$> P.one

  (* Lenses to the rescue? *)

  fun literalL (L.LIT (L.STR str)) = SOME (STR str)
    | literalL (L.LIT (L.NUM x)) = SOME (NUM x)
    | literalL (L.LIT (L.BOOL b)) = SOME (BOOL b)
    | literalL (L.LIT (L.NULL)) = SOME NULL
    | literalL (L.STRUCTURAL _) = NONE

  fun stringL (L.LIT (L.STR str)) = SOME str
    | stringL _ = NONE

  fun rbracketL (L.STRUCTURAL L.RBRACKET) = SOME ()
    | rbracketL _ = NONE

  fun lbracketL (L.STRUCTURAL L.LBRACKET) = SOME ()
    | lbracketL _ = NONE

  fun lbraceL (L.STRUCTURAL L.LBRACE) = SOME ()
    | lbraceL _ = NONE

  fun rbraceL (L.STRUCTURAL L.RBRACE) = SOME ()
    | rbraceL _ = NONE

  fun colonL (L.STRUCTURAL L.COLON) = SOME ()
    | colonL _ = NONE

  fun commaL (L.STRUCTURAL L.COMMA) = SOME ()
    | commaL _ = NONE

  fun unLens lens = P.option (lens <$> oneTok)

  fun error p msg = 
  let 
    val getPos = snd <$> P.lookahead
    val pos = getPos
    fun stringify (row, col) = 
      "row " ^ Int.toString row ^ " col " ^ Int.toString col
    fun createErr pos = msg ^ " at " ^ (stringify pos) ^ "."
  in
    getPos >>= (fn pos => P.withError (createErr pos) p )
  end

  val literal = error (unLens literalL) "expected literal"
  val string = error (unLens stringL) "expected string key"
  val lbracket = error (unLens lbracketL) "expected ["
  val rbracket = error (unLens rbracketL) "expected ]" 
  val lbrace = error (unLens lbraceL) "expected {"
  val rbrace = error (unLens rbraceL) "expected }"
  val colon = error (unLens colonL) "expected :"
  val comma = error (unLens commaL) "expected ,"

  fun manySep p = curry op :: <$> p <*> P.many (P.commits comma *> p)

  fun between (s1, s2) p = s1 *> p <* s2

  val literal = unLens literalL
  
  fun member _ = fn cs => 
    (tup <$> (string <* colon) <*> value()) cs
  and object _ = fn cs => (between (P.commits lbrace, rbrace) 
    (OBJ <$> manySep (member ()))) cs
  and array _ = fn cs => (between (P.commits lbracket, rbracket)
    (ARR <$> manySep (value ()))) cs
  and value _ = fn cs => (literal <|> object () <|> array ()) cs

  val values = P.many1 (value ())

  fun debug e s = let val _ = print (s ^ "\n") in e end

  fun parseJSON' (toks : (L.token * L.pos) list) : result = 
    case (P.runParser values toks) of
         (_, P.ERR e) => ERR e
       | (_, P.OK vals) => OK vals

  fun parseJSON str = 
    case (L.lexJSON str) of
         (L.ERR e) => ERR e
       | (L.OK toks) => parseJSON' toks
end

structure JSONParser :> JSON_PARSER = 
struct
  datatype jvalue = STR of string (* Would like to use WideString *)
                  | NUM of real
                  | OBJ of jobject 
                  | ARR of jarray 
                  | BOOL of bool
                  | NULL
  withtype member = string * jvalue
       and jobject = member list
       and jarray = jvalue list

  datatype result = OK of jvalue list | ERR of string

  open Utility
  
  (* TODO: Improve error messages. *) 
  structure ParserError = 
  struct
    type error = string

    fun toString e = e
    val default = "Bad json."
  end

  structure P = Parser(ParserError)(struct type token = char end)

  val <|> = P.<|>; val >>= = P.>>=; val <*> = P.<*>
  val *> = P.*>; val <* = P.<*; val <$> = P.<$>

  infix 2 <|>
  infix 3 >>=
  infix 4 <*> <* *>
  infix 5 <$>

  fun charSat c = P.sat (fn c' => c' = c) P.one 

  fun strSat str = implode <$> P.sequence (map charSat (explode str))

  val whiteSpace = P.many (P.alt [
    charSat #" ", charSat #"\n", charSat #"\r", charSat #"\t"
  ])

  fun between p (c1, c2) = charSat c1 *> whiteSpace *> p <* whiteSpace <* charSat c2
  fun manySep p s = curry op :: <$> p <*> P.many (s *> whiteSpace *> p)

  val hexDigits = P.option (hexToChar <$> P.one <*> P.one <*> P.one <*> P.one)

  val escapeChars = P.alt [
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
    fn cs => (P.alt [jString, jNull, jBool, jNumber, jArray (), jObject ()]) cs

  val jValues = P.many1 (jValue ())
 
  fun parseJSON str = 
    case (P.runParser jValues (explode str)) of
         (_, P.OK a) => OK a
       | (_, P.ERR s) => ERR s


  (*
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
  *)
end

functor JSONValidatorFromParser(P:JSON_PARSER) :> JSON_VALIDATOR =
struct
  datatype result = VALID | ERR of string
  type filename = string

  fun validateJSON file = 
  let
    val stream = TextIO.openIn file
    val contents = TextIO.inputAll stream
    val output = P.parseJSON contents
  in
    case output of 
         (P.OK _) => VALID
       | (P.ERR e) => ERR e
  end

  fun validateJSON_IO file = 
    case validateJSON file of
         VALID => print "JSON is valid.\n"
       | (ERR e) => print ("JSON is invalid.\nError: " ^ e ^ "\n")
end
