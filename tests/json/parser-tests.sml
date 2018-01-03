functor TestJSONParser(JP:JSON_PARSER) :> 
sig 
  val tests : (bool * string) list
end
  =
struct
  open JP
  open JP.JSON

  fun zipWith f _ [] = []
    | zipWith f [] _ = []
    | zipWith f (x::xs) (y::ys) = (f (x, y)) :: (zipWith f xs ys)

  val all = foldr (fn (a, b) => a andalso b) true 

  (* TODO: Comparing real numbers is weird. *)
  fun jEq (STR x1, STR x2) = x1 = x2
    | jEq (NUM x1, NUM x2) = Real.toString x1 = Real.toString x2
    | jEq (BOOL x1, BOOL x2) = x1 = x2
    | jEq (NULL, NULL) = true
    | jEq (OBJ x1, OBJ x2) = all (zipWith memberEq x1 x2)
    | jEq (ARR x1, ARR x2) = all (zipWith jEq x1 x2)
    | jEq _ = false
  and memberEq ((x1, x2), (y1, y2)) = x1 = y1 andalso (jEq (x2, y2)) 

  fun jEqs (xs, expected) = 
    (length xs = length expected) andalso (all (zipWith jEq xs expected))

  val test1 = 
    let 
      val json = concat [
        "{\"hello darkness\": {\"my\" : \"old friend.\"}, ",
        "\"arr\": [123e+5, 100e-3]}"
      ]
      val expected = [OBJ [
        ("hello darkness", OBJ [("my", STR "old friend.")]),
        ("arr", ARR [NUM 12300000.0, NUM 0.1])
      ]]
      val output = parseJSON json
      val passed = case output of
                       ERR _ => false
                     | OK xs => jEqs (xs, expected)
    in
      (passed, "Simple JSON test.")
    end

  val test2 = 
    let 
      val json = "{\"hell\\b\\f\\\\ \\t\\u000axs\" : null }"
      val expectedStr = "hell\b\f\\ \t\nxs"
      val expected = [OBJ [(expectedStr, NULL)]]
      val output = parseJSON json
      val passed = case output of
                       ERR _ => false
                     | OK xs => jEqs (xs, expected)      
    in
      (passed, "Escape sequence test.")
    end

  val tests = [test1, test2]
end

structure JParserTests = TestJSONParser(JSONParser)
structure JParserWithLexerTests = TestJSONParser(JSONParserFromLexer)
