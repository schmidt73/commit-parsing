structure JSONTests =
struct
  structure J = JSONParser
  open J

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

  val test1 = 
    let 
      val json = concat [
        "{\"hello darkness\": {\"my\" : \"old friend.\"}, ",
        "\"arr\": [123e+5, 100e-3]}"
      ]
      val expected = OBJ [
        ("hello darkness", OBJ [("my", STR "old friend.")]),
        ("arr", ARR [NUM 12300000.0, NUM 0.1])
      ]
      val output = parseJSON json
      val passed = case output of
                       ERR _ => false
                     | OK a => jEq (a, expected)
   in
      (passed, "Simple JSON test.")
    end

end
