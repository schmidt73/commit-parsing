structure Tests = 
struct
  type name = string
  type tests = (bool * name) list

  fun zipWith f _ [] = []
    | zipWith f [] _ = []
    | zipWith f (x::xs) (y::ys) = (f (x, y)) :: (zipWith f xs ys)

  fun enumerate n = List.tabulate(n, fn x => x+1); 
  fun fst (a, _) = a

  fun getReport (passed, failed)  = 
  let 
    fun getMsg (_, msg) = msg ^ "\n"
    fun assignNums (num, str) = (Int.toString num) ^ ". " ^ str
    val (npass, nfail) = (length passed, length failed)
    val ntotal = npass + nfail
    val total = Int.toString ntotal
    val failTest = concat o (zipWith assignNums (enumerate ntotal)) o (map getMsg)
    val str = case (npass, nfail) of
                (_, 0) => "All " ^ total ^ " tests passed."
              | (0, _) => "All " ^ total ^ " tests failed."
              | (_, _) => concat [
                  "TESTS FAILED\n============\n",
                   failTest failed,
                  "\nNUM FAILED: " ^ (Int.toString nfail),
                  "\nNUM PASSED: " ^ (Int.toString npass),
                  "\nNUM TOTAL: " ^ total
                ]
  in 
    str 
  end

  fun runTests t = 
  let
    val passed = List.filter fst t
    val failed = List.filter (not o fst) t
  in 
    print ("\n" ^ (getReport (passed, failed)) ^ "\n")
  end
end

structure Main = 
struct 
  val _ = Tests.runTests []
end
