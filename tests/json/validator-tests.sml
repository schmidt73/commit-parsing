structure PARSER = JSONParserFromLexer(JSONLexer)
structure V = JSONValidatorFromParser(PARSER)

structure ValidatorTests :>
sig 
  val tests : (bool * string) list
end
=
struct
  val test1 = 
  let
    val output = V.validateJSON "tests/json/examples/missing_bracket.json"
    val success = 
      case output of
           V.VALID => false
        | (V.ERR e) => e = "expected ] at row 5 col 5."
  in 
    (success, "Missing bracket test.")
  end

  val tests = [test1]
end
