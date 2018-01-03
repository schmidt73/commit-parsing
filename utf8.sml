structure UTF8 :> sig
  val printUTF8 : int -> unit
  exception NotUTF8
  val decodeUTF8 : char list -> (word * char list) option  (* raises NotUTF8 *)
end
  =
struct
  exception NotUTF8
  exception RuntimeError of string

  fun printUTF8 code =
    let val w = Word.fromInt code
        val (&, >>) = (Word.andb, Word.>>)
        infix 6 & >>
        val _ = if (w & 0wx1fffff) <> w then
                  raise RuntimeError (intString code ^
                                      " does not represent a Unicode code point")
                else
                   ()
        fun printbyte w = TextIO.output1 (TextIO.stdOut, chr (Word.toInt w))
        fun prefix byte byte' = Word.orb (byte, byte')
    in  if w > 0wxffff then
          app printbyte [ prefix 0wxf0  (w >> 0w18)
                        , prefix 0wx80 ((w >> 0w12) & 0wx3f)
                        , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                        , prefix 0wx80 ((w      ) & 0wx3f)
                        ]
        else if w > 0wx7ff then
          app printbyte [ prefix 0wxe0  (w >> 0w12)
                        , prefix 0wx80 ((w >>  0w6) & 0wx3f)
                        , prefix 0wx80 ((w        ) & 0wx3f)
                        ]
        else if w > 0wx7f then
          app printbyte [ prefix 0wxc0  (w >>  0w6)
                        , prefix 0wx80 ((w        ) & 0wx3f)
                        ]
        else
          printbyte w
    end
 
  (* prefixes
       0xxxxxxx <= 0wx7f
       110xxxxx <= 0wxdf
       1110xxxx <= 0wxef
       11110xxx <= 0wxf7
  *)

  fun decodeUTF8 [] = NONE
    | decodeUTF8 (c::cs) =
        let val w = Word.fromInt (Char.ord c)
        in  if w <= 0wx7f then
               (w, cs)
            else if w <= 0wxdf then
              decode (Word.andb (w, 0wx1f)) 1 cs
            else if w <= 0wxef then
              decode (Word.andb (w, 0wx0f)) 2 cs
            else if w <= 0wxf7 then
              decode (Word.andb (w, 0wx07)) 3 cs
            else
              raise NotUTF8
        end
  and decode prefix 0 cs = SOME (prefix, cs)
    | decode prefix n [] = raise NotUTF8
    | decode prefix n (c::cs) =
        let val w = Word.fromInt (Char.ord c)
            infix 5 << &&
            val op << = Word.<<
            val op && = Word.andb
        in  if w < 0wx80 then
              raise NotUTF8
            else if w <= 0wxbf then
              decode ((prefix << 0w6) + (w && 0wx3f)) (n - 1) cs
            else
              raise NotUTF8
        end
              
              
              
end
