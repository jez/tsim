structure Exponentiator : TURING =
struct
  open Prelude
  infixr 0 $

  datatype token = One
  type symbol = token option
  type tape = symbol Vector.vector

  datatype state =  Q1 |  Q2 |  Q3 |  Q4 |  Q5 |  Q6 |  Q7 |  Q8 |  Q9 | Q10
                 | Q11 | Q12 | Q13 | Q14 | Q15 | Q16 | Q17 | Q18 | Q19 | Q20
                 | Q21 | Q22 | Q23 | Q24 | Q25 | Q26 | Q27 | Q28 | Q29 | Q30
                 | Q31 | Q32 | Q33 | Q34 | Q35 | Q36 | Q37 | Q38 | Q39 | Q40
                 | Q41 | Q42 | Q43 | Q44 | Q45 | Q46 | Q47 | Q48 | Q49 | Q50
                 | Q51 | Q52 | Q53 | Q54 | Q55 | Q56 | Q57 | Q58 | Q59 | Q60
                 | Q61 | Q62 | Q63 | Q64 | Q65 | Q66 | Q67

  exception ExecError of string
  exception ParseError of string

  datatype move = Left | Here | Right

  fun initial () = Q1
  fun isFinal q = q = Q66

  fun delta (q, sym) =
    case (q, sym)
      of (Q1, SOME One)  => ( Q1, sym,      Right)
       | (Q1, NONE)      => ( Q2, sym,      Right)
       | (Q2, SOME One)  => ( Q3, sym,      Right)
       | (Q3, NONE)      => ( Q4, SOME One, Here)
       | (Q4, SOME One)  => ( Q4, sym,      Left)
       | (Q4, NONE)      => ( Q5, sym,      Left)
       | (Q5, SOME One)  => ( Q5, NONE,     Here)
       | (Q5, NONE)      => ( Q6, sym,      Left)
       | (Q6, SOME One)  => ( Q5, SOME One, Here)
       | (Q6, NONE)      => ( Q7, NONE,     Here)
       | (Q7, NONE)      => ( Q7, sym,      Right)
       | (Q7, SOME One)  => (Q66, SOME One, Here)
       | (Q3, SOME One)  => ( Q8, SOME One, Here)
       | (Q8, SOME One)  => ( Q8, sym,      Left)
       | (Q8, NONE)      => ( Q9, sym,      Left)
       | (Q9, SOME One)  => (Q10, sym,      Left)
       | (Q10, NONE)     => (Q11, sym,      Right)
       | (Q11, SOME One) => (Q11, sym,      Right)
       | (Q11, NONE)     => (Q12, sym,      Right)
       | (Q12, SOME One) => (Q12, NONE,     Here)
       | (Q12, NONE)     => (Q13, sym,      Right)
       | (Q13, SOME One) => (Q12, SOME One, Here)
       | (Q13, NONE)     => (Q14, NONE,     Here)
       | (Q14, NONE)     => (Q14, sym,      Left)
       | (Q14, SOME One) => (Q66, SOME One, Here)
       | (Q10, SOME One) => (Q15, SOME One, Here)
       | (Q15, SOME One) => (Q15, sym,      Right)
       | (Q15, NONE)     => (Q16, sym,      Right)
       | (Q16, SOME One) => (Q16, sym,      Right)
       | (Q16, NONE)     => (Q17, sym,      Left)
       | (Q17, SOME One) => (Q18, sym,      Left)
       | (Q18, SOME One) => (Q19, sym,      Left)
       | (Q19, NONE)     => (Q37, sym,      Right)
       | (Q37, SOME One) => (Q37, NONE,     Here)
       | (Q37, NONE)     => (Q38, sym,      Right)
       | (Q38, SOME One) => (Q39, NONE,     Here)
       | (Q39, NONE)     => (Q39, sym,      Left)
       | (Q39, SOME One) => (Q40, SOME One, Here)
       | (Q19, SOME One) => (Q20, sym,      Right)
       | (Q20, SOME One) => (Q21, sym,      Right)
       | (Q21, SOME One) => (Q21, NONE,     Here)
       | (Q21, NONE)     => (Q22, sym,      Left)
       | (Q22, SOME One) => (Q22, sym,      Left)
       | (Q22, NONE)     => (Q23, sym,      Left)
       | (Q23, SOME One) => (Q22, SOME One, Here)
       | (Q23, NONE)     => (Q24, SOME One, Here)
       | (Q24, SOME One) => (Q24, sym,      Right)
       | (Q24, NONE)     => (Q25, sym,      Right)
       | (Q25, SOME One) => (Q26, sym,      Right)
       | (Q26, SOME One) => (Q27, SOME One, Here)
       | (Q27, SOME One) => (Q27, sym,      Right)
       | (Q27, NONE)     => (Q28, sym,      Left)
       | (Q28, SOME One) => (Q28, NONE,     Here)
       | (Q28, NONE)     => (Q29, sym,      Left)
       | (Q29, SOME One) => (Q29, sym,      Left)
       | (Q29, NONE)     => (Q30, sym,      Left)
       | (Q30, SOME One) => (Q30, sym,      Left)
       | (Q30, NONE)     => (Q23, NONE,     Here)
       | (Q26, NONE)     => (Q31, SOME One, Here)
       | (Q31, SOME One) => (Q32, sym,      Right)
       | (Q32, NONE)     => (Q26, NONE,     Here)
       | (Q32, SOME One) => (Q33, sym,      Left)
       | (Q33, SOME One) => (Q33, NONE,     Here)
       | (Q33, NONE)     => (Q34, sym,      Right)
       | (Q34, SOME One) => (Q34, sym,      Right)
       | (Q34, NONE)     => (Q35, sym,      Right)
       | (Q35, SOME One) => (Q34, SOME One, Here)
       | (Q35, NONE)     => (Q36, sym,      Left)
       | (Q36, NONE)     => (Q17, sym,      Left)
       | (Q40, SOME One) => (Q40, sym,      Left)
       | (Q40, NONE)     => (Q41, sym,      Left)
       | (Q41, NONE)     => (Q65, NONE,     Here)
       | (Q65, NONE)     => (Q65, sym,      Right)
       | (Q65, SOME One) => (Q66, SOME One, Here)
       | (Q41, SOME One) => (Q42, SOME One, Here)
       | (Q42, SOME One) => (Q42, sym,      Left)
       | (Q42, NONE)     => (Q43, sym,      Left)
       | (Q43, SOME One) => (Q42, SOME One, Here)
       | (Q43, NONE)     => (Q44, SOME One, Here)
       | (Q44, SOME One) => (Q44, sym,      Right)
       | (Q44, NONE)     => (Q45, sym,      Right)
       | (Q45, SOME One) => (Q44, SOME One, Here)
       | (Q45, NONE)     => (Q46, sym,      Left)
       | (Q46, NONE)     => (Q46, sym,      Left)
       | (Q46, SOME One) => (Q47, sym,      Left)
       | (Q47, NONE)     => (Q61, sym,      Right)
       | (Q61, SOME One) => (Q62, NONE,     Here)
       | (Q62, NONE)     => (Q62, sym,      Left)
       | (Q62, SOME One) => (Q63, NONE,     Here)
       | (Q63, NONE)     => (Q64, sym,      Left)
       | (Q64, SOME One) => (Q62, SOME One, Here)
       | (Q64, NONE)     => (Q40, sym,      Left)
       | (Q47, SOME One) => (Q48, sym,      Right)
       | (Q48, SOME One) => (Q48, NONE,     Here)
       | (Q48, NONE)     => (Q49, sym,      Left)
       | (Q49, SOME One) => (Q49, sym,      Left)
       | (Q49, NONE)     => (Q50, sym,      Left)
       | (Q50, SOME One) => (Q50, NONE,     Here)
       | (Q50, NONE)     => (Q51, sym,      Left)
       | (Q51, SOME One) => (Q51, sym,      Left)
       | (Q51, NONE)     => (Q52, sym,      Left)
       | (Q52, SOME One) => (Q51, SOME One, Here)
       | (Q52, NONE)     => (Q67, sym,      Right)
       | (Q67, NONE)     => (Q53, SOME One, Here)
       | (Q53, SOME One) => (Q53, sym,      Right)
       | (Q53, NONE)     => (Q54, sym,      Right)
       | (Q54, SOME One) => (Q53, SOME One, Here)
       | (Q54, NONE)     => (Q55, sym,      Left)
       | (Q55, NONE)     => (Q55, sym,      Left)
       | (Q55, SOME One) => (Q56, sym,      Left)
       | (Q56, SOME One) => (Q50, sym,      Right)
       | (Q56, NONE)     => (Q57, sym,      Right)
       | (Q57, SOME One) => (Q58, sym,      Right)
       | (Q58, NONE)     => (Q57, SOME One, Here)
       | (Q58, SOME One) => (Q59, sym,      Left)
       | (Q59, SOME One) => (Q59, NONE,     Here)
       | (Q59, NONE)     => (Q60, sym,      Right)
       | (Q60, SOME One) => (Q60, sym,      Right)
       | (Q60, NONE)     => (Q46, sym,      Left)
       | _ => raise ExecError "Bad state"

  fun init [n, m] =
        let
          val n1 = List.tabulate (n + 1, fn _ => SOME One)
          val m1 = List.tabulate (m + 1, fn _ => SOME One)
        in
          Vector.fromList (n1 @ [NONE] @ m1)
        end
    | init _ = raise Fail "Needs n and m"

  fun interpret tape =
    let
      val tapeList = vectorToList tape
      val isOne = fn (SOME One) => true | _ => false
      val count = List.length $ List.filter isOne tapeList
    in
      count - 1
    end

  fun toSymbol #" " = NONE
    | toSymbol #"1" = SOME One
    | toSymbol c = raise ParseError $ "Unknown char: " ^ (Char.toString c)

  fun decode input = Vector.fromList $ map toSymbol (String.explode input)

  fun fromSymbol NONE = #" "
    | fromSymbol (SOME One) = #"1"

  fun encode tape =
    String.implode o vectorToList $ (Vector.map fromSymbol tape)

  (* ----- Pretty printing functions ----- *)
  fun stateToString q = case q
   of  Q1 => " Q1" |  Q2 => " Q2" |  Q3 => " Q3" |  Q4 => " Q4" |  Q5 => " Q5"
    |  Q6 => " Q6" |  Q7 => " Q7" |  Q8 => " Q8" |  Q9 => " Q9" | Q10 => "Q10"
    | Q11 => "Q11" | Q12 => "Q12" | Q13 => "Q13" | Q14 => "Q14" | Q15 => "Q15"
    | Q16 => "Q16" | Q17 => "Q17" | Q18 => "Q18" | Q19 => "Q19" | Q20 => "Q20"
    | Q21 => "Q21" | Q22 => "Q22" | Q23 => "Q23" | Q24 => "Q24" | Q25 => "Q25"
    | Q26 => "Q26" | Q27 => "Q27" | Q28 => "Q28" | Q29 => "Q29" | Q30 => "Q30"
    | Q31 => "Q31" | Q32 => "Q32" | Q33 => "Q33" | Q34 => "Q34" | Q35 => "Q35"
    | Q36 => "Q36" | Q37 => "Q37" | Q38 => "Q38" | Q39 => "Q39" | Q40 => "Q40"
    | Q41 => "Q41" | Q42 => "Q42" | Q43 => "Q43" | Q44 => "Q44" | Q45 => "Q45"
    | Q46 => "Q46" | Q47 => "Q47" | Q48 => "Q48" | Q49 => "Q49" | Q50 => "Q50"
    | Q51 => "Q51" | Q52 => "Q52" | Q53 => "Q53" | Q54 => "Q54" | Q55 => "Q55"
    | Q56 => "Q56" | Q57 => "Q57" | Q58 => "Q58" | Q59 => "Q59" | Q60 => "Q60"
    | Q61 => "Q61" | Q62 => "Q62" | Q63 => "Q63" | Q64 => "Q64" | Q65 => "Q65"
    | Q66 => "Q66" | Q67 => "Q67"

  fun leftpad n = String.implode (List.tabulate (n, fn _ => #" "))

  fun traceLineToString (tape, pos, q) =
    let
      val stateStr = "[" ^ (stateToString q) ^ "] "
      val n = String.size stateStr
      val tapeStr = encode tape
      val headStr = leftpad (n + pos) ^ "^"
    in
      stateStr ^ tapeStr ^ "\n" ^ headStr
    end

  fun traceToString trace =
    String.concatWith "\n" $ List.map traceLineToString trace


end

