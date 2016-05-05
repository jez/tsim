structure Adder : TURING =
struct
  open Prelude
  infixr 0 $

  datatype token = One
  type symbol = token option
  type tape = symbol Vector.vector

  datatype state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6

  exception ExecError of string
  exception ParseError of string

  datatype move = Left | Here | Right

  fun initial () = Q0
  fun isFinal q = q = Q6

  fun delta (q, sym) =
    case (q, sym)
      of (Q0, NONE)     => (Q1, NONE,     Right)
       | (Q1, NONE)     => (Q2, SOME One, Right)
       | (Q1, SOME One) => (Q1, SOME One, Right)
       | (Q2, NONE)     => (Q3, NONE,     Left)
       | (Q2, SOME One) => (Q2, SOME One, Right)
       | (Q3, SOME One) => (Q4, NONE,     Left)
       | (Q4, SOME One) => (Q5, NONE,     Left)
       | (Q5, NONE)     => (Q6, NONE,     Here)
       | (Q5, SOME One) => (Q5, SOME One, Left)
       | (Q0, SOME One) => raise ExecError "Q0 must read a blank"
       | (Q3, NONE)     => raise ExecError "Q3 must read a 1"
       | (Q4, NONE)     => raise ExecError "Q4 must read a 1"
       | (Q6, _)        => raise ExecError "Stepped halting state"

  fun init [n, m] =
        let
          val n1 = List.tabulate (n + 1, fn _ => SOME One)
          val m1 = List.tabulate (m + 1, fn _ => SOME One)
        in
          Vector.fromList ([NONE] @ n1 @ [NONE] @ m1)
        end
    | init _ = raise Fail "Needs n and m"

  fun interpret tape =
    let
      val tapeList = vectorToList tape
      val count =
        List.length $ List.filter (fn (SOME One) => true | _ => false) tapeList
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
  fun stateToString Q0 = "Q0"
    | stateToString Q1 = "Q1"
    | stateToString Q2 = "Q2"
    | stateToString Q3 = "Q3"
    | stateToString Q4 = "Q4"
    | stateToString Q5 = "Q5"
    | stateToString Q6 = "Q6"

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

