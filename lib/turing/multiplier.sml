structure Multiplier : TURING =
struct
  open Prelude
  infixr 0 $

  datatype token = Hash | One | Star | Times
  type symbol = token option
  type tape = symbol Vector.vector

  datatype state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7

  exception ExecError of string
  exception ParseError of string

  datatype move = Left | Here | Right

  fun initial () = Q0
  fun isFinal q = q = Q7

  fun delta (q, sym) =
    case (q, sym)
      of (Q0, SOME Hash)  => (Q0, SOME Hash,  Right)
       | (Q0, SOME One)   => (Q1, NONE,       Right)
       | (Q0, SOME Star)  => (Q6, SOME Star,  Right)

       | (Q1, SOME One)   => (Q1, SOME One,   Right)
       | (Q1, SOME Star)  => (Q2, SOME Star,  Right)

       | (Q2, SOME One)   => (Q3, SOME Times, Right)
       | (Q2, SOME Hash)  => (Q5, SOME Hash,  Left)

       | (Q3, SOME One)   => (Q3, SOME One,   Right)
       | (Q3, SOME Hash)  => (Q3, SOME Hash,  Right)
       | (Q3, NONE)       => (Q4, SOME One,   Left)

       | (Q4, SOME Hash)  => (Q4, SOME Hash,  Left)
       | (Q4, SOME One)   => (Q4, SOME One,   Left)
       | (Q4, SOME Times) => (Q2, SOME Times, Right)

       | (Q5, SOME Times) => (Q5, SOME One,   Left)
       | (Q5, SOME Star)  => (Q5, SOME Star,  Left)
       | (Q5, SOME One)   => (Q5, SOME One,   Left)
       | (Q5, NONE)       => (Q0, NONE,       Right)

       | (Q6, SOME Hash)  => (Q7, SOME Hash,  Here)
       | (Q6, _)          => (Q6, NONE,       Right)

       | _ => raise ExecError "Bad state"

  fun init [n, m] =
        let
          val n0 = List.tabulate (n, fn _ => SOME One)
          val m0 = List.tabulate (m, fn _ => SOME One)
        in
          Vector.fromList ([SOME Hash] @ n0 @ [SOME Star] @ m0 @ [SOME Hash])
        end
    | init _ = raise Fail "Needs n and m"

  fun interpret tape =
    let
      val tapeList = vectorToList tape
      val isOne = fn (SOME One) => true | _ => false
    in
        List.length $ List.filter isOne tapeList
    end

  fun toSymbol #" " = NONE
    | toSymbol #"#" = SOME Hash
    | toSymbol #"1" = SOME One
    | toSymbol #"*" = SOME Star
    | toSymbol #"x" = SOME Times
    | toSymbol c = raise ParseError $ "Unknown char: " ^ (Char.toString c)

  fun decode input = Vector.fromList $ map toSymbol (String.explode input)

  fun fromSymbol NONE         = #" "
    | fromSymbol (SOME Hash)  = #"#"
    | fromSymbol (SOME One)   = #"1"
    | fromSymbol (SOME Star)  = #"*"
    | fromSymbol (SOME Times) = #"x"

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
    | stateToString Q7 = "Q7"

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
