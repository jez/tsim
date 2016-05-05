structure Main =
struct
  open Prelude
  infixr 0 $

  structure AdderSim = TSimulatorFn (Adder)
  structure MulSim = TSimulatorFn (Multiplier)
  structure ExpSim = TSimulatorFn (Exponentiator)

  val getNumber = Option.valOf o Int.fromString
  fun main args =
    if List.length args <> 3
    then println $ "usage: " ^ (CommandLine.name ()) ^ " (add|mult|exp) <n> <m>"
    else
      let
        val command::numbers = args
      in
        case command
          of "add" =>
               let
                 val tape = AdderSim.M.init (List.map getNumber numbers)
                 val (trace, final) = AdderSim.eval tape
               in
                 println $ AdderSim.M.traceToString trace;
                 println $ "Answer: " ^ Int.toString (AdderSim.M.interpret final)
               end
           | "mult" =>
               let
                 val tape = MulSim.M.init (List.map getNumber numbers)
                 val (trace, final) = MulSim.eval tape
               in
                 println $ MulSim.M.traceToString trace;
                 println $ "Answer: " ^ Int.toString (MulSim.M.interpret final)
               end
           | "exp" =>
               let
                 val tape = ExpSim.M.init (List.map getNumber numbers)
                 val (trace, final) = ExpSim.eval tape
               in
                 println $ ExpSim.M.traceToString trace;
                 println $ "Answer: " ^ Int.toString (ExpSim.M.interpret final)
               end
           | _ => raise Fail "Unimplemented"
      end

  val main = fn () => makeMain main ()

end
