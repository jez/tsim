functor TSimulatorFn (M : TURING) : TSIMULATOR =
struct
  open Prelude
  infixr 0 $

  structure M = M

  datatype d = Final | Step of (M.tape * int * M.state)

  fun moveFrom pos M.Left  = pos - 1
    | moveFrom pos M.Here  = pos
    | moveFrom pos M.Right = pos + 1

  (**
   * One invariant we maintain in @step@ is that the read head is never out of
   * bounds of the current tap, in either direction. If this is not the case, we
   * append a bunch of blank symbols.
   *)
  fun expandTape tape pos =
    let val n = Vector.length tape in
      if pos = n
      then
        let val blanks = Vector.tabulate (n, fn _ => NONE) in
          (pos, Vector.concat [tape, blanks])
        end
      else if pos = ~1
      then
        let val blanks = Vector.tabulate (n, fn _ => NONE) in
          (n - pos, Vector.concat [blanks, tape])
        end
      else (pos, tape)
    end

  fun trystep (tape, pos, q) =
    if M.isFinal q then Final
    else
      let
        val sym = Vector.sub (tape, pos)
        val (q', sym', move) = M.delta (q, sym)
        val tape' = Vector.update (tape, pos, sym')
        val pos' = moveFrom pos move
        val (pos', tape') = expandTape tape' pos'
      in
        Step (tape', pos', q')
      end

  fun tracer (tape, pos, q) =
    case trystep (tape, pos, q)
      of Final => ([(tape, pos, q)], tape)
       | Step (tape', pos', q') =>
           let
             val (trace, final) = tracer (tape', pos', q')
           in
             ((tape, pos, q) :: trace, final)
           end

  fun eval tape = tracer (tape, 0, M.initial ())

end
