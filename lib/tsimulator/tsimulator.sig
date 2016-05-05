signature TSIMULATOR =
sig
  (* The machine that this simulator simulates. *)
  structure M : TURING

  datatype d = Final | Step of (M.tape * int * M.state)

  (**
   * Either:
   * - take a single step to a new tape, tape head, and state, or
   * - say that the original state was final
   *
   * May raise ExecError if the underlying call to M.delta did.
   *)
  val trystep : (M.tape * int * M.state) -> d

  (**
   * Evaluate this tape completely according to this machine. Keeps track of
   * both:
   * - a trace of the computation (a list of the tapes, tape heads, and states)
   * - the final tape (convenience, so you don't have to take an O(n) operation
   *   to get the last element of a list)
   *)
  val eval : M.tape -> ((M.tape * int * M.state) list * M.tape)
end
