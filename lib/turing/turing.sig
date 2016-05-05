signature TURING =
sig
  (* Tokens that this machine can use, not including the blank symbol *)
  type token

  (* Tape alphabet, including all the tokens as well as a blank symbol: NONE *)
  type symbol = token option

  (* A tape is just a vector of tape symbols. We treat this as a two-way
   * unbounded array (unbounded on both ends), a convention which is enforced
   * by the Turing machine simulator, not here by the Turing machine. *)
  type tape = symbol Vector.vector

  (* Each Turing machine can have it's own set of states that it uses. *)
  type state

  (* Can be raised by the delta function if a transition isn't defined *)
  exception ExecError of string
  (* Available for the various encoding/decoding functions to raise *)
  exception ParseError of string

  (* Represents moving the tape head to the left, right, or current cell *)
  datatype move = Left | Here | Right

  (* Functions for checking initial and final conditions *)
  val initial : unit -> state
  val isFinal : state -> bool

  (* A transition function reads a state and a symbol, then both moves and
   * writes a new symbol in one step, while moving to the next state. *)
  val delta : (state * symbol) -> (state * symbol * move)

  (* Given something like [n, m], constructs a tape to represent this machine's
   * initial conditions. *)
  val init : int list -> tape
  (* Given a tape that represents terminal conditions for this turing machine,
   * gets the "return value" of the Turing machine's computation. *)
  val interpret : tape -> int


  (* Convert between tapes and strings, good for pretty printing. *)
  val decode : string -> tape
  val encode : tape -> string

  (* Pretty printing functions for printing traces *)
  val traceLineToString : (tape * int * state) -> string
  val traceToString : (tape * int * state) list -> string
end
