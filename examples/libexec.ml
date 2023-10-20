module V = Stdint.Uint64

type register = Rax | Rdx | Rdi | Tmp0
type operator = Plus | Minus

type expression =
  | Cst of V.t
  | Reg of register
  | Op of operator * expression * expression

module R = Map.Make (struct
  type t = register

  let compare = compare
end)

type instruction =
  | SetR of register * expression * instruction
  | SetZ of expression * instruction
  | IfZ of instruction * instruction
  | Ret of register
  | Builtin of (V.t R.t -> bool -> instruction -> V.t) * instruction

type code = Fun of instruction

let rec eval regs = function
  | Cst v -> v
  | Reg r -> R.find r regs
  | Op (Plus, x, y) -> V.add (eval regs x) (eval regs y)
  | Op (Minus, x, y) -> V.sub (eval regs x) (eval regs y)

let rec step regs z = function
  | SetR (r, e, i) -> step (R.add r (eval regs e) regs) z i
  | SetZ (e, i) -> step regs V.(compare (eval regs e) zero = 0) i
  | IfZ (i, i') -> if z then step regs z i else step regs z i'
  | Ret r -> R.find r regs
  | Builtin (f, i) -> f regs z i

let run rdi (Fun i) = step (R.singleton Rdi rdi) false i
