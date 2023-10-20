open Libexec

let is_zero v = V.compare v V.zero = 0

let c_add = Jitpsi.const V.add
and c_sub = Jitpsi.const V.sub
and c_is_zero = Jitpsi.const is_zero
and c_assign = Jitpsi.const R.add
and c_lookup = Jitpsi.const R.find
and c_step = Jitpsi.const step

let c_rax = Jitpsi.const Rax
and c_rdx = Jitpsi.const Rdx
and c_rdi = Jitpsi.const Rdi
and c_tmp0 = Jitpsi.const Tmp0

let c_register = function
  | Rax -> c_rax
  | Rdx -> c_rdx
  | Rdi -> c_rdi
  | Tmp0 -> c_tmp0

let c_operator = function Plus -> c_add | Minus -> c_sub

module C = Hashtbl.Make (struct
  type t = expression

  let equal = ( = )
  let hash = Hashtbl.hash
end)

let rec fold e env =
  match e with
  | Cst _ -> e
  | Reg r -> ( try R.find r env with Not_found -> e)
  | Op (f, x, y) -> Op (f, fold x env, fold y env)

let hcons c e e' =
  C.add c e e';
  e'

let rec c_eval c regs e =
  try C.find c e
  with Not_found -> (
    match e with
    | Cst v -> hcons c e (Jitpsi.const v)
    | Reg r -> hcons c e (Jitpsi.apply2 c_lookup (c_register r) regs)
    | Op (op, x, y) ->
        hcons c e
          (Jitpsi.apply2 (c_operator op) (c_eval c regs x) (c_eval c regs y)))

let compile il =
  let env, zf =
    List.fold_left
      (fun (env, z) -> function
        | SetR (r, e, _) -> (R.add r (fold e env) env, z)
        | SetZ (e, _) -> (env, Some (fold e env))
        | IfZ _ | Ret _ | Builtin _ -> failwith "unexpected instruction kind")
      (R.empty, None) il
  in
  let env = R.remove Tmp0 env in
  Jitpsi.return
    (Jitpsi.lambda3 (fun regs z i ->
         let c_eval = c_eval (C.create 0) regs in
         let regs =
           R.fold
             (fun r e regs ->
               Jitpsi.apply3 c_assign (c_register r) (c_eval e) regs)
             env regs
         in
         let z =
           match zf with
           | None -> z
           | Some e -> Jitpsi.apply c_is_zero (c_eval e)
         in
         Jitpsi.apply3 c_step regs z i))
