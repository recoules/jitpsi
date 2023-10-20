open Libexec

module V0 = struct
  let rec fibonacci = Fun i0
  and i0 = SetR (Rax, Cst V.zero, i1)
  and i1 = SetZ (Reg Rdi, i2)
  and i2 = IfZ (ib, i3)
  and i3 = SetR (Rdx, Cst V.one, i4)
  and i4 = SetR (Tmp0, Reg Rdx, i5)
  and i5 = SetR (Rdx, Reg Rax, i6)
  and i6 = SetR (Rax, Reg Tmp0, i7)
  and o1 = Op (Plus, Reg Rax, Reg Rdx)
  and i7 = SetR (Rax, o1, i8)
  and o2 = Op (Minus, Reg Rdi, Cst V.one)
  and i8 = SetR (Rdi, o2, i9)
  and i9 = SetZ (Reg Rdi, ia)
  and ia = IfZ (ib, i4)
  and ib = Ret Rax
end

module V1 = struct
  let rec fibonacci = Fun i0
  and i0 = SetR (Rax, Cst V.zero, i1)
  and i1 = SetZ (Reg Rdi, i2)
  and i2 = IfZ (i6, i3)
  and i3 = SetR (Rdx, Cst V.one, i4)

  and loop r _ i =
    let v0 = R.find Rdx r in
    let r0 = R.add Tmp0 v0 r in
    let v1 = R.find Rax r0 in
    let r1 = R.add Rdx v1 r0 in
    let v2 = R.find Tmp0 r1 in
    let r2 = R.add Rax v2 r1 in
    let v3 = R.find Rax r2 in
    let v4 = R.find Rdx r2 in
    let v5 = V.add v3 v4 in
    let r3 = R.add Rax v5 r2 in
    let v6 = R.find Rdi r3 in
    let v7 = V.one in
    let v8 = V.sub v6 v7 in
    let r4 = R.add Rdi v8 r3 in
    let v9 = R.find Rdi r4 in
    let va = V.zero in
    let z0 = V.compare v9 va = 0 in
    step r4 z0 i

  and i4 = Builtin (loop, i5)
  and i5 = IfZ (i6, i4)
  and i6 = Ret Rax
end

module V2 = struct
  let rec fibonacci = Fun i0
  and i0 = SetR (Rax, Cst V.zero, i1)
  and i1 = SetZ (Reg Rdi, i2)
  and i2 = IfZ (i6, i3)
  and i3 = SetR (Rdx, Cst V.one, i4)

  and loop r _ i =
    let v0 = V.zero in
    let v1 = V.one in
    let v2 = R.find Rdx r in
    let v3 = R.find Rax r in
    let v4 = R.find Rdi r in
    let v5 = V.add v2 v3 in
    let v6 = V.sub v4 v1 in
    let r0 = R.add Rdx v3 r in
    let r1 = R.add Rax v5 r0 in
    let r2 = R.add Rdi v6 r1 in
    let z0 = V.compare v6 v0 = 0 in
    step r2 z0 i

  and i4 = Builtin (loop, i5)
  and i5 = IfZ (i6, i4)
  and i6 = Ret Rax
end

module V3 () = struct
  let rec fibonacci = Fun i0
  and i0 = SetR (Rax, Cst V.zero, i1)
  and i1 = SetZ (Reg Rdi, i2)
  and i2 = IfZ (i6, i3)
  and i3 = SetR (Rdx, Cst V.one, i4)
  and loop = Libopt.compile V0.[ i4; i5; i6; i7; i8; i9 ]
  and i4 = Builtin (loop, i5)
  and i5 = IfZ (i6, i4)
  and i6 = Ret Rax
end

type optim = O0 | O1 | O2 | Oj
type perf = No | Warmup | Exec | Full

let run perf opt n =
  let t0 = Landmark.clock () in
  let fibonacci =
    match opt with
    | O0 -> V0.fibonacci
    | O1 -> V1.fibonacci
    | O2 -> V2.fibonacci
    | Oj ->
        let module J = V3 () in
        J.fibonacci
  in
  let t1 = Landmark.clock () in
  let r = run n fibonacci in
  let t2 = Landmark.clock () in
  Format.printf "%s@." (V.to_string r);
  let printer =
    match perf with
    | No -> ignore
    | Warmup | Exec | Full -> Format.eprintf "%s cycles@."
  in
  let cycles =
    match perf with
    | No -> 0L
    | Warmup -> Int64.sub t1 t0
    | Exec -> Int64.sub t2 t1
    | Full -> Int64.sub t2 t0
  in
  printer (Int64.to_string cycles)

open Cmdliner

let vconv : V.t option Arg.conv =
  ( (fun str ->
      try `Ok (Some (V.of_string str))
      with _ -> `Error "not a positive integer"),
    Format.pp_print_option (fun ppf v ->
        Format.pp_print_string ppf (V.to_string v)) )

let n =
  Arg.(
    required & pos 0 vconv None
    & info [] ~docv:"N" ~doc:"Index in the Fibonacci sequence")

let optim =
  Arg.(
    value
    & opt (enum [ ("0", O0); ("1", O1); ("2", O2); ("j", Oj) ]) O0
    & info [ "O" ] ~docv:"LEVEL" ~doc:"Level of optimization [ 0, 1, 2, j ]")

let perf =
  Arg.(
    value
    & opt ~vopt:Exec
        (enum
           [ ("full", Full); ("warmup", Warmup); ("exec", Exec); ("no", No) ])
        No
    & info [ "p"; "perf" ] ~docv:"SELECT"
        ~doc:"Performance measurement [ exec, warmup, full ]")

let run_t = Term.(const run $ perf $ optim $ n)
let cmd = Cmd.(v (info "fibonacci") run_t)
let () = exit (Cmd.eval cmd)
