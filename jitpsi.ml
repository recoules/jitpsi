external global_symbol : string -> Int64.t = "ocaml_jitpsi_global_symbol"

let caml_currymax, caml_applymax =
  let rec continue prim n =
    let n' = n + 1 in
    if Int64.equal (global_symbol (prim n')) Int64.zero then n
    else continue prim n'
  in
  let max prim =
    assert (not (Int64.equal (global_symbol (prim 2)) Int64.zero));
    continue prim 2
  in
  (max (Printf.sprintf "caml_curry%d"), max (Printf.sprintf "caml_apply%d"))

type value
and lambda

type (+'a, 'b) t =
  | Lambda : {
      env : Obj.t Ident.Map.t;
      locals : Lambda.lambda Ident.Map.t;
      revlet : Ident.t list;
      params : (Ident.t * Lambda.value_kind) list;
      arity : int;
    }
      -> ('a, lambda) t
  | Value : {
      env : Obj.t Ident.Map.t;
      locals : Lambda.lambda Ident.Map.t;
      revlet : Ident.t list;
    }
      -> ('a, value) t
  | Apply : {
      env : Obj.t Ident.Map.t;
      locals : Lambda.lambda Ident.Map.t;
      revlet : Ident.t list;
      closure : Lambda.lambda;
      revargs : Lambda.lambda list;
      arity : int;
    }
      -> ('a, value) t

let keygen =
  let n = ref (-1) in
  let to_string n =
    let b = Bytes.create ((Sys.int_size + 1) lsr 3) in
    Bytes.set_uint16_le b 0 n;
    Bytes.set_uint16_le b 2 (n lsr 16);
    if Sys.int_size > 32 then (
      Bytes.set_uint16_le b 4 (n lsr 32);
      Bytes.set_uint16_le b 6 (n lsr 48));
    Bytes.unsafe_to_string b
  in
  fun () ->
    incr n;
    to_string !n

let append =
  let rec rev_filter r env = function
    | [] -> r
    | x :: l -> rev_filter (if Ident.Map.mem x env then r else x :: r) env l
  in
  fun revlet1 revlet0 locals0 ->
    List.rev_append (rev_filter [] locals0 revlet1) revlet0

let const (a : 'a) : ('a, value) t =
  let label = Ident.create_local (keygen ()) in
  if Obj.(is_int (repr a)) then
    Value
      {
        env = Ident.Map.empty;
        locals =
          Ident.Map.singleton label Lambda.(Lconst (const_int (Obj.magic a)));
        revlet = [ label ];
      }
  else
    Value
      {
        env = Ident.Map.singleton label (Obj.repr a);
        locals = Ident.Map.singleton label (Lambda.Lvar label);
        revlet = [ label ];
      }

let sequence (v0 : ('a, value) t) (v1 : ('b, value) t) : ('b, value) t =
  match (v0, v1) with
  | ( ( Value { env = env0; locals = locals0; revlet = revlet0 }
      | Apply { env = env0; locals = locals0; revlet = revlet0; _ } ),
      Value { env = env1; locals = locals1; revlet = revlet1 } ) ->
      let env = Ident.Map.union_left env0 env1
      and locals = Ident.Map.union_left locals0 locals1
      and revlet = append revlet1 revlet0 locals0 in
      Value { env; locals; revlet }
  | ( ( Value { env = env0; locals = locals0; revlet = revlet0 }
      | Apply { env = env0; locals = locals0; revlet = revlet0; _ } ),
      Apply
        {
          env = env1;
          locals = locals1;
          revlet = revlet1;
          closure;
          revargs;
          arity;
        } ) ->
      let env = Ident.Map.union_left env0 env1
      and locals = Ident.Map.union_left locals0 locals1
      and revlet = append revlet1 revlet0 locals0 in
      Apply { env; locals; revlet; closure; revargs; arity }

let apply =
  let apply1 env0 locals0 revlet0 env1 locals1 revlet1 =
    let label = Ident.create_local (keygen ())
    and closure = Lambda.Lvar (List.hd revlet0) in
    let env = Ident.Map.union_left env0 env1
    and locals = Ident.Map.union_left locals0 locals1
    and revlet = label :: append revlet1 revlet0 locals0 in
    let revargs = [ Lambda.Lvar (List.hd revlet1) ] in
    let lambda_apply =
      Lambda.(
        Lapply
          {
            ap_func = closure;
            ap_args = revargs;
            ap_loc = Loc_unknown;
            ap_tailcall = Default_tailcall;
            ap_inlined = Default_inline;
            ap_specialised = Default_specialise;
          })
    in
    let locals = Ident.Map.add label lambda_apply locals in
    Apply { env; locals; revlet; closure; revargs; arity = 1 }
  in
  fun (v0 : ('a -> 'b, value) t) (v1 : ('a, value) t) : ('b, value) t ->
    match (v0, v1) with
    | ( Value { env = env0; locals = locals0; revlet = revlet0 },
        ( Value { env = env1; locals = locals1; revlet = revlet1 }
        | Apply { env = env1; locals = locals1; revlet = revlet1; _ } ) ) ->
        apply1 env0 locals0 revlet0 env1 locals1 revlet1
    | ( Apply
          {
            env = env0;
            locals = locals0;
            revlet = revlet0;
            closure;
            revargs;
            arity;
          },
        ( Value { env = env1; locals = locals1; revlet = revlet1 }
        | Apply { env = env1; locals = locals1; revlet = revlet1; _ } ) ) ->
        if arity = caml_applymax then
          apply1 env0 locals0 revlet0 env1 locals1 revlet1
        else
          let label = Ident.create_local (keygen ()) in
          let env = Ident.Map.union_left env0 env1
          and locals = Ident.Map.union_left locals0 locals1
          and revlet = label :: append revlet1 (List.tl revlet0) locals0
          and revargs = Lambda.Lvar (List.hd revlet1) :: revargs in
          let lambda_apply =
            Lambda.(
              Lapply
                {
                  ap_func = closure;
                  ap_args = List.rev revargs;
                  ap_loc = Loc_unknown;
                  ap_tailcall = Default_tailcall;
                  ap_inlined = Default_inline;
                  ap_specialised = Default_specialise;
                })
          in
          let locals = Ident.Map.add label lambda_apply locals in
          Apply { env; locals; revlet; closure; revargs; arity = arity + 1 }

let pair (v0 : ('a, value) t) (v1 : ('b, value) t) : ('a * 'b, value) t =
  match (v0, v1) with
  | ( ( Value { env = env0; locals = locals0; revlet = revlet0 }
      | Apply { env = env0; locals = locals0; revlet = revlet0; _ } ),
      ( Value { env = env1; locals = locals1; revlet = revlet1 }
      | Apply { env = env1; locals = locals1; revlet = revlet1; _ } ) ) ->
      let env = Ident.Map.union_left env0 env1
      and locals = Ident.Map.union_left locals0 locals1
      and revlet = append revlet1 revlet0 locals0 in
      let label = Ident.create_local (keygen ()) in
      let locals =
        Ident.Map.add label
          (Lambda.Lprim
             ( Pmakeblock (0, Immutable, None),
               [ Lambda.Lvar (List.hd revlet0); Lambda.Lvar (List.hd revlet1) ],
               Loc_unknown ))
          locals
      and revlet = label :: revlet in
      Value { env; locals; revlet }

let field i v =
  let (Value { env; locals; revlet } | Apply { env; locals; revlet; _ }) = v in
  let label = Ident.create_local (keygen ()) in
  let locals =
    Ident.Map.add label
      (Lambda.Lprim (Pfield i, [ Lambda.Lvar (List.hd revlet) ], Loc_unknown))
      locals
  and revlet = label :: revlet in
  Value { env; locals; revlet }

let fst : ('a * 'b, value) t -> ('a, value) t = fun v -> field 0 v
let snd : ('a * 'b, value) t -> ('b, value) t = fun v -> field 1 v

let lambda : type c. (('a, value) t -> ('b, c) t) -> ('a -> 'b, lambda) t =
 fun body ->
  let label = Ident.create_local (keygen ()) in
  let arg =
    Value
      {
        env = Ident.Map.empty;
        locals = Ident.Map.singleton label (Lambda.Lvar label);
        revlet = [ label ];
      }
  in
  match body arg with
  | Value { env; locals; revlet } | Apply { env; locals; revlet; _ } ->
      Lambda { env; locals; revlet; params = [ (label, Pgenval) ]; arity = 1 }
  | Lambda { env; locals; revlet; params; arity } ->
      if arity = caml_currymax then
        raise (Failure "[lambda] too many parameters");
      Lambda
        {
          env;
          locals;
          revlet;
          params = (label, Pgenval) :: params;
          arity = arity + 1;
        }

let closure_arity obj =
  let info : int = Obj.magic (Obj.field obj 1) in
  info lsr (Sys.int_size - 8)

let closure_closed obj =
  Obj.tag obj = Obj.closure_tag
  &&
  let size = Obj.size obj in
  size <= 3
  &&
  let info : int = Obj.magic (Obj.field obj 1) in
  info lsr (Sys.int_size - 8) <> 1 || size = 2

let init_env witness env lambda =
  let n = Ident.Map.cardinal env in
  let binding = Array.make n (Obj.repr 0) in
  let approx = Array.make n Clambda.Value_unknown in
  let global =
    Lambda.Lprim (Pgetglobal (Ident.create_persistent "Jitpsi"), [], Loc_unknown)
  in
  let pos = ref (-1) in
  let lambda =
    Ident.Map.fold
      (fun label value lambda ->
        incr pos;
        Array.set binding !pos value;
        if
          let tag = Obj.tag value in
          tag = Obj.closure_tag || tag = Obj.infix_tag
        then
          Array.set approx !pos
            (Value_closure
               ( {
                   fun_label = Printf.sprintf "jitpsi__dcall_%d" !pos;
                   fun_arity = closure_arity value;
                   fun_closed = closure_closed value;
                   fun_inline = None;
                   fun_float_const_prop = false;
                   fun_poll = Default_poll;
                 },
                 Value_unknown ));
        Lambda.(
          Llet
            ( Strict,
              Pgenval,
              label,
              Lprim (Pfield !pos, [ global ], Loc_unknown),
              lambda )))
      env lambda
  in
  Compilenv.cache_unit_info
    Cmx_format.
      {
        ui_name = "Jitpsi";
        ui_symbol = "";
        ui_defines = [];
        ui_imports_cmi = [];
        ui_imports_cmx = [];
        ui_curry_fun = [];
        ui_apply_fun = [];
        ui_send_fun = [];
        ui_export_info = Clambda (Value_tuple approx);
        ui_force_link = false;
      };
  ( binding,
    Lambda.Llet
      ( Strict,
        Pgenval,
        witness,
        Lambda.Lprim
          ( Pccall (Primitive.simple ~name:"jitpsi__root" ~arity:0 ~alloc:false),
            [],
            Loc_unknown ),
        lambda ) )

let rec make_body locals revlet lambda =
  match revlet with
  | [] -> lambda
  | label :: revlet ->
      make_body locals revlet
        Lambda.(
          Llet (Strict, Pgenval, label, Ident.Map.find label locals, lambda))

let return (Lambda { env; locals; revlet; params; _ } : ('a, lambda) t) : 'a =
  let label = Ident.create_local (keygen ()) in
  let root = Ident.create_local "jitpsi__root" in
  let binding, code =
    init_env root env
      Lambda.(
        lfunction ~kind:Curried ~params ~return:Pgenval
          ~body:
            (Lsequence
               ( Lprim (Pignore, [ Lvar root ], Loc_unknown),
                 make_body locals (List.tl revlet)
                   (Ident.Map.find (List.hd revlet) locals) ))
          ~attr:
            {
              inline = Never_inline;
              specialise = Never_specialise;
              local = Never_local;
              poll = Default_poll;
              is_a_functor = false;
              stub = false;
              tmc_candidate = false;
            }
          ~loc:Loc_unknown)
  in
  let lambda_program =
    Lambda.
      {
        module_ident = label;
        main_module_block_size = 0;
        required_globals = Ident.Set.empty;
        code;
      }
  in
  Loader.assemble_and_link binding lambda_program

let ( @@ ) : (unit, value) t -> ('b, value) t -> ('b, value) t = sequence

let apply2 (f : ('a -> 'b -> 'c, value) t) (a : ('a, value) t)
    (b : ('b, value) t) : ('c, value) t =
  apply (apply f a) b

let apply3 (f : ('a -> 'b -> 'c -> 'd, value) t) (a : ('a, value) t)
    (b : ('b, value) t) (c : ('c, value) t) : ('d, value) t =
  apply (apply (apply f a) b) c

let apply4 (f : ('a -> 'b -> 'c -> 'd -> 'e, value) t) (a : ('a, value) t)
    (b : ('b, value) t) (c : ('c, value) t) (d : ('d, value) t) : ('e, value) t
    =
  apply (apply (apply (apply f a) b) c) d

let apply5 (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f, value) t) (a : ('a, value) t)
    (b : ('b, value) t) (c : ('c, value) t) (d : ('d, value) t)
    (e : ('e, value) t) : ('f, value) t =
  apply (apply (apply (apply (apply f a) b) c) d) e

let lambda2 (body : ('a, value) t -> ('b, value) t -> ('c, 'd) t) :
    ('a -> 'b -> 'c, lambda) t =
  lambda (fun a -> lambda (fun b -> body a b))

let lambda3
    (body : ('a, value) t -> ('b, value) t -> ('c, value) t -> ('d, 'e) t) :
    ('a -> 'b -> 'c -> 'd, lambda) t =
  lambda (fun a -> lambda (fun b -> lambda (fun c -> body a b c)))

let lambda4
    (body :
      ('a, value) t ->
      ('b, value) t ->
      ('c, value) t ->
      ('d, value) t ->
      ('e, 'f) t) : ('a -> 'b -> 'c -> 'd -> 'e, lambda) t =
  lambda (fun a ->
      lambda (fun b -> lambda (fun c -> lambda (fun d -> body a b c d))))

let lambda5
    (body :
      ('a, value) t ->
      ('b, value) t ->
      ('c, value) t ->
      ('d, value) t ->
      ('e, value) t ->
      ('f, 'g) t) : ('a -> 'b -> 'c -> 'd -> 'e -> 'f, lambda) t =
  lambda (fun a ->
      lambda (fun b ->
          lambda (fun c -> lambda (fun d -> lambda (fun e -> body a b c d e)))))

let unsafe_sequence (v0 : ('a, value) t) (v1 : ('b, value) t) : ('b, value) t =
  match (v0, v1) with
  | ( ( Value { env = env0; locals = locals0; revlet = revlet0 }
      | Apply { env = env0; locals = locals0; revlet = revlet0; _ } ),
      ( Value { env = env1; locals = locals1; revlet = revlet1 }
      | Apply { env = env1; locals = locals1; revlet = revlet1; _ } ) ) ->
      let env = Ident.Map.union_left env0 env1
      and locals = Ident.Map.union_left locals0 locals1
      and label1 = List.hd revlet1 in
      let revlet =
        if Ident.Map.mem label1 locals0 then revlet0 else label1 :: revlet0
      in
      Value { env; locals; revlet }

let unsafe_pair x (v0 : ('a, value) t) (v1 : ('b, value) t) : ('a * 'b, value) t
    =
  let (Value { env; locals; revlet } | Apply { env; locals; revlet; _ }) = x
  and (Value { revlet = revlet0; _ } | Apply { revlet = revlet0; _ }) = v0
  and (Value { revlet = revlet1; _ } | Apply { revlet = revlet1; _ }) = v1 in
  let label = Ident.create_local (keygen ()) in
  let locals =
    Ident.Map.add label
      (Lambda.Lprim
         ( Pmakeblock (0, Immutable, None),
           [ Lambda.Lvar (List.hd revlet0); Lambda.Lvar (List.hd revlet1) ],
           Loc_unknown ))
      locals
  and revlet = label :: revlet in
  Value { env; locals; revlet }

let unsafe_field i x v =
  let (Value { env; locals; revlet } | Apply { env; locals; revlet; _ }) = x in
  let (Value { revlet = revlet0; _ } | Apply { revlet = revlet0; _ }) = v in
  let label = Ident.create_local (keygen ()) in
  let locals =
    Ident.Map.add label
      (Lambda.Lprim (Pfield i, [ Lambda.Lvar (List.hd revlet0) ], Loc_unknown))
      locals
  and revlet = label :: revlet in
  Value { env; locals; revlet }

let unsafe_fst : ('a, value) t -> ('b * 'c, value) t -> ('b, value) t =
 fun x v -> unsafe_field 0 x v

let unsafe_snd : ('a, value) t -> ('b * 'c, value) t -> ('c, value) t =
 fun x v -> unsafe_field 1 x v

let unsafe_apply =
  let apply1 env locals revlet revlet0 revlet1 =
    let label = Ident.create_local (keygen ())
    and closure = Lambda.Lvar (List.hd revlet0)
    and revargs = [ Lambda.Lvar (List.hd revlet1) ] in
    let lambda_apply =
      Lambda.(
        Lapply
          {
            ap_func = closure;
            ap_args = revargs;
            ap_loc = Loc_unknown;
            ap_tailcall = Default_tailcall;
            ap_inlined = Default_inline;
            ap_specialised = Default_specialise;
          })
    in
    let locals = Ident.Map.add label lambda_apply locals
    and revlet = label :: revlet in
    Apply { env; locals; revlet; closure; revargs; arity = 1 }
  in
  fun (k : ('a, value) t) (v0 : ('b -> 'c, value) t) (v1 : ('b, value) t) :
      ('c, value) t ->
    match (k, v0, v1) with
    | ( (Value { env; locals; revlet } | Apply { env; locals; revlet; _ }),
        Value { revlet = revlet0; _ },
        (Value { revlet = revlet1; _ } | Apply { revlet = revlet1; _ }) ) ->
        apply1 env locals revlet revlet0 revlet1
    | ( (Value { env; locals; revlet } | Apply { env; locals; revlet; _ }),
        Apply { revlet = revlet0; closure; revargs; arity; _ },
        (Value { revlet = revlet1; _ } | Apply { revlet = revlet1; _ }) ) ->
        if arity = caml_applymax then apply1 env locals revlet revlet0 revlet1
        else
          let label = Ident.create_local (keygen ())
          and revargs = Lambda.Lvar (List.hd revlet1) :: revargs in
          let lambda_apply =
            Lambda.(
              Lapply
                {
                  ap_func = closure;
                  ap_args = List.rev revargs;
                  ap_loc = Loc_unknown;
                  ap_tailcall = Default_tailcall;
                  ap_inlined = Default_inline;
                  ap_specialised = Default_specialise;
                })
          in
          let locals = Ident.Map.add label lambda_apply locals
          and revlet = label :: revlet in
          Apply { env; locals; revlet; closure; revargs; arity = arity + 1 }

let unsafe_apply2 (x : ('x, value) t) (f : ('a -> 'b -> 'c, value) t)
    (a : ('a, value) t) (b : ('b, value) t) : ('c, value) t =
  unsafe_apply x (unsafe_apply x f a) b

let unsafe_apply3 (x : ('x, value) t) (f : ('a -> 'b -> 'c -> 'd, value) t)
    (a : ('a, value) t) (b : ('b, value) t) (c : ('c, value) t) : ('d, value) t
    =
  unsafe_apply x (unsafe_apply x (unsafe_apply x f a) b) c

let unsafe_apply4 (x : ('x, value) t)
    (f : ('a -> 'b -> 'c -> 'd -> 'e, value) t) (a : ('a, value) t)
    (b : ('b, value) t) (c : ('c, value) t) (d : ('d, value) t) : ('e, value) t
    =
  unsafe_apply x (unsafe_apply x (unsafe_apply x (unsafe_apply x f a) b) c) d

let unsafe_apply5 (x : ('x, value) t)
    (f : ('a -> 'b -> 'c -> 'd -> 'e -> 'f, value) t) (a : ('a, value) t)
    (b : ('b, value) t) (c : ('c, value) t) (d : ('d, value) t)
    (e : ('e, value) t) : ('f, value) t =
  unsafe_apply x
    (unsafe_apply x
       (unsafe_apply x (unsafe_apply x (unsafe_apply x f a) b) c)
       d)
    e
