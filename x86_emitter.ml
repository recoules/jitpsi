module Root = struct
  type t

  external create : unit -> t = "ocaml_jitpsi_create"

  external load_code :
    Bytes.t ->
    bytesize:(int[@untagged]) ->
    align:(int[@untagged]) ->
    t ->
    (int[@untagged]) = "ocaml_jitpsi_load_code" "native_jitpsi_load_code"

  external load_frame :
    Bytes.t -> bytesize:(int[@untagged]) -> align:(int[@untagged]) -> t -> unit
    = "ocaml_jitpsi_load_frame" "native_jitpsi_load_frame"

  external global_symbol : string -> Int64.t = "ocaml_jitpsi_global_symbol"
end

module Symbol = struct
  module Map = Map.Make (String)

  type 'a t =
    | Scan
    | Function of {
        name : string;
        shellcode : Bytes.t;
        bytesize : int;
        align : int;
        labels : int Map.t;
      }
    | Loaded of { root : Root.t; base : int; labels : int Map.t }
    | Closed of { root : Root.t; base : int; labels : int Map.t; f : 'a }
end

module R = struct
  module V = struct
    type t = Obj of Obj.t | Raw of Nativeint.t

    let obj x = Obj (Obj.repr x)
    let raw x = Raw x
  end

  type t = V.t array

  let create () : t = Array.make 16 (V.obj 0)

  let idx (r : X86_ast.reg64) =
    match r with
    | RAX -> 0
    | RBX -> 1
    | RCX -> 2
    | RDX -> 3
    | RSP -> 4
    | RBP -> 5
    | RSI -> 6
    | RDI -> 7
    | R8 -> 8
    | R9 -> 9
    | R10 -> 10
    | R11 -> 11
    | R12 -> 12
    | R13 -> 13
    | R14 -> 14
    | R15 -> 15

  let num (r : X86_ast.reg64) =
    match r with
    | RAX -> 0
    | RCX -> 1
    | RDX -> 2
    | RBX -> 3
    | RSP -> 4
    | RBP -> 5
    | RSI -> 6
    | RDI -> 7
    | R8 -> 8
    | R9 -> 9
    | R10 -> 10
    | R11 -> 11
    | R12 -> 12
    | R13 -> 13
    | R14 -> 14
    | R15 -> 15

  let get_obj (t : t) r =
    match Array.get t (idx r) with
    | Obj x -> Obj.magic x
    | Raw _ -> raise Not_found

  let set_obj t r x = Array.set t (idx r) (V.obj x)
  let set_raw t r x = Array.set t (idx r) (V.raw x)

  let set_any t r x =
    if Obj.is_int (Obj.repr x) then set_raw t r (Nativeint.of_int (Obj.magic x))
    else set_obj t r x

  let get t r = Array.get t (idx r)
  let set t r x = Array.set t (idx r) x
end

module Assembly = struct
  type t = {
    name : string;
    align : int;
    mutable shellcode : Bytes.t;
    mutable offset : int;
    mutable locals : int Symbol.Map.t;
    mutable extern : string list;
    mutable reloc : (int * string) list;
    binding : Obj.t array;
  }

  let make name align binding =
    {
      name;
      align;
      shellcode = Bytes.create 512;
      offset = 0;
      locals = Symbol.Map.singleton name 0;
      extern = [];
      reloc = [];
      binding;
    }

  let widen t =
    t.shellcode <- Bytes.extend t.shellcode 0 (Bytes.length t.shellcode)

  let ensure n t = if Bytes.length t.shellcode < t.offset + n then widen t

  let space n t =
    ensure n t;
    t.offset <- t.offset + n

  let push_byte byte t =
    if Bytes.length t.shellcode = t.offset then widen t;
    Bytes.unsafe_set t.shellcode t.offset byte;
    t.offset <- t.offset + 1

  let push_bytes bytes t =
    let n = String.length bytes in
    ensure n t;
    Bytes.unsafe_blit_string bytes 0 t.shellcode t.offset n;
    t.offset <- t.offset + n

  let push_imm8 imm t = push_byte (Char.unsafe_chr (0xff land imm)) t
  let push_imm8L imm t = push_imm8 (Int64.to_int imm) t

  let push_imm32l imm t =
    ensure 4 t;
    Bytes.set_int32_le t.shellcode t.offset imm;
    t.offset <- t.offset + 4

  let push_imm32L imm t = push_imm32l (Int64.to_int32 imm) t

  let push_imm64L imm t =
    ensure 8 t;
    Bytes.set_int64_le t.shellcode t.offset imm;
    t.offset <- t.offset + 8

  let push_symbol name t =
    assert (t.offset land 0b111 = 0);
    t.locals <- Symbol.Map.add name t.offset t.locals;
    let value =
      Root.global_symbol (String.sub name 0 (String.length name - 4))
    in
    if Int64.equal Int64.zero value then raise (Failure name);
    push_imm64L value t

  let nop n t =
    match n with
    | 0 -> ()
    | 1 -> push_byte '\x90' t
    | 2 -> push_bytes "\x66\x90" t
    | 3 -> push_bytes "\x0f\x1f\x00" t
    | 4 -> push_bytes "\x0f\x1f\x40\x00" t
    | 5 -> push_bytes "\x0f\x1f\x44\x00\x00" t
    | 6 -> push_bytes "\x66\x0f\x1f\x44\x00\x00" t
    | 7 -> push_bytes "\x0f\x1f\x80\x00\x00\x00\x00" t
    | 8 -> push_bytes "\x0f\x1f\x84\x00\x00\x00\x00\x00" t
    | 9 -> push_bytes "\x66\x0f\x1f\x84\x00\x00\x00\x00\x00" t
    | _ -> assert false

  let is_imm8 x = -128 <= x && x < 128
  let is_imm8L x = -128L <= x && x < 128L
  let is_imm32L x = -0x8000_0000L <= x && x < 0x8000_0000L

  let push_cond (c : X86_ast.condition) t =
    push_byte
      (Char.unsafe_chr
         (0x80
         lor
         match c with
         | O -> 0
         | NO -> 1
         | B -> 2
         | AE -> 3
         | E -> 4
         | NE -> 5
         | BE -> 6
         | A -> 7
         | S -> 8
         | NS -> 9
         | P -> 10
         | NP -> 11
         | L -> 12
         | GE -> 13
         | LE -> 14
         | G -> 15))
      t

  let push_rex rex t =
    if rex <> 0 then push_byte (Char.unsafe_chr (0b0100_0000 lor rex)) t

  let rexr_reg reg = ((7 - reg) asr 4) land 0b0100
  let rexb_rm reg = ((7 - reg) asr 4) land 0b0001

  let mod_rm_reg m rm reg =
    Char.unsafe_chr ((m lsl 6) lor ((0b111 land reg) lsl 3) lor (0b111 land rm))

  let push_mod_reg_reg64 rex opcode rm reg t =
    push_rex (rex lor rexr_reg reg lor rexb_rm rm) t;
    push_bytes opcode t;
    push_byte (mod_rm_reg 0b11 rm reg) t

  let push_plt name opcode reg t =
    push_bytes opcode t;
    push_byte (mod_rm_reg 0b00 0b101 reg) t;
    t.reloc <- (t.offset, name) :: t.reloc;
    space 4 t

  let push_mod_idx_reg64 rex opcode idx displ reg t =
    let m =
      if displ = 0 && 0b111 land idx <> 0b101 then 0b00
      else if is_imm8 displ then 0b01
      else 0b10
    in
    push_rex (rex lor rexr_reg reg lor rexb_rm idx) t;
    push_bytes opcode t;
    push_byte (mod_rm_reg m idx reg) t;
    if 0b111 land idx = 0b100 then push_byte '\x24' t;
    if m = 0b01 then push_imm8 displ t
    else if m = 0b10 then push_imm32l (Int32.of_int displ) t

  let push (ins : X86_ast.instruction) t =
    match ins with
    | CALL (Sym name) ->
        assert (String.ends_with ~suffix:"@PLT" name);
        if not (List.mem name t.extern) then t.extern <- name :: t.extern;
        push_plt name "\xff" 2 t
    | JMP (Sym name) ->
        if String.ends_with ~suffix:"@PLT" name then (
          if not (List.mem name t.extern) then t.extern <- name :: t.extern;
          push_plt name "\xff" 4 t)
        else (
          push_byte '\xe9' t;
          t.reloc <- (t.offset, name) :: t.reloc;
          space 4 t)
    | CALL (Reg64 rm) -> push_mod_reg_reg64 0 "\xff" (R.num rm) 2 t
    | JMP (Reg64 rm) -> push_mod_reg_reg64 0 "\xff" (R.num rm) 4 t
    | ADD (Imm i, Reg64 r) ->
        if is_imm8L i then (
          push_mod_reg_reg64 0b1000 "\x83" (R.num r) 0 t;
          push_imm8L i t)
        else (
          push_mod_reg_reg64 0b1000 "\x81" (R.num r) 0 t;
          push_imm32L i t)
    | SUB (Imm i, Reg64 r) ->
        if is_imm8L i then (
          push_mod_reg_reg64 0b1000 "\x83" (R.num r) 5 t;
          push_imm8L i t)
        else (
          push_mod_reg_reg64 0b1000 "\x81" (R.num r) 5 t;
          push_imm32L i t)
    | CMP
        ( Mem { typ = QWORD; idx; scale = 1; base = None; sym = None; displ; _ },
          Reg64 r ) ->
        push_mod_idx_reg64 0b1000 "\x3b" (R.num idx) displ (R.num r) t
    | J (c, Sym name) ->
        push_byte '\x0f' t;
        push_cond c t;
        t.reloc <- (t.offset, name) :: t.reloc;
        space 4 t
    | MOV (Reg64 s, Reg64 d) ->
        push_mod_reg_reg64 0b1000 "\x8b" (R.num s) (R.num d) t
    | MOV
        ( Mem { typ = QWORD; idx; scale = 1; base = None; sym = None; displ; _ },
          Reg64 r ) ->
        push_mod_idx_reg64 0b1000 "\x8b" (R.num idx) displ (R.num r) t
    | MOV
        ( Reg64 r,
          Mem { typ = QWORD; idx; scale = 1; base = None; sym = None; displ; _ }
        ) ->
        push_mod_idx_reg64 0b1000 "\x89" (R.num idx) displ (R.num r) t
    | MOV (Imm i, Reg64 r) ->
        if is_imm32L i then (
          push_mod_reg_reg64 0b1000 "\xc7" (R.num r) 0 t;
          push_imm32L i t)
        else (
          push_rex (0b1000 lor rexb_rm (R.num r)) t;
          push_imm8 (0xb8 lor (0b111 land R.num r)) t;
          push_imm64L i t)
    | MOV (Imm i, Reg32 r) ->
        push_rex (rexb_rm (R.num r)) t;
        push_imm8 (0xb8 lor (0b111 land R.num r)) t;
        push_imm32L i t
    | MOV
        ( Imm i,
          Mem { typ = QWORD; sym = None; base = None; scale = 1; idx; displ; _ }
        ) ->
        push_mod_idx_reg64 0b1000 "\xc7" (R.num idx) displ 0 t;
        push_imm32L i t
    | LEA (Mem { sym = None; base = None; scale = 1; idx; displ; _ }, Reg64 r)
      ->
        push_mod_idx_reg64 0b1000 "\x8d" (R.num idx) displ (R.num r) t
    | RET -> push_byte '\xc3' t
    | _ -> raise (Failure "[assemble] unexpected instruction")

  let align =
    let rec fill n t =
      if n <= 9 then nop n t
      else (
        nop 9 t;
        fill (n - 9) t)
    in
    fun n t ->
      let r = t.offset mod n in
      if r <> 0 then fill (n - r) t

  let record name t = t.locals <- Symbol.Map.add name t.offset t.locals

  let emit t =
    align 8 t;
    List.iter
      (fun name ->
        try
          let n = Scanf.sscanf name "jitpsi__dcall_%d%@PLT" Fun.id in
          let closure = Array.get t.binding n in
          let info : int = Obj.magic (Obj.field closure 1) in
          let addr =
            if info lsr (Sys.int_size - 8) > 1 then Obj.raw_field closure 2
            else Obj.raw_field closure 0
          in
          t.locals <- Symbol.Map.add name t.offset t.locals;
          push_imm64L (Int64.of_nativeint addr) t
        with Scanf.Scan_failure _ -> push_symbol name t)
      t.extern;
    List.iter
      (fun (i, name) ->
        Bytes.set_int32_le t.shellcode i
          (Int32.of_int (Symbol.Map.find name t.locals - (i + 4))))
      t.reloc;
    Symbol.Function
      {
        name = t.name;
        shellcode = t.shellcode;
        bytesize = t.offset;
        align = t.align;
        labels = t.locals;
      }
end

module Lbuffer = struct
  type t = { buf : X86_ast.asm_line array; mutable i : int; mutable j : int }

  let create () = { buf = Array.make 8 X86_ast.Cfi_endproc; i = 0; j = 0 }

  let assemble (line : X86_ast.asm_line) asm =
    match line with
    | NewLabel (name, _) -> Assembly.record name asm
    | Ins ins -> Assembly.push ins asm
    | Align (_, n) -> Assembly.align n asm
    | _ -> ()

  let incr x = (x + 1) land 0x7
  let decr x = (x - 1) land 0x7

  let push line asm t =
    let i' = incr t.i in
    if i' = t.j then (
      assemble (Array.get t.buf t.j) asm;
      t.j <- incr t.j);
    Array.set t.buf t.i line;
    t.i <- i'

  let flush asm t =
    while t.i <> t.j do
      assemble (Array.get t.buf t.j) asm;
      t.j <- incr t.j
    done

  type target =
    | Restore_stack
    | Set_Arg of bool
    | Get_closure of X86_ast.reg64 * bool
    | Tailcall

  let finalize_return asm t ret =
    flush asm t;
    Assembly.push ret asm

  let rec rewind_return asm t target i =
    if i = t.j then finalize_return asm t RET
    else
      let i = decr i in
      let line = Array.get t.buf i in
      match (target, line) with
      | Restore_stack, Ins (ADD (Imm _, Reg64 RSP)) ->
          rewind_return asm t (Set_Arg true) i
      | Set_Arg _, Ins (MOV (_, Reg64 RAX)) ->
          rewind_return asm t (Set_Arg false) i
      | ( Set_Arg tailcall,
          Ins
            (MOV
              (Mem { idx; scale = 1; base = None; sym = None; _ }, Reg64 RBX)) )
        ->
          Array.set t.buf i Cfi_endproc;
          rewind_return asm t (Get_closure (idx, tailcall)) i
      | Get_closure (idx, tailcall), Ins (MOV (_, Reg64 idx')) when idx = idx'
        ->
          Array.set t.buf i Cfi_endproc;
          if tailcall then rewind_return asm t Tailcall i
          else finalize_return asm t RET
      | Tailcall, NewLabel _ -> (
          let i' = decr i in
          match Array.get t.buf i' with
          | Ins (CALL (Sym _ as sym)) ->
              Array.set t.buf i Cfi_endproc;
              Array.set t.buf i' Cfi_endproc;
              finalize_return asm t (JMP sym)
          | _ -> finalize_return asm t RET)
      | _, _ -> finalize_return asm t RET

  let optimize_return asm t = rewind_return asm t Restore_stack t.i
end

let rec read_program binding (target : 'a Symbol.t)
    (program : X86_ast.asm_program) in_text =
  match program with
  | [] -> raise (Failure "unexpected end of program")
  (* unsupported *)
  | Indirect_symbol _ :: _ ->
      raise (Failure "'.indirect_symbol' is not supported")
  | Set _ :: _ -> raise (Failure "'.set' is not supported")
  (* masm only *)
  | (Model _ | Mode386 | External _) :: _ -> assert false
  (* text section *)
  | Section ([ ".text" ], _, _) :: program ->
      read_program binding target program true
  (* other sections *)
  | Section _ :: program -> read_program binding target program false
  (* entry point *)
  | Align _ :: Global "caml__entry" :: NewLabel ("caml__entry", _) :: program ->
      let root = Root.create () in
      Array.set binding (Array.length binding - 1) (Obj.repr root);
      exec_program binding target program root (R.create ())
  | Align (_, 8)
    :: Global "caml__frametable"
    :: NewLabel ("caml__frametable", _)
    :: Quad _ :: program -> (
      match target with
      | Scan | Function _ ->
          raise (Failure "failed to find the target function")
      | Loaded _ -> raise (Failure "failed to close the target function")
      | Closed { root; base; labels; f } ->
          build_frame root base labels program (Bytes.create 128) 8 0 false;
          f)
  (* assembly function *)
  | Align (_, align) :: Global sym :: NewLabel (name, _) :: program when in_text
    ->
      assert (String.equal sym name);
      assert (target = Symbol.Scan);
      assemble_function binding target program
        (Assembly.make name align binding)
        (Lbuffer.create ())
  (* ignore *)
  | ( NewLabel _ | Align _ | Ins _ | File _ | Loc _ | Comment _ | Global _
    | Private_extern _ | Size _ | Type _ | Cfi_startproc
    | Cfi_adjust_cfa_offset _ | Cfi_endproc | Byte _ | Word _ | Long _ | Quad _
    | Bytes _ | Space _ )
    :: program ->
      read_program binding target program in_text

and exec_program binding target (program : X86_ast.asm_program) root registers =
  match program with
  | [] -> raise (Failure "missing 'ret'")
  | Ins (SUB (Imm i, Reg64 R15))
    :: Ins (CALL (Sym "caml_allocN@PLT"))
    :: NewLabel _
    :: Ins
         (LEA
           ( Mem { sym = None; base = None; scale = 1; idx = R15; displ = 8; _ },
             Reg64 d ))
    :: Ins
         (MOV
           ( Imm i',
             Mem
               {
                 typ = QWORD;
                 sym = None;
                 base = None;
                 scale = 1;
                 idx = d';
                 displ = -8;
                 _;
               } ))
    :: program ->
      let size : int = (Int64.to_int i - 8) asr 3 in
      let tag : int = Int64.to_int i' land 0xff in
      assert (d = d' && Int64.to_int i' lsr 10 = size);
      R.set_obj registers d (Obj.new_block tag size);
      exec_program binding target program root registers
  | Ins (MOV (Reg64 s, Reg64 d)) :: program ->
      R.set registers d (R.get registers s);
      exec_program binding target program root registers
  | Ins (MOV (Imm i, Reg64 r)) :: program ->
      R.set_raw registers r (Int64.to_nativeint i);
      exec_program binding target program root registers
  | Ins
      (MOV
        ( Imm i,
          Mem { typ = QWORD; sym = None; base = None; scale = 1; idx; displ; _ }
        ))
    :: program ->
      Obj.set_raw_field (R.get_obj registers idx) (displ asr 3)
        (Int64.to_nativeint i);
      exec_program binding target program root registers
  | Ins
      (MOV
        ( Reg64 s,
          Mem { typ = QWORD; sym = None; base = None; scale = 1; idx; displ; _ }
        ))
    :: program ->
      let b = R.get_obj registers idx in
      (match R.get registers s with
      | Obj v -> Obj.set_field b (displ asr 3) v
      | Raw x -> Obj.set_raw_field b (displ asr 3) x);
      exec_program binding target program root registers
  | Ins
      (MOV
        ( Mem { typ = QWORD; sym = None; base = None; scale = 1; idx; displ; _ },
          Reg64 d ))
    :: program ->
      let b = R.get_obj registers idx in
      R.set_any registers d (Obj.field b (displ asr 3));
      exec_program binding target program root registers
  | Ins (MOV (Mem64_RIP (QWORD, "caml@GOTPCREL", 0), Reg64 r)) :: program ->
      R.set_obj registers r (Obj.repr binding);
      exec_program binding target program root registers
  | Ins (MOV (Mem64_RIP (QWORD, name, 0), Reg64 r)) :: program -> (
      assert (String.ends_with ~suffix:"@GOTPCREL" name);
      let sym = String.sub name 0 (String.length name - 9) in
      match target with
      | Scan -> raise (Failure "failed to find the target function")
      | Function { name; shellcode; bytesize; align; labels }
        when String.equal sym name ->
          let addr = Root.load_code shellcode ~bytesize ~align root in
          R.set_raw registers r (Nativeint.of_int addr);
          exec_program binding
            (Loaded { root; base = addr; labels })
            program root registers
      | Function _ | Loaded _ ->
          let value = Root.global_symbol sym in
          if Int64.equal Int64.zero value then raise (Failure name);
          R.set_raw registers r (Int64.to_nativeint value);
          exec_program binding target program root registers
      | Closed _ -> assert false)
  | Ins (SUB (Imm i, Reg64 RSP)) :: program ->
      R.set_obj registers RSP (Obj.new_block 0 (Int64.to_int i lsr 3));
      exec_program binding target program root registers
  | Ins (ADD (Imm _, Reg64 RSP)) :: program ->
      R.set_obj registers RSP (Obj.repr 0);
      exec_program binding target program root registers
  | Ins RET :: program -> (
      match target with
      | Scan | Function _ | Closed _ -> assert false
      | Loaded { root; base; labels } ->
          read_program binding
            (Closed { root; base; labels; f = R.get_obj registers RAX })
            program true)
  | Ins _ :: _ -> raise (Failure "[link] unexpected instruction")
  (* ignore *)
  | ( File _ | Align _ | Loc _ | Comment _ | Global _ | NewLabel _
    | Private_extern _ | Size _ | Type _ | Cfi_startproc
    | Cfi_adjust_cfa_offset _ | Cfi_endproc )
    :: program ->
      exec_program binding target program root registers
  | Section _ :: _ -> raise (Failure "section before 'ret'")
  (* unknown *)
  | Indirect_symbol _ :: _ ->
      raise (Failure "'.indirect_symbol' is not supported")
  | Set _ :: _ -> raise (Failure "'.set' is not supported")
  | (Byte _ | Word _ | Long _ | Quad _ | Bytes _ | Space _) :: _ ->
      raise (Failure "unexpected data in '.text'")
  (* masm only *)
  | (Model _ | Mode386 | External _) :: _ -> assert false

and assemble_function binding target (program : X86_ast.asm_program)
    (assembly : Assembly.t) lbuf =
  match program with
  | [] -> raise (Failure "'.size' is missing")
  | Size (name, _) :: program ->
      assert (String.equal assembly.name name);
      Lbuffer.flush assembly lbuf;
      read_program binding (Assembly.emit assembly) program true
  | Ins (JMP (Sym "jitpsi__ret@PLT")) :: program ->
      Lbuffer.optimize_return assembly lbuf;
      assemble_function binding target program assembly lbuf
  | ((NewLabel _ | Ins _ | Align _) as line) :: program ->
      Lbuffer.push line assembly lbuf;
      assemble_function binding target program assembly lbuf
  | ( Loc _ | Comment _ | Type _ | Cfi_startproc | Cfi_adjust_cfa_offset _
    | Cfi_endproc )
    :: program ->
      assemble_function binding target program assembly lbuf
  | (File _ | Section _ | Global _ | Private_extern _) :: _ ->
      raise (Failure "unexpected directive in function")
  (* unknown *)
  | Indirect_symbol _ :: _ ->
      raise (Failure "'.indirect_symbol' is not supported")
  | Set _ :: _ -> raise (Failure "'.set' is not supported")
  | (Byte _ | Word _ | Long _ | Quad _ | Bytes _ | Space _) :: _ ->
      raise (Failure "unexpected data in '.text'")
  (* masm only *)
  | (Model _ | Mode386 | External _) :: _ -> assert false

and build_frame =
  let ensure bytes offset n =
    let size = Bytes.length bytes in
    if size < offset + n then Bytes.extend bytes 0 size else bytes
  in
  fun root base labels (program : X86_ast.asm_program) frametable bytesize
      entries in_entry ->
    match program with
    | [] -> raise (Failure "'.size' is missing")
    | Size ("caml__frametable", _) :: _ ->
        Bytes.set_int64_le frametable 0 (Int64.of_int entries);
        Root.load_frame frametable ~bytesize ~align:8 root
    | Quad (ConstLabel name) :: program -> (
        try
          let offset = Symbol.Map.find name labels in
          let frametable = ensure frametable bytesize 8 in
          Bytes.set_int64_le frametable bytesize (Int64.of_int (base + offset));
          build_frame root base labels program frametable (bytesize + 8)
            (entries + 1) true
        with Not_found ->
          build_frame root base labels program frametable bytesize entries false
        )
    | Long (Const value) :: program when in_entry ->
        let frametable = ensure frametable bytesize 4 in
        Bytes.set_int32_le frametable bytesize (Int64.to_int32 value);
        build_frame root base labels program frametable (bytesize + 4) entries
          in_entry
    | Word (Const value) :: program when in_entry ->
        let frametable = ensure frametable bytesize 2 in
        Bytes.set_int16_le frametable bytesize (Int64.to_int value);
        build_frame root base labels program frametable (bytesize + 2) entries
          in_entry
    | Byte (Const value) :: program when in_entry ->
        let frametable = ensure frametable bytesize 1 in
        Bytes.set frametable bytesize (Char.unsafe_chr (Int64.to_int value));
        build_frame root base labels program frametable (bytesize + 1) entries
          in_entry
    | Align (_, n) :: program when in_entry ->
        let n =
          let m = bytesize mod n in
          if m = 0 then 0 else n - m
        in
        let frametable = ensure frametable bytesize n in
        Bytes.fill frametable bytesize n '\x00';
        build_frame root base labels program frametable (bytesize + n) entries
          in_entry
    | Space n :: program when in_entry ->
        let frametable = ensure frametable bytesize n in
        Bytes.fill frametable bytesize n '\x00';
        build_frame root base labels program frametable (bytesize + n) entries
          in_entry
    | ( Long (Const _)
      | Word (Const _)
      | Byte (Const _)
      | Align _ | Space _ | Loc _ | Comment _ | Type _ | Cfi_startproc
      | Cfi_adjust_cfa_offset _ | Cfi_endproc )
      :: program ->
        build_frame root base labels program frametable bytesize entries
          in_entry
    | (File _ | Section _ | Global _ | Private_extern _ | NewLabel _ | Size _)
      :: _ ->
        raise (Failure "unexpected directive in 'caml__frametable'")
    | (Ins _ | Byte _ | Word _ | Long _ | Quad _ | Bytes _) :: _ ->
        raise (Failure "unexpected content in 'caml__frametable'")
    (* unknown *)
    | Indirect_symbol _ :: _ ->
        raise (Failure "'.indirect_symbol' is not supported")
    | Set _ :: _ -> raise (Failure "'.set' is not supported")
    (* masm only *)
    | (Model _ | Mode386 | External _) :: _ -> assert false

let generate_asm binding (program : X86_ast.asm_program) =
  read_program binding Symbol.Scan program false
