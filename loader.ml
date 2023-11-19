assert (Sys.backend_type = Sys.Native)

let backend =
  (module struct
    let symbol_for_global' = Compilenv.symbol_for_global'
    let closure_symbol = Compilenv.closure_symbol
    let really_import_approx = Import_approx.really_import_approx
    let import_symbol = Import_approx.import_symbol
    let size_int = Arch.size_int
    let big_endian = Arch.big_endian

    let max_sensible_number_of_arguments =
      (* The "-1" is to allow for a potential closure environment parameter. *)
      Proc.max_arguments_for_tailcalls - 1
  end : Backend_intf.S)

let assemble_and_link cmx binding lambda_program =
  Profile.reset ();
  Compilenv.reset "";
  Compilenv.cache_unit_info cmx;
  Backend_var.reinit ();
  let output : 'a option ref = ref None in
  X86_proc.with_internal_assembler
    (fun x86_asm _ -> output := Some x86_asm)
    (fun () ->
      Asmgen.compile_implementation ~backend ~prefixname:""
        ~middle_end:Closure_middle_end.lambda_to_clambda
        ~ppf_dump:Format.err_formatter lambda_program);
  match !output with
  | None -> raise (Failure "return")
  | Some x86_asm -> X86_emitter.generate_asm binding x86_asm
