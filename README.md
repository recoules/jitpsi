[![License: MIT](https://img.shields.io/badge/License-GNU%20GPL-blue)](https://opensource.org/licenses/GPL-3-0)

# JITPSI

JITPSI is a tiny library for Just In Time compilation in OCaml.
It allows some simple staged programming, as defined by [MetaOCaml](https://okmij.org/ftp/ML/MetaOCaml.html).
It is designed to help interpretor developpers to specialize their evaluation loop for *runtime-known* constant sequence of operations.
Typical use is basic bloc unfolding in emulator'*like* programs.
For speed, JITPSI emits **x86-64** code directly in memory in the same way [ocaml-jit](https://github.com/tarides/ocaml-jit) does.

## Quickstart

Consider the example from the MetaOcaml (from the A.P.Ershov's 1977 paper).
```ocaml
let square x = x * x
(* int -> int *)
let rec power n x =
  if n = 0 then 1
  else if n mod 2 = 0 then square (power (n / 2) x)
  else x * (power (n - 1) x)
(* int -> int -> int *)
```

At some point of the execution, the program may know that it has to compute several times the result with a constant exponent of `7`.

If it was at compile time, we would have written the following, expecting for the compiler to specialize the function.
```ocaml
let power7 = power 7
(* int -> int *)
```

JITPSI allows to do something similar by creating a new function at runtime.
For instance, we can write a generator of `power`'s function.
```ocaml
let rec jpower n x =
  if n = 0 then Jitpsi.const 1
  else if n mod 2 = 0 then 
    Jitpsi.apply (Jitpsi.const square) (jpower (n / 2) x)
  else Jitpsi.apply2 (Jitpsi.const ( * )) x (jpower (n - 1) x)
(* int -> (int, Jitpsi.value) Jitpsi.t -> (int, Jitpsi.value) Jitpsi.t *)
```

We can then specialize the `power` function for the exponent `7`.
```ocaml
let lpower7 = Jitpsi.lambda (jpower 7)
(* (int -> int, Jitpsi.lambda) Jitpsi.t *)
let power7 = Jitpsi.return lpower7
(* int -> int *)
```

The returned closure is a fresh piece of `x86` native code that can be used as any OCaml function.
```ocaml
power7 2;;
(* 128 *)
```

## Examples

An example of specialization for a toy interpreter can be found in the directory [examples](examples).

As a more advanced example, JITPSI has been used to optimize the symbolic execution of the [BINSEC](https://github.com/binsec/binsec) plateform.
The branch [jitpsi](https://github.com/binsec/binsec/tree/jitpsi) shows a significant performance boost for the [fibonacci](https://github.com/binsec/binsec/tree/jitpsi/examples/sse/fibonacci) (+100%) and [licorne](https://github.com/binsec/binsec/tree/jitpsi/examples/sse/fcsc/2022.licorne) (+40%) when using the flag `-sse-jit`.
