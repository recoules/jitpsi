(** Just In Time Partial Specialization for Interpreters *)

type value
and lambda

and (+'a, 'b) t
(**
   A staged term [('a,]{!type:value}[)]{!type:t} evaluates
   to a value of type ['a].
   A staged function [('a -> 'b,]{!type:lambda}[)]{!type:t} assembles
   to a function of type ['a -> 'b].
*)

(** {1 Core primitives} *)

val const : 'a -> ('a, value) t
(** [const value]
    creates a staged term that evaluates to the runtime value [value].
*)

val apply : ('a -> 'b, value) t -> ('a, value) t -> ('b, value) t
(** [apply f a]
    creates a staged application of the staged term [f] to the staged
    term [a].
    {!val:apply} can be chained to create an application of greater arity
    (see {!val:apply2}, {!val:apply3}, {!val:apply4} or {!val:apply5}
    for examples).

    A staged application is evaluated once and only once.
    If [f] is not pure, make sure to create as many staged applications
    it should be evaluated at runtime.
*)

val sequence : (unit, value) t -> ('a, value) t -> ('a, value) t
(** [sequence a b]
    creates a staged sequence that evaluates to [b] after
    the evaluation of [a] took place.
*)

val pair : ('a, value) t -> ('b, value) t -> ('a * 'b, value) t
(** [pair a b]
    creates a staged value that evaluates to the pair [(a, b)] at runtime.
*)

val fst : ('a * 'b, value) t -> ('a, value) t
(** [fst a]
    creates a staged application of the [fst] function
    (return the first element of a pair).
*)

val snd : ('a * 'b, value) t -> ('b, value) t
(** [snd a]
    creates a staged application of the [snd] function
    (return the second element of a pair).
*)

val lambda : (('a, value) t -> ('b, 'c) t) -> ('a -> 'b, lambda) t
(** [lambda body]
    creates a staged function that takes one argument and evaluates
    to the staged term returned by the generator [body].
    {!val:lambda} can be chained to create a function of greater arity
    (see {!val:lambda2}, {!val:lambda3}, {!val:lambda4} or
    {!val:lambda5} for examples).

    @param body Given a staged argument [a], [body a] returns a staged
                term that evaluates to the return value.
*)

val return : ('a, lambda) t -> 'a
(** [return lambda]
    assembles the staged function [lambda] and returns its {i OCaml} closure.
*)

(** {1 Syntactic sugar} *)

val ( @@ ) : (unit, value) t -> ('a, value) t -> ('a, value) t
(** [a @@ b]
    alias {!val:sequence}[ a b].
*)

val apply2 :
  ('a -> 'b -> 'c, value) t -> ('a, value) t -> ('b, value) t -> ('c, value) t
(** [apply2 f a b]
    alias {!val:apply}[(]{!val:apply}[ f a) b].
*)

val apply3 :
  ('a -> 'b -> 'c -> 'd, value) t ->
  ('a, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t
(** [apply3 f a b c]
    alias {!val:apply}[(]{!val:apply2}[ f a b) c].
*)

val apply4 :
  ('a -> 'b -> 'c -> 'd -> 'e, value) t ->
  ('a, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, value) t
(** [apply4 f a b c d]
    alias {!val:apply}[(]{!val:apply3}[ f a b c) d].
*)

val apply5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f, value) t ->
  ('a, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, value) t ->
  ('f, value) t
(** [apply5 f a b c d e]
    alias {!val:apply}[(]{!val:apply4}[ f a b c d) e].
*)

val lambda2 :
  (('a, value) t -> ('b, value) t -> ('c, 'd) t) -> ('a -> 'b -> 'c, lambda) t
(** [lambda2 body]
    alias {!val:lambda}[(fun a ->]{!val:lambda}[(body a))]
*)

val lambda3 :
  (('a, value) t -> ('b, value) t -> ('c, value) t -> ('d, 'e) t) ->
  ('a -> 'b -> 'c -> 'd, lambda) t
(** [lambda3 body]
    alias {!val:lambda}[(fun a ->]{!val:lambda2}[(body a))]
*)

val lambda4 :
  (('a, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, 'f) t) ->
  ('a -> 'b -> 'c -> 'd -> 'e, lambda) t
(** [lambda4 body]
    alias {!val:lambda}[(fun a ->]{!val:lambda3}[(body a))]
*)

val lambda5 :
  (('a, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, value) t ->
  ('f, 'g) t) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f, lambda) t
(** [lambda5 body]
    alias {!val:lambda}[(fun a ->]{!val:lambda4}[(body a))]
*)

(** {1 Expert section} *)

val unsafe_sequence : ('a, value) t -> ('b, value) t -> ('b, value) t
(** [unsafe_sequence x a]
    creates a staged sequence that evaluates [x] then [a]
    assuming all the dependencies of [a] are defined in [x].
*)

val unsafe_pair :
  ('a, value) t -> ('b, value) t -> ('c, value) t -> ('b * 'c, value) t
(** [pair x a b]
    creates a staged value that evaluates to the pair [(a, b)] at runtime
    assuming all the dependencies of [a] and [b] are defined in [x].
*)

val unsafe_fst : ('a, value) t -> ('b * 'c, value) t -> ('b, value) t
(** [unsafe_fst x a]
    creates a staged application of the [fst] function
    assuming all the dependencies of [a] are defined in [x].
*)

val unsafe_snd : ('a, value) t -> ('b * 'c, value) t -> ('c, value) t
(** [unsafe_snd x a]
    creates a staged application of the [snd] function
    assuming all the dependencies of [a] are defined in [x].
*)

val unsafe_apply :
  ('a, value) t -> ('b -> 'c, value) t -> ('b, value) t -> ('c, value) t
(** [unsafe_apply x f a]
    creates a staged sequence that evaluates [x] then the application
    of the staged term [f] to the staged term [a] assuming that all
    dependencies of [f] and [a] are defined in [x].
*)

val unsafe_apply2 :
  ('a, value) t ->
  ('b -> 'c -> 'd, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t
(** [unsafe_apply2 x f a b]
    alias {!val:unsafe_apply}[x (]{!val:unsafe_apply}[ x f a) b].
*)

val unsafe_apply3 :
  ('a, value) t ->
  ('b -> 'c -> 'd -> 'e, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, value) t
(** [unsafe_apply3 x f a b c]
    alias {!val:unsafe_apply}[x (]{!val:unsafe_apply2}[ x f a b) c].
*)

val unsafe_apply4 :
  ('a, value) t ->
  ('b -> 'c -> 'd -> 'e -> 'f, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, value) t ->
  ('f, value) t
(** [unsafe_apply4 x f a b c d]
    alias {!val:unsafe_apply}[x (]{!val:unsafe_apply3}[ x f a b c) d].
*)

val unsafe_apply5 :
  ('a, value) t ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g, value) t ->
  ('b, value) t ->
  ('c, value) t ->
  ('d, value) t ->
  ('e, value) t ->
  ('f, value) t ->
  ('g, value) t
(** [unsafe_apply5 x f a b c d e]
    alias {!val:unsafe_apply}[x (]{!val:unsafe_apply4}[ x f a b c d) e].
*)
