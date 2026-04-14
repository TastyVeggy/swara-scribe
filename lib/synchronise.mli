module Error : sig
  type t

  val pp : Format.formatter -> t -> unit
end

val synchronise : Ir.Ast.t -> (Ir.Score.t, Error.t) result
