module Error : sig
  type t

  val pp : Format.formatter -> t -> unit
end

val parse : Lexer.Token.t list -> (Ir.Ast.t, Error.t) result
