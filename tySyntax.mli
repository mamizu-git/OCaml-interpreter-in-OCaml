type tyvar
	      
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyTuple of ty list (* tuple *)
  | TyList of ty (* list *)

(*
 * Generate a fresh type variable
 *   (i.e. a type variable that has not been appeared)
 *)
val new_tyvar : unit -> tyvar

val print_type : ty -> unit
