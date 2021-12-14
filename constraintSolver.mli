exception TyError

(*
 * the type of substitution
 *)
type subst

(*
 * the type of constraints
 *   a list of equations t1 = t2 for types t1 and t2
 *)
type constraints = (TySyntax.ty * TySyntax.ty) list

(*
 * return the most general unifier of the constraints
 * raise TyError if unification fails
 *)
val ty_unify : constraints -> subst

(*
 * apply the substitution to the type
 *)
val ty_subst : subst -> TySyntax.ty -> TySyntax.ty

type type_schema = TySyntax.tyvar list * TySyntax.ty

type type_env = (Syntax.name * type_schema) list (* 型環境 *)

val generalize : type_env -> TySyntax.ty -> type_schema

val instantiate : type_schema -> TySyntax.ty

val tysc_subst : subst -> type_schema -> type_schema (* type_schemaへの代入 *)

val tysc_subst_env : subst -> type_env -> type_env (* 型環境への代入 *)
