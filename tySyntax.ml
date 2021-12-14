type tyvar = int

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyTuple of ty list (* tuple *)
  | TyList of ty (* list *)

let tyvar_count = ref 0

let new_tyvar () = tyvar_count := !tyvar_count + 1; !tyvar_count

let rec print_type t =
  match t with
  | TyInt -> print_string "int"
  | TyBool -> print_string "bool"
  | TyFun (s1,s2) -> print_string "("; print_type s1; print_string " -> "; print_type s2; print_string ")"
  | TyVar a -> print_string "'a"; print_int a
  | TyTuple ts -> (* tuple *)
    let rec print_tytuple ts =
      match ts with
      | t1::t2::nts -> print_type t1; print_string " * "; print_tytuple (t2::nts)
      | t1::[] -> print_type t1
      | [] -> print_string "unit"
    in
    print_string "("; print_tytuple ts; print_string ")"
  | TyList t -> (* list *)
    print_type t; print_string " list"



