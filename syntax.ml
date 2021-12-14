open TySyntax

type name = string 

type pattern =
  | PInt  of int
  | PBool of bool
  | PVar  of name
  | PTuple of pattern list (* tuple *)
  | PNil
  | PCons of pattern * pattern
  | PWild (* ワイルドカード *)

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name 
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | EOr        of expr * expr
  | EAnd       of expr * expr
  | EXor       of expr * expr
  | ENeq       of expr * expr
  | EFun       of name list * expr
  | EApp       of expr * expr
  | ELetRec    of (name * name list * expr) list * expr
  | ELetFun    of name * name list * expr * expr
  | ETuple     of expr list (* tuple *)
  | EList      of expr list (* list *)
  | EAppend    of expr * expr (* listへの要素の追加 *)
  | EMatch     of expr * (pattern * expr) list

type value =
  | VInt  of int
  | VBool of bool 
  | VFun of name list * expr * env ref
  | VTuple of value list (* tuple *)
  | VList of value list (* list *)
and env = (name * value) list

type command =
  | CExp     of expr
  | CDecl    of name * expr
  | CRecDecl of (name * name list * expr) list
  | CFunDecl of name * name list * expr 


let rec find_match p v =
  match p, v with
  | PInt pi, VInt vi when pi = vi -> Some []
  | PBool pb, VBool vb when pb = vb -> Some []
  | PVar x, v -> Some [(x,v)]
  | PTuple ps, VTuple vs -> 
    let res = ref [] in
    let flag = ref 0 in
    let rec match_tuple ps vs =
      match ps, vs with
      | p::nps, v::nvs -> 
        let r = find_match p v in
        (match r with
         | Some c -> res := c@(!res); match_tuple nps nvs
         | None -> flag := !flag + 1)
      | [], [] -> ()
      | _ -> flag := !flag + 1
    in
    match_tuple ps vs;
    if !flag = 0 then Some !res else None
  | PNil, VList [] -> Some []
  | PCons (p1,p2), VList (v1::vs) ->
    let r1 = find_match p1 v1 in
    let r2 = find_match p2 (VList vs) in
    (match r1, r2 with
     | Some c1, Some c2 -> Some (c1@c2)
     | _ -> None)
  | PWild, v -> Some [] (* 無条件で成功 *)
  | _ -> None

				  
let print_name = print_string

let rec print_pattern p =
  match p with
  | PInt i -> print_int i
  | PBool b -> print_string (string_of_bool b)
  | PVar x -> print_string x
  | PTuple ps ->
    let rec print_patterns ps =
      match ps with
      | p1::p2::ns -> print_pattern p1; print_string ","; print_patterns (p2::ns)
      | p1::[] -> print_pattern p1;
      | [] -> ()
    in
     (print_string ("(");
      print_patterns ps;
      print_string ")")
  | PNil -> print_string "[]"
  | PCons (p1, p2) ->
     (print_pattern p1;
      print_string "::";
      print_pattern p2)
  | PWild -> print_string "_"

let rec print_value v =
  match v with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (x,e,env) -> print_string "<fun>"
  | VTuple xs ->
    print_string "(";
    let rec print_tuple xs =
      match xs with
      | x::y::ys -> print_value x; print_string ", "; print_tuple (y::ys)
      | x::[] -> print_value x; print_string ")"
      | [] -> print_string ")"
    in
    print_tuple xs
  | VList xs ->
    print_string "[";
    let rec print_list xs =
      match xs with
      | x::y::ys -> print_value x; print_string "; "; print_list (y::ys)
      | x::[] -> print_value x; print_string "]"
      | [] -> print_string "]"
    in
    print_list xs

let rec printvals ids tyscs vs = 
  match ids, tyscs, vs with
  | id::xs, (vars,ty)::ns, v::ws -> Printf.printf "%s : " id; print_type ty; Printf.printf " = "; print_value v; print_newline(); printvals xs ns ws
  | _ -> ()

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 を活用すること
*)

let rec print_strings xs = 
   match xs with
   | [] -> ()
   | x::ys -> print_string (x ^ ","); print_strings ys

let rec print_expr e =
  match e with
  | EConstInt i ->
     print_int i
  | EConstBool b ->
     print_string (string_of_bool b)
  | EVar x -> 
     print_name x
  | EAdd (e1,e2) -> 
     (print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ESub (e1,e2) -> 
     (print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EMul (e1,e2) -> 
     (print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EDiv (e1,e2) -> 
     (print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EEq (e1,e2) ->
     (print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELt (e1, e2) ->
     (print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EIf (e1,e2,e3) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ","; 
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | EOr (e1,e2) ->
     (print_string "EOr (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EAnd (e1,e2) ->
     (print_string "EAnd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EXor (e1,e2) ->
     (print_string "EXor (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ENeq (e1,e2) ->
     (print_string "ENeq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELet (x,e1,e2) ->
     (print_string ("ELet (" ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EFun (xs,e) ->
     (print_string ("EFun (");
      print_strings xs;
      print_expr e;
      print_string ")")
  | EApp (e1,e2) ->
     (print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")") 
  | ELetRec (decls,e) ->
     (print_string ("ELetRec ([");
      List.iter (fun (id,xs,e) ->
		 print_string ("(" ^ id ^ ",");
       print_strings xs;
		 print_expr e;
		 print_string ");")
		decls;
      print_string "],";
      print_expr e;
      print_string ")")
  | ELetFun (f,xs,e1,e2) ->
     (print_string ("ELetFun (" ^ f ^ ",");
      print_strings xs;
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ETuple es ->
    let rec print_exprs es =
      match es with
      | e1::e2::ns -> print_expr e1; print_string ","; print_exprs (e2::ns)
      | e1::[] -> print_expr e1;
      | [] -> ()
    in
     (print_string ("ETuple (");
      print_exprs es;
      print_string ")")
  | EList es ->
    let rec print_exprs es =
      match es with
      | e1::e2::ns -> print_expr e1; print_string ","; print_exprs (e2::ns)
      | e1::[] -> print_expr e1;
      | [] -> ()
    in
     (print_string ("EList (");
      print_exprs es;
      print_string ")")
  | EAppend (e1,el) ->
     (print_string "EAppend (";
      print_expr e1;
      print_string ",";
      print_expr el;
      print_string ")") 
  | EMatch (e, cases) ->
     (print_string "EMatch (";
      print_expr e;
      print_string ",";
      print_cases cases;
      print_string ")")
and print_cases cases =
  List.iter (fun (p, e) ->
	     print_pattern p;
	     print_string " -> ";
	     print_expr e;
	     print_string ",")
	    cases

    
let rec print_command p =       
  match p with
  | CExp e -> print_expr e
  | CDecl (x,e) ->
     (print_string ("CDecl (" ^ x ^ ",");
      print_expr e;
      print_string ")")
  | CRecDecl decls ->
     (print_string ("ERecDecl ([");
      List.iter (fun (id,xs,e) ->
		 print_string ("(" ^ id ^ ",");
       print_strings xs;
		 print_expr e;
		 print_string ");")
		decls;
      print_string "])")
  | CFunDecl (f,xs,e) ->
     (print_string ("CFunDecl (" ^ f ^ ",");
      print_strings xs;
      print_expr e;
      print_string ")")
