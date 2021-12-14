open TySyntax

exception TyError

type subst = (tyvar * ty) list

type constraints = (ty * ty) list

let rec ty_subst s t = 
  match t with
  | TyFun (t1, t2) -> TyFun (ty_subst s t1, ty_subst s t2)
  | TyTuple ts -> TyTuple (List.map (ty_subst s) ts) (* tuple *)
  | TyList t -> TyList (ty_subst s t) (* list *)
  | _ -> (match s with
          | [] -> t
          | (a,b)::ns -> if (TyVar a) = t then b else ty_subst ns t)
  
let rec lookup a res =
  match res with
  | (b,t)::ns -> if a = b then true else lookup a ns
  | [] -> false

let rec compose_sub1 s1 res =
  match s1 with
  | (a1,t1)::ns1 -> 
    if not (lookup a1 res) then compose_sub1 ns1 ((a1,t1)::res) 
    else compose_sub1 ns1 res
  | [] -> res

let compose s1 s2 =
  let rec compose_sub2 s1 s2 res =
    match s2 with
    | (a2,t2)::ns2 -> compose_sub2 s1 ns2 ((a2,(ty_subst s1 t2))::res)
    | [] -> compose_sub1 s1 res
  in
  compose_sub2 s1 s2 []

let rec lookup_ty a t =
  match t with
  | TyVar b -> if a = b then true else false
  | TyFun (t1, t2) -> lookup_ty a t1 || lookup_ty a t2
  | TyTuple ts -> (* tuple *)
    (match ts with
     | t1::nts -> lookup_ty a t1 || lookup_ty a (TyTuple nts)
     | [] -> false)
  | TyList t -> lookup_ty a t (* list *)
  | _ -> false

let rec ty_unify_sub s c res =
  match c with
  | (a,b)::nc -> 
    let na = ty_subst s a in 
    let nb = ty_subst s b in
    let nres = (na,nb)::res in
    ty_unify_sub s nc nres
  | [] -> res

let rec ty_unify c = 
  match c with
  | (s,t)::nc when s = t -> ty_unify nc
  | (TyFun (s1,t1), TyFun (s2,t2))::nc -> ty_unify ([(s1,s2);(t1,t2)]@nc)
  | (TyTuple ts1, TyTuple ts2)::nc -> (* tuple *)
    (match ts1, ts2 with
     | t1::ns1, t2::ns2 -> ty_unify ([(t1,t2);(TyTuple ns1, TyTuple ns2)]@nc)
     | [], [] -> ty_unify nc
     | _ -> raise TyError)
  | (TyVar a,t)::nc ->
    if lookup_ty a t then raise TyError 
    else compose (ty_unify (ty_unify_sub [(a,t)] nc [])) [(a,t)]
  | (t,TyVar a)::nc ->
    if lookup_ty a t then raise TyError 
    else compose (ty_unify (ty_unify_sub [(a,t)] nc [])) [(a,t)]
  | (TyList t1, TyList t2)::nc -> ty_unify ((t1,t2)::nc) (* list *)
  | [] -> []
  | _ -> raise TyError

type type_schema = tyvar list * ty

type type_env = (Syntax.name * type_schema) list 

let rec lookup_list a l =
  match l with
  | x::nl -> if a = x then true else lookup_list a nl
  | [] -> false

let rec lookup_tyenv a tyenv =
  match tyenv with
  | (x,(vars,t))::nt -> 
    if lookup_list a vars then false 
    else if lookup_ty a t then true else lookup_tyenv a nt
  | [] -> false

let rec union_list l1 l2 = 
  match l2 with 
  | a::nl2 -> if lookup_list a l1 then union_list l1 nl2 else union_list (a::l1) nl2
  | [] -> l1

let generalize tyenv t = 
  let rec generalize_sub tyenv t = 
    match t with
    | TyVar a -> if lookup_tyenv a tyenv then [] else [a] 
    | TyFun (t1, t2) -> 
      let vars1 = generalize_sub tyenv t1 in 
      let vars2 = generalize_sub tyenv t2 in
      union_list vars1 vars2
    | TyTuple ts -> (* tuple *)
      (match ts with
       | t1::nts -> 
        let vars1 = generalize_sub tyenv t1 in
        union_list vars1 (generalize_sub tyenv (TyTuple nts))
       | [] -> [])
    | TyList t -> generalize_sub tyenv t (* list *)
    | _ -> []
  in
  (generalize_sub tyenv t, t)

let instantiate tysc = 
  let (vars,t) = tysc in
  let rec instantiate_sub vars t = 
    match vars with 
    | a::nvars -> 
      let b = new_tyvar () in
      let nt = ty_subst [(a,TyVar b)] t in
      instantiate_sub nvars nt
    | [] -> t
  in
  instantiate_sub vars t

let rec tysc_subst s tysc = 
  let (vars,t) = tysc in
  (match t with
   | TyFun (t1, t2) -> 
     let (_,nt1) = tysc_subst s (vars,t1) in
     let (_,nt2) = tysc_subst s (vars,t2) in
     (vars, TyFun (nt1,nt2))
   | TyTuple ts -> (* tuple *)
     let rec tysc_subst_tuple ts res =
       match ts with
       | t1::nts -> 
         let (_,nt1) = tysc_subst s (vars,t1) in
         tysc_subst_tuple nts (nt1::res)
       | [] -> TyTuple (List.rev res)
     in
     (vars, tysc_subst_tuple ts [])
   | TyList t -> (* list *)
     let (_,nt) = tysc_subst s (vars,t) in
     (vars, TyList nt)
   | _ -> (match s with
           | [] ->  (vars,t)
           | (a,b)::ns -> 
             if lookup_list a vars then tysc_subst ns (vars,t) 
             else if t = (TyVar a) then (vars,b) else tysc_subst ns (vars,t)))

let tysc_subst_env s tyenv = 
  let rec ty_subst_env_sub s tyenv res = 
    match tyenv with
    | (x,tysc)::ntyenv -> ty_subst_env_sub s ntyenv ((x,tysc_subst s tysc)::res)
    | [] -> res
  in
  ty_subst_env_sub s (List.rev tyenv) []