open Syntax
open TySyntax
open ConstraintSolver

exception EvalErr
exception Unbound of string
exception Div_by_Zero
exception TyError

let empty_env = []
let extend x v env = (x,v) :: env

let rec lookup x env =
  try List.assoc x env with Not_found -> raise (Unbound x)

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EVar x ->
    lookup x env
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise EvalErr)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalErr)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise EvalErr)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> if i2 = 0 then raise Div_by_Zero else VInt (i1 / i2)
     | _ -> raise EvalErr)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | _ -> raise EvalErr)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | _ -> raise EvalErr)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> raise EvalErr)
  | EOr (e1,e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool b1, VBool b2 -> VBool (b1 || b2)
     | _ -> raise EvalErr)
  | EAnd (e1,e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool b1, VBool b2 -> VBool (b1 && b2)
     | _ -> raise EvalErr)
  | EXor (e1,e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VBool b1, VBool b2 -> VBool (b1 <> b2)
     | _ -> raise EvalErr)
  | ENeq (e1,e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VBool (i1 != i2)
     | VBool b1, VBool b2 -> VBool (b1 != b2)
     | _ -> raise EvalErr)
  | ELet (x,e1,e2) ->
    let v1 = eval_expr env e1 in
    let newenv = extend x v1 env in
    eval_expr newenv e2
  | EFun (xs,e) -> VFun (xs,e,ref env)
  | EApp (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
      | VFun (xs,e,oenv) -> 
        (match xs with 
         | x::y::zs -> let newenv = extend x v2 !oenv in VFun (y::zs,e,ref newenv)
         | x::[] -> oenv := extend x v2 !oenv; eval_expr !oenv e
         | _ -> raise EvalErr)
      | _ -> raise EvalErr)
  | ELetRec (xs,e2) -> 
    let oenv = ref env in 
    let rec eval_letrec xs e2 =
      match xs with
      | (f,ys,e1)::zs -> 
        let v = VFun (ys,e1,oenv) in 
        oenv := extend f v !oenv; eval_letrec zs e2 
      | [] -> eval_expr !oenv e2 
    in
    eval_letrec xs e2
  | ELetFun (f,xs,e1,e2) -> 
    let v1 = VFun (xs,e1,ref env) in
    let newenv = extend f v1 env in
    eval_expr newenv e2
  | ETuple es -> (* tuple *)
    let vs = List.map (eval_expr env) es in
    VTuple vs
  | EList es -> (* list *)
    let vs = List.map (eval_expr env) es in
    VList vs
  | EAppend (e1,el) -> (* listへの要素の追加 *)
    let v1 = eval_expr env e1 in
    let vl = eval_expr env el in
    (match vl with
     | VList vs -> VList (v1::vs)
     | _ -> raise EvalErr)
  | EMatch (e,cases) ->
    let v = eval_expr env e in
    let rec eval_match v cases = 
      match cases with
      | (p1,e1)::nc -> 
        (match find_match p1 v with
         | Some c -> eval_expr (c@env) e1
         | None -> eval_match v nc)
      | [] -> raise EvalErr
    in
    eval_match v cases

let rec infer_expr tyenv e = 
  match e with
  | EConstInt i ->
    (TyInt,[])
  | EConstBool b ->
    (TyBool,[])
  | EVar x -> 
    let tysc = lookup x tyenv in
    let t = instantiate tysc in
    (t,[])
  | EAdd (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt, (t1,TyInt)::(t2,TyInt)::c1@c2)
  | ESub (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt, (t1,TyInt)::(t2,TyInt)::c1@c2)
  | EMul (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt, (t1,TyInt)::(t2,TyInt)::c1@c2)
  | EDiv (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyInt, (t1,TyInt)::(t2,TyInt)::c1@c2)
  | EEq (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool, (t1,TyInt)::(t2,TyInt)::c1@c2)
  | ELt (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool, (t1,TyInt)::(t2,TyInt)::c1@c2)
  | EIf (e1,e2,e3) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    let (t3,c3) = infer_expr tyenv e3 in
    (t2, (t1,TyBool)::(t2,t3)::c1@c2@c3)
  | EOr (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool, (t1,TyBool)::(t2,TyBool)::c1@c2)
  | EAnd (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool, (t1,TyBool)::(t2,TyBool)::c1@c2)
  | EXor (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool, (t1,TyBool)::(t2,TyBool)::c1@c2)
  | ENeq (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    (TyBool, (t1,TyBool)::(t2,TyBool)::c1@c2)
  | ELet (x,e1,e2) -> 
    let (t1,c1) = infer_expr tyenv e1 in
    let sg = ty_unify c1 in
    let nt = ty_subst sg t1 in
    let newtyenv = tysc_subst_env sg tyenv in 
    let tysc = generalize newtyenv nt in 
    let (t2,c2) = infer_expr (extend x tysc newtyenv) e2 in 
    (t2, c1@c2)
  | EFun (xs,e) ->
    let rec infer_fun tyenv xs tyvars =
      match xs with
      | x::ys -> 
        let a = new_tyvar () in
        let newtyenv = extend x ([],(TyVar a)) tyenv in
        let newvars = a::tyvars in
        infer_fun newtyenv ys newvars
      | [] -> (tyenv,tyvars)
    in
    let (newtyenv,tyvars) = infer_fun tyenv xs [] in
    let (t,c) = infer_expr newtyenv e in
    let rec infer_fun_vars tyvars t =
      match tyvars with
      | a::zs -> TyFun (TyVar a, infer_fun_vars zs t)
      | [] -> t
    in
    (infer_fun_vars (List.rev tyvars) t, c) 
  | EApp (e1,e2) ->
    let (t1,c1) = infer_expr tyenv e1 in
    let (t2,c2) = infer_expr tyenv e2 in
    let a = new_tyvar () in
    (TyVar a, (t1,TyFun (t2, TyVar a))::c1@c2) 
  | ELetRec (xs,e2) ->
    let rec infer_letrec1 xs fun_tyenv vars = 
      let infer_letrec2 f ys = 
        let b = new_tyvar () in
        let rec infer_fun vars_tyenv ys tyvars =
          match ys with
          | y::zs -> 
            let a = new_tyvar () in
            let newvars_tyenv = extend y ([],(TyVar a)) vars_tyenv in
            let newvars = a::tyvars in
            infer_fun newvars_tyenv zs newvars
          | [] -> (vars_tyenv,tyvars)
        in
        let rec infer_fun_vars tyvars t =
          match tyvars with
          | a::zs -> TyFun (TyVar a, infer_fun_vars zs t)
          | [] -> t
        in
        let (newvars_tyenv,tyvars) = infer_fun [] ys [] in
        let ts = infer_fun_vars (List.rev tyvars) (TyVar b) in
        (b,ts,newvars_tyenv)
      in
      (match xs with
       | (f,ys,e1)::zs -> 
         let (b,ts,vars_tyenv) = infer_letrec2 f ys in
         let newfun_tyenv = extend f ([],ts) fun_tyenv in
         let newvars = (e1,b,vars_tyenv)::vars in
         infer_letrec1 zs newfun_tyenv newvars
       | [] -> (fun_tyenv,vars))
    in
    let (fun_tyenv,vars) = infer_letrec1 xs [] [] in
    let rec infer_letrec3 vars c =
      match vars with
      | (e1,b,vars_tyenv)::ns -> 
        let (t1,c1) = infer_expr (vars_tyenv@fun_tyenv@tyenv) e1 in
        infer_letrec3 ns ((t1,TyVar b)::c1@c)
      | [] -> c
    in
    let c = infer_letrec3 vars [] in
    let sg = ty_unify c in
    let tyscs = ref [] in
    let otyenv = tysc_subst_env sg tyenv in 
    let newtyenv = ref otyenv in
    let rec infer_recdecl fun_tyenv = 
      match fun_tyenv with
      | (f,(_,ts))::ns -> 
        let nts = ty_subst sg ts in
        let tysc = generalize otyenv nts in 
        tyscs := tysc::!tyscs; newtyenv := extend f tysc !newtyenv;
        infer_recdecl ns
      | [] -> ()
    in
    infer_recdecl (List.rev fun_tyenv);
    let (t2,c2) = infer_expr (!newtyenv) e2 in 
    (t2, c2@c)
  | ELetFun (f,xs,e1,e2) -> 
    let (ts,c1) = infer_expr tyenv (EFun (xs,e1)) in
    let sg = ty_unify c1 in
    let nts = ty_subst sg ts in 
    let newtyenv = tysc_subst_env sg tyenv in 
    let tysc = generalize newtyenv nts in 
    let (t2,c2) = infer_expr (extend f tysc newtyenv) e2 in 
    (t2, c1@c2)
  | ETuple es -> (* tuple *)
    let rec infer_tuple es res1 res2 =
      match es with
      | e::ns -> 
        let (t,c) = infer_expr tyenv e in
        infer_tuple ns (t::res1) (c@res2)
      | [] -> (res1, res2)
    in
    let (ts,cs) = infer_tuple es [] [] in
    (TyTuple (List.rev ts), cs)
  | EList es -> (* list *)
    let rec infer_list1 es res1 res2 =
      match es with
      | e::ns -> 
        let (t,c) = infer_expr tyenv e in
        infer_list1 ns (t::res1) (c@res2)
      | [] -> (res1, res2)
    in
    let rec infer_list2 ts res = 
      match ts with
      | t1::t2::ns -> infer_list2 (t2::ns) ((t1,t2)::res)
      | _ -> res
    in
    let (ts,cs) = infer_list1 es [] [] in
    (match ts with
     | t::ns -> (TyList t, (infer_list2 ts [])@cs)
     | [] -> 
       let a = new_tyvar () in
       (TyList (TyVar a), []))
  | EAppend (e1,el) -> (* listへの要素の追加 *)
    let (t1,c1) = infer_expr tyenv e1 in
    let (tl,cl) = infer_expr tyenv el in
    (TyList t1, (TyList t1, tl)::c1@cl)
  | EMatch (e,cases) ->
    let (t,c) = infer_expr tyenv e in
    let rec pattern_type p =
      match p with
      | PInt _ -> (TyInt, [], [])
      | PBool _ -> (TyBool, [], [])
      | PVar x -> let a = new_tyvar () in (TyVar a, [], [(x,([],TyVar a))])
      | PTuple ps -> (* tuple *)
        let ts = ref [] in
        let cs = ref [] in
        let newtyenv = ref [] in
        let rec pattern_tuple ps =
          match ps with
          | p1::nps -> 
            let (t1,c1,tyenv1) = pattern_type p1 in
            ts := t1::!ts; cs := c1@(!cs); newtyenv := tyenv1@(!newtyenv); pattern_tuple nps
          | [] -> ()
        in
        pattern_tuple ps;
        (TyTuple !ts, !cs, !newtyenv)
      | PNil -> let a = new_tyvar () in (TyList (TyVar a), [], []) 
      | PCons (p1,p2) -> 
        let (t1,c1,tyenv1) = pattern_type p1 in
        let (t2,c2,tyenv2) = pattern_type p2 in
        let a = new_tyvar () in
        (TyList (TyVar a), (TyVar a, t1)::(TyList (TyVar a), t2)::c1@c2, tyenv1@tyenv2)
      | PWild -> (TyVar (new_tyvar ()), [], []) (* 型も制約もなし *)
    in
    let infer_match1 (p1,e1) =  
      let (t1,c1,newtyenv) = pattern_type p1 in 
      let (t1',c1') = infer_expr (newtyenv@tyenv) e1 in
      (t1,t1',c1@c1')
    in
    let res = List.map infer_match1 cases in
    let a = new_tyvar () in
    let rec infer_match2 res =
      match res with
      | (t1,t1',c1)::(t2,t2',c2)::ns -> 
        (t1,t2)::(t1',t2')::c1@(infer_match2 ((t2,t2',c2)::ns))
      | (t1,t1',c1)::[] ->
        (t,t1)::(TyVar a,t1')::c1@c
      | [] -> c
    in
    (TyVar a, infer_match2 res)
        

let rec eval_command env cmd =
  match cmd with
  | CExp e -> 
    let v = eval_expr env e in
    (["-"], env, [v])
  | CDecl (x,e) -> 
    let v = eval_expr env e in 
    let newenv = extend x v env in
    (["val " ^ x], newenv, [v])
  | CRecDecl xs -> 
    let oenv = ref env in
    let ids = ref [] in 
    let values = ref [] in
    let rec eval_recdecl xs = 
      match xs with
      | (f,ys,e)::zs ->
        let v = VFun (ys,e,oenv) in 
        oenv := extend f v !oenv; ids := !ids@[("val " ^ f)]; values := !values@[v]; eval_recdecl zs
      | [] -> ()
    in
    eval_recdecl xs; (!ids, !oenv@env, !values) 
  | CFunDecl (f,xs,e) -> 
    let v = VFun (xs,e,ref env) in
    let newenv = extend f v env in
    (["val " ^ f], newenv, [v])

let rec infer_command tyenv cmd = 
  match cmd with
  | CExp e -> 
    let (t,c) = infer_expr tyenv e in
    let sg = ty_unify c in
    let nt = ty_subst sg t in
    let newtyenv = tysc_subst_env sg tyenv in 
    let tysc = generalize newtyenv nt in 
    ([tysc], tyenv)
  | CDecl (x,e) ->
    let (t,c) = infer_expr tyenv e in
    let sg = ty_unify c in
    let nt = ty_subst sg t in
    let newtyenv = tysc_subst_env sg tyenv in 
    let tysc = generalize newtyenv nt in 
    ([tysc], extend x tysc tyenv) 
  | CRecDecl xs ->
    let rec infer_letrec1 xs fun_tyenv vars = 
      let infer_letrec2 f ys = 
        let b = new_tyvar () in
        let rec infer_fun vars_tyenv ys tyvars =
          match ys with
          | y::zs -> 
            let a = new_tyvar () in
            let newvars_tyenv = extend y ([],(TyVar a)) vars_tyenv in
            let newvars = a::tyvars in
            infer_fun newvars_tyenv zs newvars
          | [] -> (vars_tyenv,tyvars)
        in
        let rec infer_fun_vars tyvars t =
          match tyvars with
          | a::zs -> TyFun (TyVar a, infer_fun_vars zs t)
          | [] -> t
        in
        let (newvars_tyenv,tyvars) = infer_fun [] ys [] in
        let ts = infer_fun_vars (List.rev tyvars) (TyVar b) in
        (b,ts,newvars_tyenv)
      in
      (match xs with
       | (f,ys,e1)::zs -> 
         let (b,ts,vars_tyenv) = infer_letrec2 f ys in
         let newfun_tyenv = extend f ([],ts) fun_tyenv in
         let newvars = (e1,b,vars_tyenv)::vars in
         infer_letrec1 zs newfun_tyenv newvars
       | [] -> (fun_tyenv,vars))
    in
    let (fun_tyenv,vars) = infer_letrec1 xs [] [] in
    let rec infer_letrec3 vars c =
      match vars with
      | (e1,b,vars_tyenv)::ns -> 
        let (t1,c1) = infer_expr (vars_tyenv@fun_tyenv@tyenv) e1 in
        infer_letrec3 ns ((t1,TyVar b)::c1@c)
      | [] -> c
    in
    let c = infer_letrec3 vars [] in
    let sg = ty_unify c in
    let tyscs = ref [] in
    let otyenv = tysc_subst_env sg tyenv in
    let newtyenv = ref otyenv in
    let rec infer_recdecl fun_tyenv = 
      match fun_tyenv with
      | (f,(_,ts))::ns -> 
        let nts = ty_subst sg ts in 
        let tysc = generalize otyenv nts in 
        tyscs := tysc::!tyscs; newtyenv := extend f tysc !newtyenv;
        infer_recdecl ns
      | [] -> ()
    in
    infer_recdecl (List.rev fun_tyenv); 
    (!tyscs, !newtyenv)
  | CFunDecl (f,xs,e) ->
    let (ts,c) = infer_expr tyenv (EFun (xs,e)) in
    let sg = ty_unify c in
    let nts = ty_subst sg ts in 
    let newtyenv = tysc_subst_env sg tyenv in 
    let tysc = generalize newtyenv nts in
    ([tysc], extend f tysc newtyenv)

