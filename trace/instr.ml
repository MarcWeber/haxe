(*
 * This file is part of Bisect.
 * adopted original code at: http://bisect.x9c.fr/index.html
 * license: GPLv3
 *)

open Camlp4.PreCast;

(* Returns the identifier as a string. *)
value rec string_of_ident =
  fun
  [ Ast.IdAcc _ _ id -> string_of_ident id
  | Ast.IdApp _ id _ -> string_of_ident id
  | Ast.IdLid _ s -> s
  | Ast.IdUid _ s -> s
  | Ast.IdAnt _ s -> s ];


(* Returns the identifier of an application, as a string. *)
value rec ident_of_app e =
  match e with
  [ Ast.ExId _ id -> string_of_ident id
  | Ast.ExApp _ e' _ -> ident_of_app e'
  | _ -> "" ];


(* Tests whether the passed expression is a bare mapping,
   or starts with a bare mapping (if the expression is a sequence).
   Used to avoid unnecessary marking. *)
value rec is_bare_mapping =
  fun
  [ Ast.ExFun _ -> True
  | Ast.ExMat _ -> True
  | Ast.ExSeq _ e' -> is_bare_mapping e'
  | _ -> False ];

(* To be raised when an offset is already marked. *)
exception Already_marked;

value decorate_this_expr e =
    let buf = Buffer.create 42 in
    let _loc = Ast.loc_of_expr e in
    (* Camlp4.PreCast.Ast.loc.dump_long *)
    let () = Format.bprintf buf "%a\n" Loc.dump _loc in
    let s = Buffer.contents buf in
    <:expr< (  print_string $`str:s$; $e$) >>;

type point_kind =
  [ Binding
  | Sequence
  | For
  | If_then
  | Try
  | While
  | Match
  | Class_expr
  | Class_init
  | Class_meth
  | Class_val
  | Toplevel_expr
  | Lazy_operator

  ];

value wrap_expr k e =
  let loc = Ast.loc_of_expr e in
  let dont_wrap = (is_bare_mapping e) || (Loc.is_ghost loc) in
  if dont_wrap then
    e
  else
    decorate_this_expr e;

(* Wraps the "toplevel" expressions of a binding, using "wrap_expr". *)
value rec wrap_binding = fun
  [ Ast.BiAnd loc b1 b2 ->
      Ast.BiAnd (loc, (wrap_binding b1), (wrap_binding b2))
  | Ast.BiEq (loc, p, Ast.ExTyc (_, e, t)) ->
      Ast.BiEq (loc, Ast.PaTyc (loc, p, t), (wrap_expr Binding e))
  | Ast.BiEq (loc, p, e) ->
      Ast.BiEq (loc, p, (wrap_expr Binding e))
  | b -> b ];

(* Wraps a sequence. *)
value rec wrap_seq k = fun
  [ Ast.ExSem (loc, e1, e2) ->
      Ast.ExSem (loc, (wrap_seq k e1), (wrap_seq Sequence e2))
  | Ast.ExNil loc ->
      Ast.ExNil loc
  | x ->
      wrap_expr k x ];

(* Tests whether the passed expression is an if/then construct that has no
   else branch. *)
value has_no_else_branch e =
  match e with
  [ <:expr< if $_$ then $_$ else $(<:expr< () >> as e')$ >> ->
      Ast.loc_of_expr e = Ast.loc_of_expr e'
  | _ ->
      False ];

(* The actual "instrumenter" object, marking expressions. *)
value instrument =
  object
    inherit Ast.map as super;

    method! class_expr ce =
      match super#class_expr ce with
      [ Ast.CeApp (loc, ce, e) ->
          Ast.CeApp (loc, ce, (wrap_expr Class_expr e))
      | x -> x ];

    method! class_str_item csi =
      match super#class_str_item csi with
      [ Ast.CrIni (loc, e) ->
          Ast.CrIni (loc, (wrap_expr Class_init e))
      | Ast.CrMth (loc, id, ovr, priv, e, ct) ->
          Ast.CrMth (loc, id, ovr, priv, (wrap_expr Class_meth e), ct)
      | Ast.CrVal (loc, id, ovr, mut, e) ->
          Ast.CrVal (loc, id, ovr, mut, (wrap_expr Class_val e))
      | x -> x ];

    method! expr e =
      let e' = super#expr e in
      match e' with
      [ Ast.ExApp loc (Ast.ExApp loc' e1 e2) e3 ->
          (match ident_of_app e1 with
          [ "&&" | "&" | "||" | "or" ->
              Ast.ExApp (loc, (Ast.ExApp (loc',
                                          e1,
                                          (wrap_expr Lazy_operator e2))),
                               (wrap_expr Lazy_operator e3))
          | _ -> e'])
      | Ast.ExFor (loc, id, e1, e2, dir, e3) ->
          Ast.ExFor (loc, id, e1, e2, dir, (wrap_seq For e3))
      | Ast.ExIfe (loc, e1, e2, e3) ->
          if has_no_else_branch e then
            Ast.ExIfe (loc, e1, (wrap_expr If_then e2), e3)
          else
            Ast.ExIfe (loc,
                       e1,
                       (wrap_expr If_then e2),
                       (wrap_expr If_then e3))
      | Ast.ExLet (loc, r, bnd, e1) ->
          Ast.ExLet (loc, r, bnd, (wrap_expr Binding e1))
      | Ast.ExSeq (loc, e) ->
          Ast.ExSeq (loc, (wrap_seq Sequence e))
      | Ast.ExTry (loc, e1, h) ->
          Ast.ExTry (loc, (wrap_seq Try e1), h)
      | Ast.ExWhi (loc, e1, e2) ->
          Ast.ExWhi (loc, e1, (wrap_seq While e2))
      | x ->
          x ];

    method! match_case mc =
      match super#match_case mc with
      [ Ast.McArr (loc, p1, e1, e2) ->
          Ast.McArr (loc, p1, e1, (wrap_expr Match e2))
      | x ->
          x ];

    method! str_item si =
      match si with
      (*       [ Ast.StVal loc _ (Ast.BiEq _ (Ast.PaId _ x) _) when
                   contains (Loc.file_name loc) (string_of_ident x) -> si *)
      
      [ _ ->
          match super#str_item si with
          [ Ast.StDir loc id e ->
              Ast.StDir loc id (wrap_expr Toplevel_expr e)
          | Ast.StExp loc e -> Ast.StExp loc (wrap_expr Toplevel_expr e)
          | Ast.StVal loc rc bnd -> Ast.StVal loc rc (wrap_binding bnd)
          | x -> x ] ];

  end;


value () =
  AstFilters.register_str_item_filter instrument#str_item;
