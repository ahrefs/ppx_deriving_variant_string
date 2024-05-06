(* Heavily based on deriving.show plugin:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_plugins/show/ppx_deriving_show.ml
*)

open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ast_builder.Default
open Lib

let sig_of_type ~casing ~deriver type_decl =
  [
    Sig.value
      (Val.mk
         (mknoloc (Ppx_deriving.mangle_type_decl ~casing (`Suffix deriver) type_decl))
         (Ppx_deriving.type_of_decl type_decl));
  ]

let rec expr_of_typ ~deriver typ =
  let loc = typ.ptyp_loc in
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | [%type: _] -> [%expr fun _ -> Ppx_deriving_runtime.Format.pp_print_string fmt "_"]
  | { ptyp_desc = Ptyp_arrow _; ptyp_loc; _ } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for arrow type %s" deriver (string_of_core_type typ)
  | { ptyp_desc = Ptyp_constr _; ptyp_loc; _ } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for core type %s" deriver (string_of_core_type typ)
  | { ptyp_desc = Ptyp_tuple _typs; ptyp_loc; _ } ->
    raise_errorf ~loc:ptyp_loc "%s cannot be derived for nested Ptyp_tuple type %s" deriver (string_of_core_type typ)
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } ->
    let cases =
      fields
      |> List.map (fun field ->
           match field.prf_desc with
           | Rtag (label, true (*empty*), []) ->
             let string =
               match Attribute.get rtag_attr_as field with
               | Some l -> l
               | None -> label.txt
             in
             Exp.case (Pat.variant label.txt None) (estring ~loc string)
           | Rtag (_label, false, [ typ ]) ->
             raise_errorf ~loc:ptyp_loc "%s cannot be derived for variant with payload %s" deriver
               (string_of_core_type typ)
           | Rinherit _typ ->
             (* todo: support inheritance *)
             raise_errorf ~loc:ptyp_loc "%s cannot be derived for variant with inheritance %s" deriver
               (string_of_core_type typ)
           | _ -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver (string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name; _ } -> [%expr [%e evar ~loc ("poly_" ^ name)] fmt]
  | { ptyp_desc = Ptyp_alias (typ, _); _ } -> expr_of_typ ~deriver typ
  | { ptyp_loc; _ } -> raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver (string_of_core_type typ)

let str_of_type ~casing ~deriver ({ ptype_loc = loc; _ } as type_decl) =
  let toString_exp =
    match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, Some manifest -> expr_of_typ ~deriver manifest
    | Ptype_variant constrs, _ ->
      let cases =
        constrs
        |> List.map (fun ({ pcd_name = { txt = name'; _ }; pcd_args; _ } as constr) ->
             let constr_name = name' in
             match Attribute.get constr_attr_as constr, pcd_args with
             | Some string, Pcstr_tuple [] -> Exp.case (pconstr name') (estring ~loc string)
             | Some _, Pcstr_tuple _ -> raise_errorf ~loc "%s cannot be derived for variant with payload" deriver
             | _, Pcstr_record _ -> raise_errorf ~loc "%s cannot be derived for variant with record payload" deriver
             | None, Pcstr_tuple [] -> Exp.case (pconstr name') (estring ~loc constr_name)
             | None, Pcstr_tuple _typs -> raise_errorf ~loc "%s cannot be derived for variant with payload" deriver)
      in
      Exp.function_ cases
    | Ptype_record _, _ -> raise_errorf ~loc "%s cannot be derived for record type" deriver
    | Ptype_abstract, None -> raise_errorf ~loc "%s cannot be derived for abstract types" deriver
    | Ptype_open, _ -> raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let toString_type = Ppx_deriving.type_of_decl type_decl in
  let toString_var = pvar (Ppx_deriving.mangle_type_decl ~casing (`Suffix deriver) type_decl) in
  [ Vb.mk (Pat.constraint_ toString_var toString_type) toString_exp ]

let str_type_decl ~casing ~deriver =
  Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt:_ (_, type_decls) ->
    [ Str.value Nonrecursive (List.concat (List.map (str_of_type ~casing ~deriver) type_decls)) ])

let sig_type_decl ~casing ~deriver =
  Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt:_ (_, type_decls) ->
    List.concat (List.map (sig_of_type ~casing ~deriver) type_decls))

let () =
  let deriver = "toString" in
  Deriving.add deriver ~str_type_decl:(str_type_decl ~casing:Reason ~deriver)
    ~sig_type_decl:(sig_type_decl ~casing:Reason ~deriver)
  |> Deriving.ignore

let () =
  let deriver = "to_string" in
  Deriving.add deriver ~str_type_decl:(str_type_decl ~casing:OCaml ~deriver)
    ~sig_type_decl:(sig_type_decl ~casing:OCaml ~deriver)
  |> Deriving.ignore
