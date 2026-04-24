(* Heavily based on deriving.show plugin:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_plugins/show/ppx_deriving_show.ml
*)

open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ast_builder.Default
open Lib

let type_of_decl type_decl =
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  [%type: string -> [%t typ] option]

let type_of_decl_exn type_decl =
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  [%type: string -> [%t typ]]

let exn_deriver_name ~casing ~deriver =
  match (casing : Ppx_deriving.casing) with
  | OCaml -> deriver ^ "_exn"
  | Reason -> deriver ^ "Exn"

let sig_of_type ~casing ~deriver type_decl =
  let exn_name = exn_deriver_name ~casing ~deriver in
  [
    Sig.value
      (Val.mk
         (mknoloc
            (Ppx_deriving.mangle_type_decl ~casing (`Suffix deriver) type_decl))
         (type_of_decl type_decl));
    Sig.value
      (Val.mk
         (mknoloc
            (Ppx_deriving.mangle_type_decl ~casing (`Suffix exn_name) type_decl))
         (type_of_decl_exn type_decl));
  ]

let rec expr_of_typ ~deriver ~outer_typ typ =
  let loc = typ.ptyp_loc in
  let typ = Ppx_deriving.remove_pervasives ~deriver typ in
  match typ with
  | [%type: _] ->
      [%expr fun _ -> Ppx_deriving_runtime.Format.pp_print_string fmt "_"]
  | { ptyp_desc = Ptyp_arrow _; ptyp_loc; _ } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for arrow type %s"
        deriver (string_of_core_type typ)
  | { ptyp_desc = Ptyp_constr _; ptyp_loc; _ } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for core type %s" deriver
        (string_of_core_type typ)
  | { ptyp_desc = Ptyp_tuple _typs; ptyp_loc; _ } ->
      raise_errorf ~loc:ptyp_loc
        "%s cannot be derived for nested Ptyp_tuple type %s" deriver
        (string_of_core_type typ)
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } ->
      let classify field =
        match field.prf_desc with
        | Rtag (label, true (*empty*), []) ->
            let string =
              match get_rtag_override field with
              | Some l -> l
              | None -> label.txt
            in
            `NonPayload
              (Exp.case
                 (Ast_helper.Pat.constant (Ast_helper.Const.string string))
                 [%expr Some [%e Exp.variant label.txt None]])
        | Rtag (_label, false, [ typ ]) ->
            raise_errorf ~loc:ptyp_loc
              "%s cannot be derived for variant with payload %s" deriver
              (string_of_core_type typ)
        | Rtag (_, false, _) ->
            raise_errorf ~loc:ptyp_loc
              "%s cannot be derived for variant constructor with multiple \
               payloads"
              deriver
        | Rinherit { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, _); _ } ->
            let fn_lid =
              map_last_lid
                (fun name ->
                  Ppx_deriving.mangle
                    ~casing:(casing_of_deriver deriver)
                    (`Suffix deriver) name)
                lid
            in
            `Inherit (Exp.ident { txt = fn_lid; loc = ptyp_loc })
        | Rinherit _typ ->
            raise_errorf ~loc:ptyp_loc
              "%s cannot be derived for non-type inheritance %s" deriver
              (string_of_core_type typ)
        | _ ->
            raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver
              (string_of_core_type typ)
      in
      let classified = List.map classify fields in
      let non_payload_cases =
        List.filter_map
          (function `NonPayload c -> Some c | _ -> None)
          classified
      in
      let inherit_fn =
        List.find_opt (function `Inherit _ -> true | _ -> false) classified
        |> Option.map (function `Inherit f -> f | _ -> failwith "impossible")
      in
      let catch_all =
        match inherit_fn with
        | None -> [ Exp.case [%pat? _] [%expr None] ]
        | Some fn_expr ->
            let loc = ptyp_loc in
            let expr =
              match outer_typ with
              | Some ot ->
                  [%expr
                    Option.map
                      (fun v -> [%e Exp.coerce (evar ~loc "v") None ot])
                      ([%e fn_expr] s)]
              | None ->
                  [%expr
                    match [%e fn_expr] s with Some v -> Some v | None -> None]
            in
            [ Exp.case (pvar "s") expr ]
      in
      Exp.function_ (non_payload_cases @ catch_all)
  | { ptyp_desc = Ptyp_var name; _ } ->
      [%expr [%e evar ~loc ("poly_" ^ name)] fmt]
  | { ptyp_desc = Ptyp_alias (typ, _); _ } ->
      expr_of_typ ~deriver ~outer_typ typ
  | { ptyp_loc; _ } ->
      raise_errorf ~loc:ptyp_loc "%s cannot be derived for %s" deriver
        (string_of_core_type typ)

let str_of_type ~casing ~deriver ({ ptype_loc = loc; _ } as type_decl) =
  let fromString_exp =
    match (type_decl.ptype_kind, type_decl.ptype_manifest) with
    | Ptype_abstract, Some manifest ->
        let outer_typ = Some (Ppx_deriving.core_type_of_type_decl type_decl) in
        expr_of_typ ~deriver ~outer_typ manifest
    | Ptype_variant constrs, _ ->
        let cases =
          constrs
          |> List.map
               (fun
                 ({ pcd_name = { txt = name'; _ }; pcd_args; _ } as
                  constructor_declaration)
               ->
                 let constr_name = name' in
                 match
                   (get_constr_override constructor_declaration, pcd_args)
                 with
                 | Some string, Pcstr_tuple [] ->
                     Exp.case (pstring ~loc string)
                       [%expr Some [%e constr name']]
                 | Some _, Pcstr_tuple _ ->
                     raise_errorf ~loc
                       "%s cannot be derived for variant with payload" deriver
                 | _, Pcstr_record _ ->
                     raise_errorf ~loc
                       "%s cannot be derived for variant with record payload"
                       deriver
                 | None, Pcstr_tuple [] ->
                     Exp.case (pstring ~loc constr_name)
                       [%expr Some [%e constr name']]
                 | None, Pcstr_tuple _typs ->
                     raise_errorf ~loc
                       "%s cannot be derived for variant with payload" deriver)
        in
        let wildcard = Exp.case [%pat? _] [%expr None] in
        Exp.function_ (cases @ [ wildcard ])
    | Ptype_record _, _ ->
        raise_errorf ~loc "%s cannot be derived for record type" deriver
    | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for abstract types" deriver
    | Ptype_open, _ ->
        raise_errorf ~loc "%s cannot be derived for open types" deriver
  in
  let fromString_type = type_of_decl type_decl in
  let fromString_var =
    pvar (Ppx_deriving.mangle_type_decl ~casing (`Suffix deriver) type_decl)
  in
  [ Vb.mk (Pat.constraint_ fromString_var fromString_type) fromString_exp ]

let str_of_type_exn ~casing ~deriver ({ ptype_loc = loc; _ } as type_decl) =
  let exn_name = exn_deriver_name ~casing ~deriver in
  let of_string_name =
    Ppx_deriving.mangle_type_decl ~casing (`Suffix deriver) type_decl
  in
  let exn_exp =
    [%expr
      fun s ->
        match [%e Ast_helper.Exp.ident (lid_of_string of_string_name)] s with
        | Some v -> v
        | None -> invalid_arg (Printf.sprintf "%s: %s" __FUNCTION__ s)]
  in
  let exn_type = type_of_decl_exn type_decl in
  let exn_var =
    pvar (Ppx_deriving.mangle_type_decl ~casing (`Suffix exn_name) type_decl)
  in
  [ Vb.mk (Pat.constraint_ exn_var exn_type) exn_exp ]

let str_type_decl ~casing ~deriver =
  Deriving.Generator.V2.make Deriving.Args.empty ~attributes
    (fun ~ctxt:_ (_, type_decls) ->
      let main_vbs =
        List.concat (List.map (str_of_type ~casing ~deriver) type_decls)
      in
      let exn_vbs =
        List.concat (List.map (str_of_type_exn ~casing ~deriver) type_decls)
      in
      [
        pstr_value ~loc:Location.none Nonrecursive main_vbs;
        pstr_value ~loc:Location.none Nonrecursive exn_vbs;
      ])

let sig_type_decl ~casing ~deriver =
  Deriving.Generator.V2.make Deriving.Args.empty ~attributes
    (fun ~ctxt:_ (_, type_decls) ->
      List.concat (List.map (sig_of_type ~casing ~deriver) type_decls))

let () =
  let deriver = "fromString" in
  Deriving.add deriver
    ~str_type_decl:(str_type_decl ~casing:Reason ~deriver)
    ~sig_type_decl:(sig_type_decl ~casing:Reason ~deriver)
  |> Deriving.ignore

let () =
  let deriver = "of_string" in
  Deriving.add deriver
    ~str_type_decl:(str_type_decl ~casing:OCaml ~deriver)
    ~sig_type_decl:(sig_type_decl ~casing:OCaml ~deriver)
  |> Deriving.ignore
