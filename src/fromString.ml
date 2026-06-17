(* Heavily based on deriving.show plugin:
   https://github.com/ocaml-ppx/ppx_deriving/blob/35dfd4ad83e58bcfbc03b564e5fe6df06b6cbdd7/src_plugins/show/ppx_deriving_show.ml
*)

open Ppxlib
open Asttypes
open Parsetree
open Ast_helper
open Ast_builder.Default
open Lib

(* Compute the result polyvariant type for [type_decl]. When all rows are
   acceptable, returns the type-name constructor (e.g. [reports_feature]).
   When some rows are marked [@no_string] (poly only), returns an inline
   polyvariant excluding those rows - callers then can't see an
   [`Unknown _] value via _of_string. *)
let result_type_of_decl type_decl =
  match (type_decl.ptype_kind, type_decl.ptype_manifest) with
  | ( Ptype_abstract,
      Some ({ ptyp_desc = Ptyp_variant (fields, _, _); _ } as manifest) ) ->
      let keep_field field =
        match field.prf_desc with
        | Rtag (_, _, _) -> not (rtag_has_no_string field)
        | Rinherit _ -> true
      in
      let kept = List.filter keep_field fields in
      if List.length kept = List.length fields then
        Ppx_deriving.core_type_of_type_decl type_decl
      else { manifest with ptyp_desc = Ptyp_variant (kept, Closed, None) }
  | _ -> Ppx_deriving.core_type_of_type_decl type_decl

let type_of_decl type_decl =
  let loc = type_decl.ptype_loc in
  let typ = result_type_of_decl type_decl in
  [%type: string -> [%t typ] option]

let type_of_decl_exn type_decl =
  let loc = type_decl.ptype_loc in
  let typ = result_type_of_decl type_decl in
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
            if rtag_has_no_args field || rtag_has_any_no_string field then
              `Skipped
            else
              raise_errorf ~loc:ptyp_loc
                "%s cannot be derived for variant with payload %s" deriver
                (string_of_core_type typ)
        | Rtag (_, false, _) ->
            if rtag_has_no_args field || rtag_has_any_no_string field then
              `Skipped
            else
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
                 if constr_has_no_string constructor_declaration then
                   raise_errorf ~loc
                     "%s: [@no_string] is polymorphic-variant only; use \
                      [@no_string_exn] on classic variants"
                     deriver;
                 let no_args = constr_has_no_args constructor_declaration in
                 let no_string_exn =
                   constr_has_no_string_exn constructor_declaration
                 in
                 let skip = no_args || no_string_exn in
                 let name_to_use =
                   match get_constr_override constructor_declaration with
                   | Some s -> s
                   | None -> constr_name
                 in
                 match pcd_args with
                 | Pcstr_tuple [] ->
                     `Case
                       (Exp.case (pstring ~loc name_to_use)
                          [%expr Some [%e constr name']])
                 | Pcstr_tuple _ when skip -> `Skipped
                 | Pcstr_record _ when skip -> `Skipped
                 | Pcstr_record _ ->
                     raise_errorf ~loc
                       "%s cannot be derived for variant with record payload"
                       deriver
                 | Pcstr_tuple _ ->
                     raise_errorf ~loc
                       "%s cannot be derived for variant with payload" deriver)
          |> List.filter_map (function `Case c -> Some c | `Skipped -> None)
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
