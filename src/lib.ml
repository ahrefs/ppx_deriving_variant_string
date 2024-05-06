open Ppxlib

let raise_errorf = Location.raise_errorf

let mkloc txt loc = { txt; loc }

let mknoloc txt = mkloc txt !Ast_helper.default_loc

let str_of_string s = mknoloc s

let pvar name = Ast_helper.Pat.var (str_of_string name)

let attr_as context =
  Attribute.declare "deriving.toString.as" context Ast_pattern.(single_expr_payload (estring __)) (fun e -> e)

let constr_attr_as = attr_as Attribute.Context.constructor_declaration

let rtag_attr_as = attr_as Attribute.Context.rtag

let argn = Printf.sprintf "a%d"

let pattn typs = List.mapi (fun i _ -> pvar (argn i)) typs

let lid_of_string s = mknoloc (Longident.parse s)

let constr name = Ast_helper.Exp.construct (lid_of_string name) None

let pconstr name = Ast_helper.Pat.construct (lid_of_string name) None

module Ppx_deriving = struct
  open Ast_helper

  type casing =
    | OCaml
    | Reason

  (* Some helpers imported from ppx_deriving, inlining them here allows to remove
     the dependency on Ppx_deriving.Ast_convenience and the whole ppx_deriving package *)
  let core_type_of_type_decl { ptype_name = name; ptype_params; _ } =
    let name = mkloc (Lident name.txt) name.loc in
    Typ.constr name (List.map fst ptype_params)

  let mangle ?(fixpoint = "t") ~casing affix name =
    match name = fixpoint, affix, casing with
    | true, (`Prefix x | `Suffix x), _ -> x
    | false, `Prefix x, OCaml -> x ^ "_" ^ name
    | false, `Suffix x, OCaml -> name ^ "_" ^ x
    | false, `Prefix x, Reason -> x ^ String.capitalize_ascii name
    | false, `Suffix x, Reason -> name ^ String.capitalize_ascii x

  let mangle_type_decl ?fixpoint affix { ptype_name = { txt = name; _ }; _ } = mangle ?fixpoint affix name

  let type_of_decl type_decl =
    let loc = type_decl.ptype_loc in
    let typ = core_type_of_type_decl type_decl in
    [%type: [%t typ] -> string]

  let get_flag ~deriver attr =
    match attr with
    | None -> false
    | Some { attr_name = _; attr_payload = PStr []; attr_loc = _ } -> true
    | Some { attr_name = { txt = name; loc }; attr_payload = _; attr_loc = _ } ->
      raise_errorf ~loc "%s: invalid [@%s]: empty structure expected" deriver name

  let attr ~deriver name attrs =
    let starts prefix str =
      String.length str >= String.length prefix && String.sub str 0 (String.length prefix) = prefix
    in
    let attr_starts prefix attr = starts prefix attr.attr_name.txt in
    let attr_is name attr = name = attr.attr_name.txt in
    let try_prefix prefix f = if List.exists (attr_starts prefix) attrs then prefix ^ name else f () in
    let name = try_prefix ("deriving." ^ deriver ^ ".") (fun () -> try_prefix (deriver ^ ".") (fun () -> name)) in
    try Some (List.find (attr_is name) attrs) with Not_found -> None

  let attr_nobuiltin ~deriver attrs = attrs |> attr ~deriver "nobuiltin" |> get_flag ~deriver

  let rec remove_pervasive_lid = function
    | Lident _ as lid -> lid
    | Ldot (Lident "Pervasives", s) -> Lident s
    | Ldot (Lident "Stdlib", s) -> Lident s
    | Ldot (lid, s) -> Ldot (remove_pervasive_lid lid, s)
    | Lapply (lid, lid2) -> Lapply (remove_pervasive_lid lid, remove_pervasive_lid lid2)

  let remove_pervasives ~deriver typ =
    if attr_nobuiltin ~deriver typ.ptyp_attributes then typ
    else (
      let mapper =
        object
          inherit Ast_traverse.map as super

          method! core_type typ =
            match super#core_type typ with
            | { ptyp_desc = Ptyp_constr (lid, l); _ } ->
              let lid = { lid with txt = remove_pervasive_lid lid.txt } in
              { typ with ptyp_desc = Ptyp_constr (lid, l) }
            | { ptyp_desc = Ptyp_class (lid, l); _ } ->
              let lid = { lid with txt = remove_pervasive_lid lid.txt } in
              { typ with ptyp_desc = Ptyp_class (lid, l) }
            | typ -> typ
        end
      in
      mapper#core_type typ)
end
