open Ppxlib

let raise_errorf = Location.raise_errorf
let mkloc txt loc = { txt; loc }
let mknoloc txt = mkloc txt !Ast_helper.default_loc
let str_of_string s = mknoloc s
let pvar name = Ast_helper.Pat.var (str_of_string name)

let attr_as context =
  Attribute.declare "deriving.toString.as" context
    Ast_pattern.(single_expr_payload (estring __))
    (fun e -> e)

let constr_attr_as = attr_as Attribute.Context.constructor_declaration
let rtag_attr_as = attr_as Attribute.Context.rtag

let attr_name context =
  Attribute.declare "deriving.toString.name" context
    Ast_pattern.(single_expr_payload (estring __))
    (fun e -> e)

let constr_attr_name = attr_name Attribute.Context.constructor_declaration
let rtag_attr_name = attr_name Attribute.Context.rtag

(* [@no_args]: constructor carries a payload but the deriver ignores it.
   to_string emits [Foo _ -> "Foo"] (with [@name] / [@as] override honoured);
   of_string skips the constructor entirely (we cannot reconstruct the payload
   from the tag alone). Without [@no_args], payload-bearing constructors still
   raise "cannot be derived for variant with payload". *)
let attr_no_args context =
  Attribute.declare "deriving.toString.no_args" context
    Ast_pattern.(pstr nil)
    ()

let constr_attr_no_args = attr_no_args Attribute.Context.constructor_declaration
let rtag_attr_no_args = attr_no_args Attribute.Context.rtag

(* [@no_string_exn]: like [@no_args] but the to_string side raises instead of
   silently producing the constructor name. Useful for open-enum-style
   catch-all constructors (e.g. [`Unknown of string]) where a string projection
   has no meaningful value to return - better to fail loudly than to leak the
   literal "Unknown" downstream.
   of_string skips the constructor entirely (same as [@no_args]); the
   function still returns the full type so callers can hold an
   [`Unknown _] value built elsewhere. *)
let attr_no_string_exn context =
  Attribute.declare "deriving.toString.no_string_exn" context
    Ast_pattern.(pstr nil)
    ()

let constr_attr_no_string_exn =
  attr_no_string_exn Attribute.Context.constructor_declaration

let rtag_attr_no_string_exn = attr_no_string_exn Attribute.Context.rtag

(* [@no_string]: polymorphic-variant only. to_string raises (same as
   [@no_string_exn]); of_string both skips the case AND narrows its return
   type so the marked constructor isn't part of the result polyvariant.
   Callers cannot accidentally receive an [`Unknown _] from the parser, and
   the type system enforces handling that case via a separate code path
   (e.g. inspecting the input string before calling _of_string).
   Not supported on classic variants: OCaml has no subtype that excludes
   one constructor. *)
let attr_no_string context =
  Attribute.declare "deriving.toString.no_string" context
    Ast_pattern.(pstr nil)
    ()

let constr_attr_no_string =
  attr_no_string Attribute.Context.constructor_declaration

let rtag_attr_no_string = attr_no_string Attribute.Context.rtag

let attributes =
  [
    Attribute.T constr_attr_as;
    Attribute.T constr_attr_name;
    Attribute.T constr_attr_no_args;
    Attribute.T constr_attr_no_string_exn;
    Attribute.T constr_attr_no_string;
  ]

let get_constr_override constr =
  match Attribute.get constr_attr_as constr with
  | Some _ as s -> s
  | None -> Attribute.get constr_attr_name constr

let get_rtag_override field =
  match Attribute.get rtag_attr_as field with
  | Some _ as s -> s
  | None -> Attribute.get rtag_attr_name field

let constr_has_no_args constr =
  Option.is_some (Attribute.get constr_attr_no_args constr)

let rtag_has_no_args field =
  Option.is_some (Attribute.get rtag_attr_no_args field)

let constr_has_no_string_exn constr =
  Option.is_some (Attribute.get constr_attr_no_string_exn constr)

let rtag_has_no_string_exn field =
  Option.is_some (Attribute.get rtag_attr_no_string_exn field)

let constr_has_no_string constr =
  Option.is_some (Attribute.get constr_attr_no_string constr)

let rtag_has_no_string field =
  Option.is_some (Attribute.get rtag_attr_no_string field)

(* True for either [@no_string] or [@no_string_exn]. *)
let constr_has_any_no_string constr =
  constr_has_no_string_exn constr || constr_has_no_string constr

let rtag_has_any_no_string field =
  rtag_has_no_string_exn field || rtag_has_no_string field

let map_last_lid f = function
  | Lident name -> Lident (f name)
  | Ldot (path, name) -> Ldot (path, f name)
  | Lapply _ as lid -> lid

let argn = Printf.sprintf "a%d"
let pattn typs = List.mapi (fun i _ -> pvar (argn i)) typs
let lid_of_string s = mknoloc (Longident.parse s)
let constr name = Ast_helper.Exp.construct (lid_of_string name) None
let pconstr name = Ast_helper.Pat.construct (lid_of_string name) None

module Ppx_deriving = struct
  open Ast_helper

  type casing = OCaml | Reason

  (* Some helpers imported from ppx_deriving, inlining them here allows to remove
     the dependency on Ppx_deriving.Ast_convenience and the whole ppx_deriving package *)
  let core_type_of_type_decl { ptype_name = name; ptype_params; _ } =
    let name = mkloc (Lident name.txt) name.loc in
    Typ.constr name (List.map fst ptype_params)

  let mangle ?(fixpoint = "t") ~casing affix name =
    match (name = fixpoint, affix, casing) with
    | true, (`Prefix x | `Suffix x), _ -> x
    | false, `Prefix x, OCaml -> x ^ "_" ^ name
    | false, `Suffix x, OCaml -> name ^ "_" ^ x
    | false, `Prefix x, Reason -> x ^ String.capitalize_ascii name
    | false, `Suffix x, Reason -> name ^ String.capitalize_ascii x

  let mangle_type_decl ?fixpoint affix { ptype_name = { txt = name; _ }; _ } =
    mangle ?fixpoint affix name

  let type_of_decl type_decl =
    let loc = type_decl.ptype_loc in
    let typ = core_type_of_type_decl type_decl in
    [%type: [%t typ] -> string]

  let get_flag ~deriver attr =
    match attr with
    | None -> false
    | Some { attr_name = _; attr_payload = PStr []; attr_loc = _ } -> true
    | Some { attr_name = { txt = name; loc }; attr_payload = _; attr_loc = _ }
      ->
        raise_errorf ~loc "%s: invalid [@%s]: empty structure expected" deriver
          name

  let attr ~deriver name attrs =
    let starts prefix str =
      String.length str >= String.length prefix
      && String.sub str 0 (String.length prefix) = prefix
    in
    let attr_starts prefix attr = starts prefix attr.attr_name.txt in
    let attr_is name attr = name = attr.attr_name.txt in
    let try_prefix prefix f =
      if List.exists (attr_starts prefix) attrs then prefix ^ name else f ()
    in
    let name =
      try_prefix
        ("deriving." ^ deriver ^ ".")
        (fun () -> try_prefix (deriver ^ ".") (fun () -> name))
    in
    try Some (List.find (attr_is name) attrs) with Not_found -> None

  let attr_nobuiltin ~deriver attrs =
    attrs |> attr ~deriver "nobuiltin" |> get_flag ~deriver

  let rec remove_pervasive_lid = function
    | Lident _ as lid -> lid
    | Ldot (Lident "Pervasives", s) -> Lident s
    | Ldot (Lident "Stdlib", s) -> Lident s
    | Ldot (lid, s) -> Ldot (remove_pervasive_lid lid, s)
    | Lapply (lid, lid2) ->
        Lapply (remove_pervasive_lid lid, remove_pervasive_lid lid2)

  let remove_pervasives ~deriver typ =
    if attr_nobuiltin ~deriver typ.ptyp_attributes then typ
    else
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
      mapper#core_type typ
end

let casing_of_deriver = function
  | "toString" | "fromString" -> Ppx_deriving.Reason
  | _ -> Ppx_deriving.OCaml
