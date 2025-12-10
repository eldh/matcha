open Ppxlib

(* Extract module path from a Longident *)
let rec longident_to_string = function
  | Lident s -> s
  | Ldot (lid, s) -> longident_to_string lid ^ "." ^ s
  | Lapply _ -> failwith "Lapply not supported"

(* Build a Longident for Module.field *)
let make_field_access mod_path field =
  match mod_path with
  | Lident m -> Ldot (Lident m, field)
  | Ldot _ as l -> Ldot (l, field)
  | Lapply _ -> failwith "Lapply not supported"

(* Collect type variables from a type expression *)
let rec collect_type_vars typ acc =
  match typ.ptyp_desc with
  | Ptyp_var name -> 
      if List.mem name acc then acc else name :: acc
  | Ptyp_arrow (_, t1, t2) ->
      collect_type_vars t2 (collect_type_vars t1 acc)
  | Ptyp_tuple types ->
      List.fold_left (fun acc t -> collect_type_vars t acc) acc types
  | Ptyp_constr (_, types) ->
      List.fold_left (fun acc t -> collect_type_vars t acc) acc types
  | Ptyp_poly (_, t) ->
      collect_type_vars t acc
  | _ -> acc

let component_mapper =
  object (self)
    inherit Ast_traverse.map as super

    (* Transform JSX expressions *)
    method! expression expr =
      match expr.pexp_desc with
      (* Match JSX: Component.createElement(~prop=val, ~children=[...], ()) *)
      | Pexp_apply (fn, args) -> (
          match fn.pexp_desc with
          | Pexp_ident { txt = Ldot (module_path, "createElement"); loc = _ } 
            when List.exists (fun (lbl, _) -> 
              match lbl with 
              | Labelled "children" -> true 
              | _ -> false) args ->
              (* This looks like JSX - transform it *)
              let loc = expr.pexp_loc in
              
              (* Separate children from other props *)
              let children_expr = ref None in
              let props = List.filter_map (fun (lbl, e) ->
                match lbl with
                | Labelled "children" -> 
                    (* Check if children is empty list [] or single-element list *)
                    (match e.pexp_desc with
                    | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> ()
                    | Pexp_construct ({ txt = Lident "::"; _ }, Some tuple) ->
                        (* Single or multiple children in a list *)
                        (match tuple.pexp_desc with
                        | Pexp_tuple [child; { pexp_desc = Pexp_construct ({ txt = Lident "[]"; _ }, None); _ }] ->
                            (* Single child - extract it *)
                            children_expr := Some (self#expression child)
                        | _ ->
                            (* Multiple children - pass list directly *)
                            children_expr := Some (self#expression e))
                    | _ -> children_expr := Some (self#expression e));
                    None
                | Labelled name -> 
                    Some (name, self#expression e)
                | Optional name ->
                    Some (name, self#expression e)
                | Nolabel -> None
              ) args in
              
              (* Build the props record *)
              let first_field = make_field_access module_path (
                match props with 
                | (name, _) :: _ -> name 
                | [] -> "children"
              ) in
              
              let record_fields = 
                (List.map (fun (name, value) ->
                  ({ txt = Lident name; loc }, value)
                ) props)
                @
                (match !children_expr with
                | Some children -> [({ txt = Lident "children"; loc }, children)]
                | None -> [])
              in
              
              (* If we have fields, create a record; otherwise pass unit *)
              if List.length record_fields > 0 then
                let record = 
                  Ast_builder.Default.pexp_record ~loc
                    ((let (_name, value) = List.hd record_fields in
                      ({ txt = first_field; loc }, value)) ::
                     (List.tl record_fields |> List.map (fun (name, value) ->
                       ({ txt = name.txt; loc }, value))))
                    None
                in
                Ast_builder.Default.pexp_apply ~loc fn [(Nolabel, record)]
              else
                (* No props - pass unit *)
                Ast_builder.Default.pexp_apply ~loc fn 
                  [(Nolabel, Ast_builder.Default.pexp_construct ~loc 
                    { txt = Lident "()"; loc } None)]
                    
          | _ -> super#expression expr
        )
      | _ -> super#expression expr

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_value (Nonrecursive, [ binding ])
        when List.exists
               (fun attr -> String.equal attr.attr_name.txt "component")
               binding.pvb_attributes ->
          (* Found a [@component] let make = ... *)
          let loc = item.pstr_loc in

          (* Extract the function and its labeled arguments *)
          let rec extract_args expr acc =
            match expr.pexp_desc with
            | Pexp_fun (Labelled label, _default, pat, body) ->
                let typ =
                  match pat.ppat_desc with
                  | Ppat_constraint (_, t) -> Some t
                  | _ -> None
                in
                extract_args body ((label, typ) :: acc)
            | Pexp_fun (Optional label, _default, pat, body) ->
                let typ =
                  match pat.ppat_desc with
                  | Ppat_constraint (_, t) -> Some t
                  | _ -> None
                in
                extract_args body ((label, typ) :: acc)
            | _ -> (List.rev acc, expr)
          in

          let args, body = extract_args binding.pvb_expr [] in
          
          (* Transform any JSX in the body *)
          let body = self#expression body in

          if List.length args = 0 then
            (* No labeled args - just a simple make function, return as-is but remove attribute *)
            let new_binding = { binding with 
              pvb_attributes = [];
              pvb_expr = self#expression binding.pvb_expr 
            } in
            { item with pstr_desc = Pstr_value (Nonrecursive, [ new_binding ]) }
          else
            (* Collect type variables from all argument types *)
            let type_vars = 
              List.fold_left (fun acc (_, typ) ->
                match typ with
                | Some t -> collect_type_vars t acc
                | None -> acc
              ) [] args
              |> List.rev (* Preserve order *)
            in
            
            (* Generate props record type with type parameters *)
            let props_fields =
              List.map
                (fun (label, typ) ->
                  let field_type =
                    match typ with
                    | Some t -> t
                    | None ->
                        Ast_builder.Default.ptyp_constr ~loc
                          { txt = Lident "string"; loc }
                          []
                  in
                  Ast_builder.Default.label_declaration ~loc
                    ~name:{ txt = label; loc }
                    ~mutable_:Immutable ~type_:field_type)
                args
            in

            (* Create type parameters for the props type *)
            let type_params = 
              List.map (fun var -> 
                (Ast_builder.Default.ptyp_var ~loc var, (NoVariance, NoInjectivity))
              ) type_vars
            in

            let props_type =
              Ast_builder.Default.pstr_type ~loc Nonrecursive
                [
                  Ast_builder.Default.type_declaration ~loc
                    ~name:{ txt = "props"; loc }
                    ~params:type_params ~cstrs:[] ~private_:Public
                    ~kind:(Ptype_record props_fields)
                    ~manifest:None;
                ]
            in

            (* Generate destructuring pattern for make function argument *)
            let destructure_pat =
              Ast_builder.Default.ppat_record ~loc
                (List.map
                   (fun (label, _) ->
                     ({ txt = Lident label; loc }, 
                      Ast_builder.Default.ppat_var ~loc { txt = label; loc }))
                   args)
                Closed
            in

            (* Generate make function: let make = (props) => { let {a, b, ...} = props; body } *)
            let props_pat = Ast_builder.Default.ppat_var ~loc { txt = "props"; loc } in
            let props_var = Ast_builder.Default.pexp_ident ~loc { txt = Lident "props"; loc } in
            
            let body_with_destructure =
              Ast_builder.Default.pexp_let ~loc Nonrecursive
                [Ast_builder.Default.value_binding ~loc ~pat:destructure_pat ~expr:props_var]
                body
            in
            
            let make_fun =
              Ast_builder.Default.pexp_fun ~loc Nolabel None props_pat body_with_destructure
            in
            
            let make_binding =
              Ast_builder.Default.pstr_value ~loc Nonrecursive
                [Ast_builder.Default.value_binding ~loc
                   ~pat:(Ast_builder.Default.ppat_var ~loc { txt = "make"; loc })
                   ~expr:make_fun]
            in

            (* Generate createElement: let createElement = (props) => Element.createElement(() => make(props)) *)
            let create_element_body =
              let make_call = 
                Ast_builder.Default.pexp_apply ~loc
                  (Ast_builder.Default.pexp_ident ~loc { txt = Lident "make"; loc })
                  [(Nolabel, props_var)]
              in
              let thunk =
                Ast_builder.Default.pexp_fun ~loc Nolabel None
                  (Ast_builder.Default.ppat_construct ~loc { txt = Lident "()"; loc } None)
                  make_call
              in
              Ast_builder.Default.pexp_apply ~loc
                (Ast_builder.Default.pexp_ident ~loc 
                   { txt = Ldot (Lident "Element", "createElement"); loc })
                [(Nolabel, thunk)]
            in
            
            let create_element_fun =
              Ast_builder.Default.pexp_fun ~loc Nolabel None props_pat create_element_body
            in
            
            let create_element_binding =
              Ast_builder.Default.pstr_value ~loc Nonrecursive
                [Ast_builder.Default.value_binding ~loc
                   ~pat:(Ast_builder.Default.ppat_var ~loc { txt = "createElement"; loc })
                   ~expr:create_element_fun]
            in

            (* Return: type props, let make, let createElement *)
            Ast_builder.Default.pstr_include ~loc
              {
                pincl_mod =
                  Ast_builder.Default.pmod_structure ~loc
                    [ props_type; make_binding; create_element_binding ];
                pincl_loc = loc;
                pincl_attributes = [];
              }
      | _ -> super#structure_item item
  end

let () =
  Driver.register_transformation "ppx_component"
    ~impl:(fun str -> component_mapper#structure str)
