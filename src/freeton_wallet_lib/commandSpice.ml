open Ezcmd.V2
open EZCMD.TYPES

type todo =
  GenerateOCamlInterface of string

let generate_function_interface ~oc fonction =
  let open Ton_types.ABI in
  Printf.fprintf oc "%s\n" fonction.fun_name

let generate_ocaml_interface
      ~name
      ~(abi : Ton_types.ABI.contract)
      ~(oc : out_channel) =
  Printf.fprintf oc "(* OCaml Interface for contract %s*)\n" name;
  List.iter (generate_function_interface ~oc) abi.functions


let action ~todo (* ~force *)  =
     match todo with
     | GenerateOCamlInterface contract ->
        begin
          let abifile =
            try Misc.get_contract_abifile contract with
            | _ -> Error.raise "Unknown contract %S" contract in
          let abi = Ton_sdk.ABI.read abifile in
          let oc = stderr in
          generate_ocaml_interface ~name:contract ~abi ~oc
        end

let cmd =
  let set_todo, with_todo = Misc.todo_arg () in
  let has_todo = ref false in
  let set_todo todo =
    has_todo := true;
    set_todo todo
  in
  let can_skip_todo = ref false in
  (* let force = ref false in *)
  EZCMD.sub
    "spice"
    (fun () ->
      if !has_todo || not !can_skip_todo then
        with_todo (fun todo ->
            action
              ~todo
              (* ~force:!force *)
          )
    )
    ~args: [
      [ "generate-interface" ],
      Arg.String (fun contract ->
          set_todo "generate-interface" (GenerateOCamlInterface contract)
        ),
      EZCMD.info "CONTRACT Deploy contract CONTRACT";
    ]
    ~doc: "Generate OCaml interface for the contract given as input"
    
