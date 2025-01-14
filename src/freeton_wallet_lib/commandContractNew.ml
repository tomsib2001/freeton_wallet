(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES


let tab = '\t'

let create_contract name =

  CommandContractNewIntf.
    create_interface ("I" ^ name);

  let filename = name ^ ".spp" in
  if Sys.file_exists filename then
    Printf.eprintf "Skipping generation of %S: already exists\n%!" filename
  else
    let content =
      Printf.sprintf {|/*
  Implementation of contract %s
 */

pragma ton-solidity >= 0.32.0;

pragma AbiHeader expire;
pragma AbiHeader pubkey;

import "./I%s.sol";

contract %s is I%s {

  // 100 - message sender is not a custodian;
  uint64 constant EXN_AUTH_FAILED = 100 ;

  uint8 g_nvals ;                      // required number of ...
  mapping(uint256 => uint8) g_vals ;   // pubkey -> value_index

  constructor( ) public {
    require( msg.pubkey() == tvm.pubkey(), EXN_AUTH_FAILED );
    tvm.accept();
    // TODO
    g_vals[ 0 ] = 1;
  }

}

|}
        name name name name in
    EzFile.write_file filename content;
    Printf.eprintf "Contract %S generated\n%!" filename;

    let content =
      if Sys.file_exists "Makefile" then
        EzFile.read_file "Makefile"
      else
        Printf.sprintf
          {|# Auto-generated by ft contract --new

all: contracts
INTERFACES=I*.sol

clean:
%crm -f *~ *.code
|} tab
    in
    let content =
      Printf.sprintf {|%s

contracts::%s.code

%s.code: %s.spp $(INTERFACES)
%cft contract build %s.spp -f
|} content
        name name name tab name
    in
    EzFile.write_file "Makefile" content;
    Printf.eprintf "File Makefile updated\n%!";

    if not ( Sys.file_exists ".gitignore" ) then begin
      EzFile.write_file ".gitignore"
        {|*~
*.code
|};
      Printf.eprintf "File .gitignore created\n%!";
    end

let cmd =
  let list = ref [] in
  EZCMD.sub
    "contract new"
    (fun () ->
       match !list with
       | [] ->
           Error.raise "Error: You must provide a contrat name"
       | list -> List.iter create_contract list
    )
    ~args:[
      [], Arg.Anons (fun l -> list := l),
      EZCMD.info ~docv:"CONTRACT" "Create contract file for CONTRACT" ;
    ]
    ~doc: "Generate a new contract source file"
    ~man:[]
