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

val cmd : Ezcmd.V2.EZCMD.TYPES.sub

val send_transfer :
  account:string ->
  ?src:string ->
  dst:string ->
  amount:string ->
  ?bounce:bool -> ?args:string list -> ?wait:bool ->
  ?send:bool -> unit -> unit
