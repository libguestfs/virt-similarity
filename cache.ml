(* virt-similarity
 * Copyright (C) 2013 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

open Unix
open Filename

type filename = string
type hashes = string array
type cache = (string * hashes) list

let (//) = Filename.concat

let version = "1"

let cache_file =
  try Some (Sys.getenv "HOME" // ".similarity-cache.v" ^ version)
  with Not_found -> None

let read_cache_file () : cache =
  match cache_file with
  | None -> []
  | Some cache_file ->
    try
      let chan = open_in cache_file in
      let v = input_value chan in
      close_in chan;
      v
    with
      _ -> []

let write_cache_file (cache : cache) =
  match cache_file with
  | None -> ()
  | Some cache_file ->
    let tmp_name = cache_file ^ ".tmp" in
    let chan = open_out tmp_name in
    output_value chan cache;
    close_out chan;
    rename tmp_name cache_file

let get_hash cache filename =
  try Some (List.assoc filename cache) with Not_found -> None

let update_hash cache filename hashes =
  let cache = List.remove_assoc filename cache in
  (filename, hashes) :: cache
