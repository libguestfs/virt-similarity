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

open Printf

type t =
  | Leaf of int                  (* A single disk image (index of). *)
  | Node of t list               (* An interior node in the tree. *)

let rec images_in_subtree = function
  | Leaf i -> [i]
  | Node xs -> List.concat (List.map images_in_subtree xs)

let max_list = List.fold_left max min_int

let mapi f xs =
  let rec loop i = function
    | [] -> []
    | x :: xs -> let r = f i x in r :: loop (i+1) xs
  in
  loop 0 xs

let format_cladogram ?format_leaf t =
  let format_leaf = match format_leaf with
    | None -> string_of_int
    | Some f -> f
  in
  let rec format = function
    | Leaf i ->
      let s = "--- " ^ format_leaf i in
      [s; ""], String.length s
    | Node xs ->
      let xs = List.map format xs in
      let n = List.length xs in
      let w = 7 + max_list (List.map snd xs) in
      let xs = mapi (
        fun row (ss, _) ->
          let s, ss = match ss with
            | s :: ss -> s, ss
            | [] -> assert false in
          if row = 0 then (
            ("---+---" ^ s) ::
              List.map (fun s -> "   |   " ^ s) ss
          ) else if row < n-1 then (
            ("   +---" ^ s) ::
              List.map (fun s -> "   |   " ^ s) ss
          ) else (
            ("   +---" ^ s) ::
              List.map (fun s -> "       " ^ s) ss
          )
      ) xs in
      List.concat xs, w
  in
  let strs, _ = format t in
  strs
