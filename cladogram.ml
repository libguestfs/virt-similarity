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

open Utils

type t =
  | Leaf of int                  (* A single disk image (index of). *)
  | Node of t list * int         (* An interior node in the tree. *)

let rec images_in_subtree = function
  | Leaf i -> [i]
  | Node (xs, _) -> List.concat (List.map images_in_subtree xs)

let max_list = List.fold_left max min_int

let mapi f xs =
  let rec loop i = function
    | [] -> []
    | x :: xs -> let r = f i x in r :: loop (i+1) xs
  in
  loop 0 xs

(* Compute the minimum distance between subtrees t1 and t2.  'matrix'
 * is the distance matrix between leaves.
 *)
let min_distance_between_subtrees matrix t1 t2 =
  let min_distance = ref max_int in

  let xs = images_in_subtree t1 in
  let ys = images_in_subtree t2 in

  List.iter (
    fun (i, j) ->
      let d = matrix.(i).(j) in
      if d < !min_distance then min_distance := d
  ) (pairs xs ys);

  !min_distance

(* Find the closest subtrees and combine them. *)
let combine_closest_subtrees matrix trees =
  let trees = Array.of_list trees in
  let n = Array.length trees in

  (* Find the minimum distance between any two subtrees. *)
  let min_distance = ref max_int in
  List.iter (
    fun (i, j) ->
      let d = min_distance_between_subtrees matrix trees.(i) trees.(j) in
      if d < !min_distance then min_distance := d
  ) (pairs_of_ints n);

  let min_distance = !min_distance in

  (* For each subtree that differs from another by exactly the
   * minimum distance, group them together into a single subtree.
   *)
  let in_group = Array.make n false in
  List.iter (
    fun (i, j) ->
      let d = min_distance_between_subtrees matrix trees.(i) trees.(j) in
      if d = min_distance then (
        in_group.(i) <- true;
        in_group.(j) <- true
      )
  ) (pairs_of_ints n);

  let group = ref [] and rest = ref [] in
  Array.iteri (
    fun i in_group ->
      if in_group then
        group := trees.(i) :: !group
      else
        rest := trees.(i) :: !rest
  ) in_group;

  !rest @ [Node (!group, min_distance)]

let construct_cladogram matrix n =
  (* At the bottom level, every disk image is in its own leaf. *)
  let leaves =
    let rec loop i = if i < n then Leaf i :: loop (i+1) else [] in
    loop 0 in

  (* Work up the tree, combining subtrees together, until we end
   * up with one tree (ie. the top node of the final tree).
   *)
  let rec loop trees =
    match trees with
    | [] -> assert false
    | [x] -> x (* finished *)
    | xs -> loop (combine_closest_subtrees matrix xs)
  in
  loop leaves

let format_cladogram ?format_leaf t =
  let format_leaf = match format_leaf with
    | None -> string_of_int
    | Some f -> f
  in
  let rec format = function
    | Leaf i ->
      let s = "--- " ^ format_leaf i in
      [s; ""], String.length s
    | Node (xs, _) ->
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
