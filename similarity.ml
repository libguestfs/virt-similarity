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

open Cladogram
open Utils

let blocksize = 65536
module HashAlgorithm = Hash.MD5
let hash_bytes_per_block = HashAlgorithm.hash_bytes_per_block

(* Int64 operators. *)
let (+^)   = Int64.add
let (-^)   = Int64.sub
let ( *^ ) = Int64.mul
let (/^)   = Int64.div

(* Read command line parameters. *)
let n, filenames, debug =
  let display_version () =
    printf "%s %s (%s)\n"
      Config.package_name Config.package_version
      (if Config.ocamlopt then "native" else "bytecode");
    exit 0
  in

  let debug = ref false in

  let argspec = Arg.align [
    "--debug", Arg.Set debug, " Enable debugging";
    "-d", Arg.Set debug, " Enable debugging";
    "--version", Arg.Unit display_version, " Display version number and exit";
    "-V", Arg.Unit display_version, " Display version number and exit";
  ] in

  let filenames = ref [] in
  let collect_filenames str = filenames := str :: !filenames in

  let usage_msg = "
virt-similarity: Find clusters of similar/cloned virtual machines
Copyright (C) 2013 Red Hat Inc.

For full documentation see the virt-similarity(1) man page.

Usage:

  virt-similarity [options] disk.img disk.img [disk.img ...]

You must supply at least one disk image.  You can supply disk
images in most common formats (raw, qcow2, vmdk, etc.)

Options:
" in

  (* Do the argument parsing. *)
  Arg.parse argspec collect_filenames usage_msg;

  (* Check the arguments. *)
  let filenames = Array.of_list (List.rev !filenames) in
  let n = Array.length filenames in
  if n < 2 then (
    eprintf "virt-similarity: At least two disk images must be specified.\n";
    exit 1
  );

  let debug = !debug in

  n, filenames, debug

(* Read in the cache file. *)
let cache = Cache.read_cache_file ()

(* Read the disk images, hash them, and update the cache. *)
let read_disk_image filename =
  let g = new Guestfs.guestfs () in
  g#add_drive_ro filename;
  g#launch ();

  let dev = "/dev/sda" in
  let size = g#blockdev_getsize64 dev in
  let rec loop offset last_percent accum =
    if offset <= size -^ Int64.of_int blocksize then (
      let percent = 100L *^ offset /^ size in
      if percent <> last_percent then printf "%Ld%%\r%!" percent;

      let block = g#pread_device dev blocksize offset in
      let hash = HashAlgorithm.digest block in
      loop (offset +^ Int64.of_int blocksize) percent (hash :: accum)
    )
    else accum
  in
  let hashes_reversed = loop 0L (-1L) [] in
  g#close ();

  Array.of_list (List.rev hashes_reversed)

let cache =
  List.fold_left (
    fun cache filename ->
      let cache, hashes =
        match Cache.get_hash cache filename with
        | Some hashes ->
          if debug then
            printf "%s: disk image is already in the cache\n%!" filename;
          cache, hashes
        | None ->
          printf "%s: reading disk image ...\n%!" filename;
          let hashes = read_disk_image filename in
          Cache.update_hash cache filename hashes, hashes in
      if debug then
        printf "%s: number of blocks = %d\n" filename (Array.length hashes);
      cache
  ) cache (Array.to_list filenames)

(* Write the updated cache file. *)
let () = Cache.write_cache_file cache

(* Work out the similarity for each pair of guests and store it in a
 * matrix, where matrix.(i).(j) is the distance between filenames.(i)
 * and filenames.(j).
 *)
let hash_of_zero =
  let zero_string = String.make blocksize (Char.chr 0) in
  HashAlgorithm.digest zero_string

let calculate_distance hash1 hash2 =
  let hash1 = Array.to_list hash1 in
  let hash2 = Array.to_list hash2 in
  let rec loop = function
    | [], [] -> 0
    | (x :: xs), [] when x = hash_of_zero -> loop (xs, [])
    | (x :: xs), [] -> 1 + loop (xs, [])
    | [], (y :: ys) when y = hash_of_zero -> loop ([], ys)
    | [], (y :: ys) -> 1 + loop ([], ys)
    | (x :: xs), (y :: ys) when x = y -> loop (xs, ys)
    | (x :: xs), (y :: ys) -> 1 + loop (xs, ys)
  in
  loop (hash1, hash2)

let matrix =
  let matrix = Array.make_matrix n n 0 in
  List.iter (
    fun (i, j) ->
      let hi = Cache.get_hash cache filenames.(i) in
      let hj = Cache.get_hash cache filenames.(j) in
      match hi, hj with
      | Some hi, Some hj ->
        let d = calculate_distance hi hj in
        if debug then
          printf "distance from %s to %s = %d\n" filenames.(i) filenames.(j) d;
        matrix.(i).(j) <- d;
        matrix.(j).(i) <- d
      | _ -> assert false
  ) (pairs_of_ints n);
  matrix

(* Construct the tree (cladogram). *)
let cladogram = construct_cladogram matrix n

let () =
  let format_leaf i = Filename.basename filenames.(i) in
  let lines = format_cladogram ~format_leaf cladogram in
  List.iter print_endline lines
