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

(** Handle the cache file. *)

type cache
(** The immutable cache, which is a map of filename to {!hashes}. *)

type hashes = string array
(** An array of block hashes.  One array for each disk image. *)

type filename = string
(** A filename. *)

val read_cache_file : unit -> cache
(** Read the cache file. *)

val write_cache_file : cache -> unit
(** Write the cache file. *)

val get_hash : cache -> filename -> hashes option
(** Get the hash for a particular filename from the cache. *)

val update_hash : cache -> filename -> hashes -> cache
(** Set or update the hashes corresponding to filename in the cache.
    Note that since {!cache} is immutable, this returns a new cache
    object. *)
