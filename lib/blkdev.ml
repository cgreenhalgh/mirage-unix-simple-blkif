(* 
 * Copyright (c) 2013, The University of Nottingham
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 * 
 *   Neither the name of the University of Nottingham nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(** Mirage blkif implemented over a Unix file *)
open Lwt
open OS
open Printf

exception Error of string

(** internal state *)
type t = {
  id: string;
  fd: Lwt_unix.file_descr;
  size: int
}
           
let read_512 t offset length =
  let finished = ref false in
  Lwt_stream.from
    (fun () ->
      match !finished with
	| false ->
	  finished := true;
	  let size = Int64.to_int(length)*512 in
	  let off = Int64.to_int(offset)*512 in
	  let buf = String.create size in
	  (* TODO serialise *)
	  lwt _ = Lwt_unix.lseek t.fd off Lwt_unix.SEEK_SET in
	  lwt cnt = Lwt_unix.read t.fd buf 0 size in
	  if (cnt<=0) then return None
	  else if (cnt==size) then
		return (Some (Cstruct.of_string buf))
 	  else 
	    let cs = Cstruct.create cnt in
	    Cstruct.blit_from_string buf 0 cs 0 cnt;
            return (Some cs)
	| true -> return None
    )

let write_page t offset page =
  let len = Io_page.length page in
  (* TODO serialise *)
  lwt _ = Lwt_unix.lseek t.fd (Int64.to_int offset) Lwt_unix.SEEK_SET in
  let bs = Io_page.to_string page in
  Lwt_unix.write t.fd bs 0 len >> 
  return ()

let destroy t = 
        let dclose() = 
                printf "close blkdev %s\n%!" t.id;
                Lwt_unix.close t.fd
        in async dclose
        
(** from filename/path *)
(* var create : ?readwrite:bool -> id:string -> filename:string ->
        * OS.Devices.blkif Lwt.t *)
let create ?(readwrite=true) id filename : OS.Devices.blkif Lwt.t =
  printf "create blkdev %s file %s\n%!" id filename;
  let mode = if readwrite then Lwt_unix.O_RDWR else Lwt_unix.O_RDONLY in
  lwt fd = Lwt_unix.openfile filename [ mode ] 0o666 in
  if Lwt_unix.readable fd == false then 
          fail (Error (sprintf "File %s unreadable" filename)) 
  else return () >>
  lwt { Lwt_unix.st_size = size; _ } = Lwt_unix.fstat fd in
  let t = { id; fd; size } in
  return (object 
          method id = id
          method read_512 = read_512 t
          method write_page = write_page t
          method sector_size = 4096
          method size = Int64.of_int size
          method readwrite = readwrite
          method ppname = sprintf "Blkdev:%s(%s)" id filename
          method destroy = destroy t
  end)

(** will call OS.Devices.new_provider and plug *) 
(*let add_provider : OS.Devices.blkif -> unit Lwt.t*)
