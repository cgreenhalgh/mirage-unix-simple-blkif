(* 
 * by Chris Greenhalgh <chris.greenhalgh@nottingham.ac.uk>
 *
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
open Printf

exception Error of string

type id = string

(* The semantics should be something like ocaml-xen-block-driver.
 * The low-level read/write should not block the VM (i.e. lwt threads) 
 * - on xen it would
 * be the backend/driver in dom0 doing this; with lwt_unix it should
 * be a job running in a background pthread, which should be ok.
 * In blkfront write_page writes a single page; there is internal
 * wait for page grants then the request is queued in the ring.
 * In read_512 the read is broken down into a sequence of no-more-than
 * 11 4096byte pages (88 512byte sectors) reads, which are queued in
 * the ring as they are requested from the returned Lwt_sequence. 
 *
 * So with concurrent read/write requests arbitrary interleaving can
 * occur at the level of written page and read <=11-page block. 
 * 
 * Note: currently this implementation does NOT sub-divide reads or 
 * writes.
 *
 * Note: currently this implementation will write an IO_page.t which is
 * more than one page (unlike current xen-block-driver).
 *)

(* request *)
type rrequest = {
  roffset : int;
  rlength : int;
  rdone : Cstruct.t option Lwt.u
}
type wrequest = {
  woffset : int;
  wpage : OS.Io_page.t;
  wdone : unit Lwt.u
}
type request = Read of rrequest | Write of wrequest | Destroy 

(** internal state *)
type t = {
  id: string;
  fd: Lwt_unix.file_descr;
  size: int;
  readwrite : bool;
  requests : request Lwt_mvar.t;
}

(* worker thread - handles requests in order *)
let rec handle_request t =
  lwt req = Lwt_mvar.take t.requests in 
  lwt ok = match req with
  Destroy -> 
    printf "close blkdev %s\n%!" t.id;
    Lwt_unix.close t.fd >>
    return true
  | Read { roffset; rlength; rdone } -> 
    let buf = String.create rlength in
    lwt _ = Lwt_unix.lseek t.fd roffset Lwt_unix.SEEK_SET in
    lwt cnt = Lwt_unix.read t.fd buf 0 rlength in
    let rval = 
      if (cnt<=0) then None
      else if (cnt==rlength) then
        Some (Cstruct.of_string buf)
      else 
	let cs = Cstruct.create cnt in
	Cstruct.blit_from_string buf 0 cs 0 cnt;
        Some cs
    in 
    Lwt.wakeup rdone rval; 
    return false
  | Write { woffset; wpage; wdone } -> 
    lwt _ = Lwt_unix.lseek t.fd woffset Lwt_unix.SEEK_SET in
    let wlength = OS.Io_page.length wpage in
    let bs = OS.Io_page.to_string wpage in
    lwt _ = Lwt_unix.write t.fd bs 0 wlength in 
    Lwt.wakeup wdone ();
    return false
  in if ok then return () else handle_request t

let read_512 t offset length =
  let finished = ref false in
  Lwt_stream.from
    (fun () ->
      match !finished with
	| false ->
	  finished := true;
          (* Note: complete read is one request; 
             Would be closer to xen if limited to 11 blocks/read *)
	  let rlength = Int64.to_int(length)*512 in
	  let roffset = Int64.to_int(offset)*512 in
          let th,rdone = Lwt.task () in
          let req = Read { roffset; rlength; rdone } in
          lwt _ = Lwt_mvar.put t.requests req in
          th
	| true -> return None
    )

let write_page t offset page =
  lwt _ = if t.readwrite then return ()
  else fail(Error("write_page on readonly Blkdev")) in
  let woffset = Int64.to_int offset in
  let th,wdone = Lwt.task () in
  (* Note: page can be multiple pages; handled in one write;
     Would be closer to xen if done one by one (or fail if >1!) *)
  let req = Write { woffset; wpage=page; wdone } in
  lwt _ = Lwt_mvar.put t.requests req in
  th

let destroy t = 
        let dclose() = Lwt_mvar.put t.requests Destroy
        in async dclose
        
(** from filename/path *)
(* var create : ?readwrite:bool -> id:string -> filename:string ->
        * OS.Devices.blkif Lwt.t *)
let create ?(readwrite=true) (id:string) (filename:string) : OS.Devices.blkif Lwt.t =
  printf "create blkdev %s file %s\n%!" id filename;
  (* unix file open *)
  let mode = if readwrite then Lwt_unix.O_RDWR else Lwt_unix.O_RDONLY in
  lwt fd = Lwt_unix.openfile filename [ mode ] 0o666 in
  if Lwt_unix.readable fd == false then 
          fail (Error (sprintf "File %s unreadable" filename)) 
  else return () >>
  (* internal state *)
  lwt { Lwt_unix.st_size = size; _ } = Lwt_unix.fstat fd in
  let requests = Lwt_mvar.create_empty () in
  let t = { id; fd; size; readwrite; requests } in
  (* worker thread *)
  Lwt.async (function () -> handle_request t);
  (* blk device *)
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
(*var add_provider : id:string -> filename:string -> unit Lwt.t *)
let add_provider id filename =
  let plug_mvar = Lwt_mvar.create_empty () in
  let unplug_mvar = Lwt_mvar.create_empty () in
  let provider = (object(self)
	method create ~deps ~cfg id = 
	  lwt blkif = create id filename in
	  let entry = OS.Devices.({provider=self; id=self#id; depends=[]; node=OS.Devices.Blkif blkif}) in
	  return entry
 	method id = sprintf "Unix-simple-blkdev:%s(%s)" id filename
	method plug = plug_mvar
	method unplug = unplug_mvar
  end) in
  OS.Devices.new_provider provider;
  Lwt_mvar.put plug_mvar OS.Devices.({p_id=id;p_dep_ids=[];p_cfg=[]}) >>
  return ()

