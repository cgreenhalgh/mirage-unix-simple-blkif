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

(**  tests for Mirage blkif implemented over a Unix file *)

open OUnit
open Lwt

exception Prerequisite of string

let create () =
    let id = "testid" in
    lwt blk = Blkdev.create "testid" "testfile" in
    assert_equal blk#id id; 
    return blk

let test_create() = Lwt_main.run ( 
        lwt blk = create () in blk#destroy; return ()
)

let test_size() = 
        let fn() = 
                lwt blk = create() in
                Printf.printf "Note: test assumes testfile is 1M\n%!";
                assert_equal blk#size (Int64.mul 1024L 1024L);
                assert_equal blk#sector_size 4096;
                blk#destroy;
                return () in
        Lwt_main.run(fn())

let test_rw1() = 
        let fn() =
                (* create a blkdev *)
                lwt blk = create() in
                (* check some useful sizes *)
                let size = blk#size in
                let block_size = blk#sector_size in
                let blocks = Int64.to_int (Int64.div size (Int64.of_int
                block_size)) in
                let page_size = OS.Io_page.round_to_page_size 1 in
                let pages_per_block = (page_size+block_size-1)/block_size in
                (* write every block in the device with block number *)
                let write_block ix =
                        let page = OS.Io_page.get pages_per_block in
                        let pcs = OS.Io_page.to_cstruct page in
                        let b = ix mod 256 in
                        for i = 0 to block_size-1 do
                                Cstruct.set_uint8 pcs i b 
                        done;
                        let offset = Int64.(mul (of_int block_size) (of_int ix)) in
                        blk#write_page offset page
                in
                let read_and_check_block ix = 
                        let read512s_per_block = block_size / 512 in
                        let offset512 = Int64.of_int (ix*read512s_per_block) in
                        let str = blk#read_512 offset512 (Int64.of_int
                        read512s_per_block) in
                        let b = ix mod 256 in
                        let check_cs cs = 
                                let size = Cstruct.len cs in
                                let rec check_b i =
                                        if i>=size then true
                                        else if (Cstruct.get_uint8 cs i)!=b then
                                                false
                                        else check_b (i+1) in
                                check_b 0
                        in
                        let rec check_css str =
                                lwt hd = Lwt_stream.get str in
                                match hd with
                                | None -> return true
                                | Some cs -> 
                                        if (check_cs cs)==false then
                                                return false
                                        else 
                                                check_css str
                        in
                        lwt ok = check_css str in
                        assert_equal ok true; return ()

                in
                let rec write_blocks ix n = 
                        write_block ix >>
                        read_and_check_block ix >>
                        if n<=0 then return () else
                                write_blocks (ix+1) (n-1)
                in
                write_blocks 0 blocks >>
                (* tidy up *)
                (blk#destroy; return ())
        in
        Lwt_main.run(fn())

(* we don't really have a suitable test harness for the Mirage provider
   as we can only call OS.Main.run once *)
let test_provider() =
  let id = "testid" in
  let main wake =
    lwt blkif = OS.Devices.find_blkif id in
    Lwt.wakeup wake blkif;
    return ()
  in let test () =
    let th,wake = Lwt.task () in
    lwt _ = Blkdev.add_provider id "testfile" in
    OS.Main.run (main wake);
    lwt blkif = th in
    match blkif with
    | None -> fail(Blkdev.Error("Blkif not found"))
    | Some blkif -> 
      Printf.printf "Got blkif %s\n%!" id;
      return ()
  in
  Lwt_main.run(test())

let _ =
    let verbose = ref false in
      Arg.parse [
                  "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
      "Test blkdev code";
  let suite = "blkdev" >:::
    [
            "create" >:: test_create;
            "size" >:: test_size;
            "rw1" >:: test_rw1;
            "provider" >:: test_provider
        
    ] in
  run_test_tt ~verbose:!verbose suite
