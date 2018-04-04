(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Hiredis
open Cmdliner

module Data = struct
  type t = Hiredis.Pool.t
  type client = unit
  let new_client () = ()
end

module Server = Resp_server.Make(Resp_server.Auth.String)(Data)

let default srv cli cmd args =
  Pool.use srv (fun r ->
    Lwt.wrap (fun () ->
      Client.run_v r (Array.append [| String cmd |] args))
    >>= Lwt.return_some)

let main addr port redis_addr redis_port tls_key tls_cert connections =
  let pool = Pool.create ~port:redis_port redis_addr connections in
  Server.create ~default ~host:addr (`TCP (`Port port)) pool >>=
  Server.run

let server addr port redis_addr redis_port tls_key tls_cert connections =
  if tls_key = "" then
    print_endline "No TLS key configured"
  else if tls_cert = "" then
    print_endline "No TLS certificate configured"
  else
    Lwt_main.run (main addr port redis_addr redis_port tls_key tls_cert connections)

let addr =
  let doc = "Address to listen on" in
  Arg.(value & opt string "127.0.0.1" & info ["a"; "address"] ~doc ~docv:"HOST")

let port =
  let doc = "Port to listen on" in
  Arg.(value & opt int 6380 & info ["p"; "port"] ~doc ~docv:"PORT")

let redis_addr =
  let doc = "Address Redis is listening on" in
  Arg.(value & opt string "127.0.0.1" & info ["r"; "redis-address"] ~doc ~docv:"HOST")

let redis_port =
  let doc = "Port Redis is listening on" in
  Arg.(value & opt int 6379 & info ["q"; "redis-port"] ~doc ~docv:"PORT")

let tls_key =
  let doc = "TLS key" in
  Arg.(value & opt string "" & info ["k"; "key"] ~doc ~docv:"KEY")

let tls_cert =
  let doc = "TLS certificate" in
  Arg.(value & opt string "" & info ["c"; "cert"] ~doc ~docv:"CERT")

let connections =
  let doc = "Total number of proxy connections" in
  Arg.(value & opt int 8 & info ["n"; "connections"] ~doc ~docv:"CONN")

let server_t = Term.(const server $ addr $ port $ redis_addr $ redis_port $ tls_key $ tls_cert $ connections)
let cmd = Term.info "redis-tls" ~version:"v0.1" ~exits:Term.default_exits
let () = Term.exit @@ Term.eval (server_t, cmd)

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
