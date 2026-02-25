(* Autobahn-compatible WebSocket echo server. Listens on ws://127.0.0.1:9001 and
   echoes all data frames. Responds to Ping with Pong; Close is handled
   transparently by the library.

   To test with Autobahn: docker run --rm --network host \ -v
   $(pwd)/autobahn:/config \ crossbario/autobahn-testsuite \ wstest -m
   fuzzingclient -s /config/fuzzingclient.json *)

let handler conn =
  try
    while true do
      match Cows.recv conn with
      | Cows.Text msg -> Cows.send conn (Cows.Text msg)
      | Cows.Binary msg -> Cows.send conn (Cows.Binary msg)
      | Cows.Ping payload -> Cows.send conn (Cows.Pong payload)
      | Cows.Pong _ -> ()
    done
  with
  | Cows.Connection_closed _ -> ()
  | Cows.Protocol_error msg -> Printf.eprintf "protocol error: %s\n%!" msg
  | exn -> Printf.eprintf "unexpected: %s\n%!" (Printexc.to_string exn)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, 9001) in
  let socket = Eio.Net.listen ~reuse_addr:true ~backlog:256 ~sw env#net addr in
  Printf.printf "Listening on ws://127.0.0.1:9001\n%!";
  let server =
    Cohttp_eio.Server.make_response_action
      ~callback:(fun _conn req _body -> Cows.upgrade req handler)
      ()
  in
  Cohttp_eio.Server.run ~on_error:raise socket server
